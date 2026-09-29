#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE, warn = 1)

############################################################
# UNODC Prisons and Prisoners — estudo Bayesiano com Stan
#
# Execução padrão (rápida): recompõe os resultados finais
# a partir dos artefatos auditados versionados no repositório.
#
#   Rscript scripts/prisons_prisoners_unodc/prisons_prisoners_unodc_cmdstanr.R
#
# Reamostragem dos três modelos com as especificações finais
# efetivamente utilizadas no estudo:
#
#   Rscript scripts/prisons_prisoners_unodc/prisons_prisoners_unodc_cmdstanr.R --run_stan=1
#
# Saídas: outputs/prisons_prisoners_unodc/
############################################################

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^--", name, "="), "", hit[[1]])
}

as_flag <- function(x, default = FALSE) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(default)
  tolower(x) %in% c("1", "true", "t", "yes", "y", "sim", "s")
}

script_path <- function() {
  cmd <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", cmd, value = TRUE)
  if (!length(hit)) return(NA_character_)
  normalizePath(sub("^--file=", "", hit[[1]]), mustWork = FALSE)
}

has_repo_markers <- function(path) {
  file.exists(file.path(path, "README.md")) &&
    dir.exists(file.path(path, "scripts")) &&
    dir.exists(file.path(path, "data", "raw"))
}

find_repo_root <- function(start) {
  if (is.na(start) || !nzchar(start)) return(NA_character_)
  cur <- normalizePath(start, mustWork = FALSE)
  if (file.exists(cur) && !dir.exists(cur)) cur <- dirname(cur)
  repeat {
    if (has_repo_markers(cur)) return(cur)
    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }
  NA_character_
}

repo_root <- NA_character_
for (candidate in unique(c(arg_value("root", NA_character_), getwd(), dirname(script_path())))) {
  found <- find_repo_root(candidate)
  if (!is.na(found)) {
    repo_root <- found
    break
  }
}
if (is.na(repo_root)) {
  stop("Não foi possível localizar a raiz do repositório. Rode a partir da raiz ou use --root=/caminho/do/repo.", call. = FALSE)
}

study_id <- "prisons_prisoners_unodc"
data_dir <- file.path(repo_root, "data", "raw", study_id)
input_dir <- file.path(data_dir, "model_inputs")
audited_dir <- file.path(data_dir, "resultados_auditados")
stan_dir <- file.path(repo_root, "scripts", study_id)
out_dir <- file.path(repo_root, "outputs", study_id)
out_tables <- file.path(out_dir, "tables")
out_logs <- file.path(out_dir, "logs")
out_cmdstan <- file.path(out_dir, "cmdstan_csv")
for (d in c(out_tables, out_logs, out_cmdstan)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

run_stan <- as_flag(arg_value("run_stan", Sys.getenv("RUN_STAN", "false")), FALSE)

require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Pacote requerido não instalado: ", pkg, call. = FALSE)
  }
}

write_csv_utf8 <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

log_file <- file.path(out_logs, "prisons_prisoners_unodc_execucao.log")
log_msg <- function(...) {
  txt <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " - ", paste(..., collapse = ""))
  cat(txt, "\n")
  cat(txt, "\n", file = log_file, append = TRUE)
}

first_num <- function(df, row_col, row_val, value_col) {
  z <- df[df[[row_col]] == row_val, value_col]
  if (length(z) != 1L || !is.finite(as.numeric(z))) {
    stop("Evidência ausente ou ambígua: ", row_val, " / ", value_col, call. = FALSE)
  }
  as.numeric(z)
}

# ---------------------------------------------------------------------------
# Modo padrão: reprodução documental dos resultados finais auditados
# ---------------------------------------------------------------------------
run_audit <- function() {
  needed <- c(
    "E09_quantidades_derivadas_M1.csv",
    "E09_tabela_rastreabilidade.csv",
    "PPC_global_M2.csv",
    "PPC_global_M3.csv",
    "sensibilidade_painel_completo_M3_sex_diff.csv",
    "PSIS_LOO_resumo_M1_M2_M3.csv"
  )
  missing <- needed[!file.exists(file.path(audited_dir, needed))]
  if (length(missing)) stop("Artefatos auditados ausentes: ", paste(missing, collapse = ", "), call. = FALSE)

  m1 <- read.csv(file.path(audited_dir, "E09_quantidades_derivadas_M1.csv"), check.names = FALSE, fileEncoding = "UTF-8")
  rast <- read.csv(file.path(audited_dir, "E09_tabela_rastreabilidade.csv"), check.names = FALSE, fileEncoding = "UTF-8")
  ppc2 <- read.csv(file.path(audited_dir, "PPC_global_M2.csv"), check.names = FALSE)
  ppc3 <- read.csv(file.path(audited_dir, "PPC_global_M3.csv"), check.names = FALSE)
  sex <- read.csv(file.path(audited_dir, "sensibilidade_painel_completo_M3_sex_diff.csv"), check.names = FALSE)
  loo <- read.csv(file.path(audited_dir, "PSIS_LOO_resumo_M1_M2_M3.csv"), check.names = FALSE)

  m1_factor <- first_num(m1, "quantidade", "M1_country_factor_1sd", "transform_mean")
  m2_mean <- first_num(ppc2, "stat", "mean", "observed")
  m2_median <- first_num(ppc2, "stat", "median", "observed")
  m2_median_rep <- first_num(ppc2, "stat", "median", "rep_median")
  m3_full <- first_num(sex, "subset", "full", "observed_mean_diff")
  m3_complete <- first_num(sex, "subset", "complete_panel", "observed_mean_diff")

  k1 <- loo$n_k_gt_1[loo$model == "M1" & loo$level == "country"]
  k2 <- loo$n_k_gt_1[loo$model == "M2" & loo$level == "country"]
  k3 <- loo$n_k_gt_1[loo$model == "M3" & loo$level == "country"]
  if (length(k1) != 1L || length(k2) != 1L || length(k3) != 1L) stop("Resumo LOO por país incompleto.", call. = FALSE)

  # Gates apenas para detectar troca/corrupção de artefatos.
  stopifnot(
    abs(m1_factor - 1.93945) < 0.02,
    abs(m2_mean - 0.32116) < 0.002,
    abs(m2_median - 0.27760) < 0.002,
    abs(m3_full - 0.03718) < 0.002,
    abs(m3_complete - 0.01793) < 0.002
  )

  final <- data.frame(
    resultado = c(
      "M1_fator_pais_1dp", "M2_media_observada", "M2_mediana_observada",
      "M2_mediana_replicada", "M3_diff_FM_full", "M3_diff_FM_complete_panel",
      "LOO_pais_k_gt_1_M1", "LOO_pais_k_gt_1_M2", "LOO_pais_k_gt_1_M3"
    ),
    valor = c(m1_factor, m2_mean, m2_median, m2_median_rep, m3_full, m3_complete, k1, k2, k3),
    unidade = c(
      "fator multiplicativo", "proporção", "proporção", "proporção",
      "proporção", "proporção", "países", "países", "países"
    ),
    status = c(
      "auditado", "auditado", "auditado_com_ressalva_modelo", "auditado_com_ressalva_modelo",
      "auditado_associativo", "auditado_associativo_sensibilidade",
      "diagnóstico_restritivo", "diagnóstico_restritivo", "diagnóstico_restritivo"
    )
  )

  write_csv_utf8(final, file.path(out_tables, "resultados_finais_auditados.csv"))
  writeLines(c(
    "Reprodução documental: OK",
    paste("Linhas da tabela de rastreabilidade =", nrow(rast)),
    paste("M1 fator por 1 DP de país =", format(m1_factor, digits = 6)),
    paste("M2 média observada =", format(m2_mean, digits = 6)),
    paste("M2 mediana observada/replicada =", format(m2_median, digits = 6), "/", format(m2_median_rep, digits = 6)),
    paste("M3 Female-Male full/complete =", format(m3_full, digits = 6), "/", format(m3_complete, digits = 6)),
    paste("Países com Pareto-k > 1 (M1/M2/M3) =", k1, "/", k2, "/", k3),
    "PSIS-LOO por país não validou generalização para países novos.",
    "As conclusões são descritivas/associativas; não há estimando causal identificado."
  ), file.path(out_logs, "VALIDACAO_AUDIT.txt"))

  log_msg("Modo audit concluído.")
  invisible(final)
}

# ---------------------------------------------------------------------------
# Reamostragem: especificações finais executadas no estudo
# M1 = REV5; M2/M3 = REV4.1 preservados.
# ---------------------------------------------------------------------------
run_refit <- function() {
  for (pkg in c("cmdstanr", "posterior", "jsonlite")) require_pkg(pkg)

  if (is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
    stop("CmdStan não configurado. Rode: Rscript scripts/_setup/install_cmdstan.R", call. = FALSE)
  }

  paths <- list(
    M1 = file.path(input_dir, "stan_data_M1.json"),
    M2 = file.path(input_dir, "stan_data_M2.json"),
    M3 = file.path(input_dir, "stan_data_M3.json")
  )
  missing <- names(paths)[!file.exists(unlist(paths))]
  if (length(missing)) stop("Dados Stan ausentes: ", paste(missing, collapse = ", "), call. = FALSE)

  read_json <- function(path) {
    jsonlite::fromJSON(path, simplifyVector = TRUE)
  }

  d1_old <- read_json(paths$M1)
  if (is.null(d1_old$z) || length(d1_old$z) != d1_old$N) stop("stan_data_M1.json incompatível.", call. = FALSE)
  d1 <- d1_old
  d1$held_rate <- expm1(d1$z)
  d1$z <- NULL
  d1$vat_id <- 212L
  d1$n_vat <- sum(d1$country == d1$vat_id)
  d1$n_nonvat <- sum(d1$country != d1$vat_id)
  d1$prior_only <- 0L

  d2 <- read_json(paths$M2)
  d3 <- read_json(paths$M3)
  d2$prior_only <- 0L
  d3$prior_only <- 0L

  stopifnot(d1$N == 2878L, d1$J == 224L, d1$R == 5L, d1$T == 18L)
  stopifnot(d2$N == 2162L, d2$J == 193L, d2$R == 5L, d2$T == 18L)
  stopifnot(d3$N == 1144L, d3$P == 572L, d3$J == 106L, d3$R == 5L, d3$T == 7L)

  model_paths <- c(
    M1 = file.path(stan_dir, "modelo_M1_held_rate_hurdle_REV5.stan"),
    M2 = file.path(stan_dir, "modelo_M2_unsentenced_total.stan"),
    M3 = file.path(stan_dir, "modelo_M3_unsentenced_sex.stan")
  )
  if (any(!file.exists(model_paths))) stop("Modelo Stan ausente.", call. = FALSE)

  log_msg("Compilando modelos em modo pedantic.")
  mods <- lapply(model_paths, function(p) cmdstanr::cmdstan_model(p, pedantic = TRUE, force_recompile = TRUE))

  make_init_M1 <- function(chain_id) {
    set.seed(20260925L + 10000L + chain_id)
    list(
      alpha = log(150) + rnorm(1L, 0, 0.02),
      sigma_country = 0.60,
      sigma_global_rw = 0.015,
      sigma_region_rw = 0.020,
      sigma_obs = 0.13,
      nu_minus_two = 0.50,
      p_zero = c(0.0002, 0.75)
    )
  }

  cfg <- list(
    M1 = list(seed = 20260925L + 101L, warmup = 2500L, sampling = 3000L, init = make_init_M1),
    M2 = list(seed = 20260918L + 102L, warmup = 3000L, sampling = 4000L, init = 0.1),
    M3 = list(seed = 20260918L + 103L, warmup = 2000L, sampling = 2000L, init = 0.1)
  )
  data_list <- list(M1 = d1, M2 = d2, M3 = d3)

  fits <- list()
  for (nm in names(mods)) {
    od <- file.path(out_cmdstan, nm)
    dir.create(od, recursive = TRUE, showWarnings = FALSE)
    if (length(list.files(od, pattern = "[.]csv$", full.names = TRUE))) {
      stop("CSVs anteriores encontrados em ", od, ". Limpe a pasta para evitar mistura de execuções.", call. = FALSE)
    }
    cc <- cfg[[nm]]
    log_msg("Amostrando ", nm, ".")
    fits[[nm]] <- mods[[nm]]$sample(
      data = data_list[[nm]],
      seed = cc$seed,
      chains = 4,
      parallel_chains = 4,
      iter_warmup = cc$warmup,
      iter_sampling = cc$sampling,
      adapt_delta = 0.95,
      max_treedepth = 12,
      init = cc$init,
      output_dir = od,
      refresh = 100
    )
    write_csv_utf8(fits[[nm]]$summary(), file.path(out_tables, paste0("resumo_posterior_", nm, ".csv")))
  }

  sampler_diag <- do.call(rbind, lapply(names(fits), function(nm) {
    sd <- fits[[nm]]$sampler_diagnostics(format = "draws_array")
    vars <- dimnames(sd)$variable
    energy <- vars[grepl("^energy__$", vars)][1]
    divergent <- vars[grepl("^divergent__$", vars)][1]
    treedepth <- vars[grepl("^treedepth__$", vars)][1]
    do.call(rbind, lapply(seq_len(dim(sd)[2]), function(ch) {
      e <- sd[, ch, energy]
      data.frame(
        modelo = nm,
        cadeia = ch,
        divergencias = sum(sd[, ch, divergent]),
        treedepth_max = max(sd[, ch, treedepth]),
        saturacoes_treedepth_12 = sum(sd[, ch, treedepth] >= 12),
        e_bfmi = mean(diff(e)^2) / stats::var(e)
      )
    }))
  }))
  write_csv_utf8(sampler_diag, file.path(out_tables, "sampler_diagnostics_refit.csv"))

  # Quantidades centrais para conferência substantiva.
  s1 <- fits$M1$summary("sigma_country")
  m1_factor <- exp(s1$mean[1])

  yrep2 <- as.matrix(fits$M2$draws("y_rep", format = "matrix"))
  m2_mean_obs <- mean(d2$y)
  m2_median_obs <- median(d2$y)
  m2_mean_rep <- median(rowMeans(yrep2))
  m2_median_rep <- median(apply(yrep2, 1, median))

  yrep3 <- as.matrix(fits$M3$draws("y_rep", format = "matrix"))
  pairs <- seq_len(d3$P)
  female_idx <- vapply(pairs, function(p) which(d3$pair_id == p & d3$female == 1L)[1], integer(1))
  male_idx <- vapply(pairs, function(p) which(d3$pair_id == p & d3$female == 0L)[1], integer(1))
  obs_diff <- d3$y[female_idx] - d3$y[male_idx]
  rep_diff <- yrep3[, female_idx, drop = FALSE] - yrep3[, male_idx, drop = FALSE]
  full_rep <- rowMeans(rep_diff)

  country_years <- split(d3$pair_year, d3$pair_country)
  complete_country <- as.integer(names(Filter(function(x) length(unique(x)) == d3$T, country_years)))
  complete_pairs <- which(d3$pair_country %in% complete_country)
  complete_rep <- rowMeans(rep_diff[, complete_pairs, drop = FALSE])

  refit <- data.frame(
    resultado = c(
      "M1_fator_pais_1dp",
      "M2_media_observada", "M2_media_replicada_mediana",
      "M2_mediana_observada", "M2_mediana_replicada_mediana",
      "M3_diff_FM_full_observada", "M3_diff_FM_full_replicada_mediana",
      "M3_diff_FM_complete_observada", "M3_diff_FM_complete_replicada_mediana"
    ),
    valor = c(
      m1_factor,
      m2_mean_obs, m2_mean_rep,
      m2_median_obs, m2_median_rep,
      mean(obs_diff), median(full_rep),
      mean(obs_diff[complete_pairs]), median(complete_rep)
    )
  )
  write_csv_utf8(refit, file.path(out_tables, "resultados_refit.csv"))

  writeLines(c(
    capture.output(sessionInfo()),
    paste("CmdStan:", as.character(cmdstanr::cmdstan_version())),
    "",
    "M1 usa a especificação REV5 final; M2 e M3 preservam a especificação REV4.1 auditada.",
    "O PSIS-LOO por país final permanece no diretório resultados_auditados; ele não deve ser usado como validação de transporte para países novos."
  ), file.path(out_logs, "session_info_refit.txt"))

  log_msg("Reamostragem concluída.")
  invisible(refit)
}

if (run_stan) {
  run_refit()
} else {
  run_audit()
}
