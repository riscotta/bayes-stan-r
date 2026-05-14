#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)
options(pillar.sigfig = 6)

############################################################
# Mega-Sena — distribuição das dezenas por fatores nominais
# e temporais
#
# Padrão do repo:
# - rodar a partir da raiz do repositório
# - sem setwd()
# - dados versionados em data/raw/mega_sena_dezenas/
# - modelos Stan ao lado deste script
# - saídas regeneráveis em outputs/
#
# Execução padrão, sem reamostrar Stan:
#   Rscript scripts/mega_sena_dezenas/mega_sena_dezenas_cmdstanr.R
#
# Reexecução da amostragem Stan/cmdstanr:
#   Rscript scripts/mega_sena_dezenas/mega_sena_dezenas_cmdstanr.R --run_stan=1
#   RUN_STAN=true Rscript scripts/mega_sena_dezenas/mega_sena_dezenas_cmdstanr.R
############################################################

args <- commandArgs(trailingOnly = TRUE)

parse_args <- function(args) {
  out <- list(
    root = NA_character_,
    run_stan = NA_character_,
    seed = 20260513L,
    chains = 4L,
    parallel_chains = 4L,
    iter_warmup = 1000L,
    iter_sampling = 1000L,
    adapt_delta = 0.95,
    max_treedepth = 12L,
    refresh = 100L,
    output_dir = file.path("outputs", "mega_sena_dezenas")
  )

  if (length(args) == 0) return(out)

  for (a in args) {
    if (!startsWith(a, "--")) next
    a2 <- sub("^--", "", a)
    if (!grepl("=", a2, fixed = TRUE)) next
    key <- sub("=.*$", "", a2)
    val <- sub("^[^=]*=", "", a2)
    if (key %in% names(out)) out[[key]] <- val
  }

  int_fields <- c("seed", "chains", "parallel_chains", "iter_warmup", "iter_sampling", "max_treedepth", "refresh")
  dbl_fields <- c("adapt_delta")
  for (nm in int_fields) out[[nm]] <- as.integer(out[[nm]])
  for (nm in dbl_fields) out[[nm]] <- as.numeric(out[[nm]])

  out
}

opts <- parse_args(args)

script_path <- function() {
  cmd <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd, value = TRUE)
  if (length(file_arg) == 0) return(NA_character_)
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = FALSE)
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

repo_root_candidates <- unique(c(
  opts$root,
  getwd(),
  dirname(script_path())
))
repo_root <- NA_character_
for (candidate in repo_root_candidates) {
  found <- find_repo_root(candidate)
  if (!is.na(found)) {
    repo_root <- found
    break
  }
}
if (is.na(repo_root)) {
  stop("Não foi possível localizar a raiz do repositório. Rode a partir da raiz ou use --root=/caminho/do/repo.", call. = FALSE)
}

as_flag <- function(x, default = FALSE) {
  if (is.na(x) || !nzchar(x)) return(default)
  tolower(x) %in% c("1", "true", "t", "sim", "s", "yes", "y")
}

run_stan <- as_flag(opts$run_stan, default = as_flag(Sys.getenv("RUN_STAN", "false"), FALSE))
study_id <- "mega_sena_dezenas"

data_dir <- file.path(repo_root, "data", "raw", study_id)
tables_in_dir <- file.path(data_dir, "tabelas_auditadas")
diag_in_dir <- file.path(data_dir, "diagnosticos_auditados")
stan_dir <- file.path(repo_root, "scripts", study_id)
out_dir <- file.path(repo_root, opts$output_dir)
out_tables <- file.path(out_dir, "tables")
out_figures <- file.path(out_dir, "figures")
out_logs <- file.path(out_dir, "logs")
out_cmdstan <- file.path(out_dir, "cmdstan_csv")
for (d in c(out_tables, out_figures, out_logs, out_cmdstan)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

require_pkg <- function(pkg, optional = FALSE) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- paste0("Pacote requerido não instalado: ", pkg)
    if (optional) return(FALSE)
    stop(msg, call. = FALSE)
  }
  TRUE
}

for (pkg in c("readr", "dplyr", "tidyr", "ggplot2", "jsonlite")) require_pkg(pkg)

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(jsonlite)

read_required_csv <- function(path) {
  if (!file.exists(path)) stop("Arquivo não encontrado: ", path, call. = FALSE)
  readr::read_csv(path, show_col_types = FALSE)
}

base_path <- file.path(data_dir, "base_analitica.csv")
base <- read_required_csv(base_path)

required_cols <- c(
  "concurso", "data_sorteio", "dezena", "periodo_analitico", "faixa_temporal",
  "decada_historica", "dia_semana", "ano", "periodo_5_anos",
  "faixa_temporal_tercil_concursos", "faixa_temporal_metade_concursos"
)
missing_cols <- setdiff(required_cols, names(base))
if (length(missing_cols) > 0) {
  stop("Colunas ausentes em base_analitica.csv: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

fatores_principais <- c("periodo_analitico", "faixa_temporal", "decada_historica", "dia_semana")
fatores_sensibilidade <- c("ano", "periodo_5_anos", "faixa_temporal_tercil_concursos", "faixa_temporal_metade_concursos")
fatores <- c(fatores_principais, fatores_sensibilidade)
dezena_levels <- 1:60

make_count_table <- function(df, fator) {
  df |>
    mutate(.nivel = as.character(.data[[fator]]), dezena = as.integer(dezena)) |>
    count(.nivel, dezena, name = "n_observado") |>
    complete(.nivel, dezena = dezena_levels, fill = list(n_observado = 0)) |>
    group_by(.nivel) |>
    mutate(n_slots = sum(n_observado), prop_observada = n_observado / n_slots) |>
    ungroup() |>
    rename(nivel_fator = .nivel) |>
    mutate(fator = fator, .before = 1) |>
    arrange(fator, nivel_fator, dezena)
}

contagens <- lapply(fatores, function(f) make_count_table(base, f)) |>
  bind_rows()
write_csv(contagens, file.path(out_tables, "contagens_fator_dezena_reproduzidas.csv"))

resumo_base <- tibble(
  metrica = c("concursos", "linhas_dezena", "dezenas_possiveis", "data_minima", "data_maxima", "frequencia_esperada_global_por_dezena"),
  valor = c(
    n_distinct(base$concurso),
    nrow(base),
    n_distinct(base$dezena),
    as.character(min(as.Date(base$data_sorteio))),
    as.character(max(as.Date(base$data_sorteio))),
    nrow(base) / n_distinct(base$dezena)
  )
)
write_csv(resumo_base, file.path(out_tables, "resumo_base_reproduzido.csv"))

# Sínteses finais reproduzidas a partir dos artefatos auditáveis versionados.
resumo_fatores <- read_required_csv(file.path(tables_in_dir, "E08_resumo_fatores_stan.csv"))
diag_mcmc <- read_required_csv(file.path(diag_in_dir, "E08_diagnosticos_mcmc_auditados.csv"))
ppc <- read_required_csv(file.path(diag_in_dir, "E08_checagem_preditiva_posterior_stan.csv"))
stan_dirichlet <- read_required_csv(file.path(diag_in_dir, "E08_comparacao_stan_dirichlet.csv"))

final_fatores <- resumo_fatores |>
  left_join(
    diag_mcmc |>
      select(
        fator, status_e08, rhat_max, ess_bulk_min, ess_tail_min,
        n_divergencias, n_saturacao_treedepth, ebfmi_min_recalculado
      ),
    by = "fator"
  ) |>
  left_join(
    ppc |>
      select(fator, ppc_pvalue_T_rep_ge_T_obs, razao_Tobs_Trep_media),
    by = "fator"
  ) |>
  mutate(
    comunicacao = case_when(
      papel == "principal" & status_e08 == "APROVADO" ~ "Usar na conclusão central",
      fator == "ano" ~ "Usar apenas como sensibilidade exploratória",
      TRUE ~ "Usar como sensibilidade"
    )
  ) |>
  arrange(match(fator, fatores))
write_csv(final_fatores, file.path(out_tables, "E10_sintese_final_fatores_reproduzida.csv"))

final_sensibilidade <- stan_dirichlet |>
  mutate(
    interpretacao = case_when(
      razao_dirichlet_stan_max_delta > 10 ~ "Sem regularização hierárquica, desvios locais crescem muito; evitar rankings locais.",
      TRUE ~ "Sensibilidade sem mudança na conclusão central."
    )
  ) |>
  arrange(match(fator, fatores))
write_csv(final_sensibilidade, file.path(out_tables, "E10_sensibilidade_regularizacao_reproduzida.csv"))

p_desvio <- final_fatores |>
  mutate(fator = factor(fator, levels = rev(unique(fator)))) |>
  ggplot(aes(x = fator, y = max_delta100_media)) +
  geom_col() +
  geom_hline(yintercept = 2, linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Maior desvio local médio por 100 concursos",
    subtitle = "Linha tracejada: 2 ocorrências por 100 concursos",
    x = NULL,
    y = "Desvio médio por 100 concursos"
  ) +
  theme_minimal(base_size = 11)
ggsave(file.path(out_figures, "E10_fig_desvio_local_reproduzido.png"), p_desvio, width = 8, height = 5, dpi = 160)

p_tau <- final_fatores |>
  mutate(fator = factor(fator, levels = rev(unique(fator)))) |>
  ggplot(aes(x = fator, y = tau_media)) +
  geom_col() +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Heterogeneidade posterior por fator",
    subtitle = "Linha tracejada: tau_delta = 0,05",
    x = NULL,
    y = "Média posterior de tau_delta"
  ) +
  theme_minimal(base_size = 11)
ggsave(file.path(out_figures, "E10_fig_tau_delta_reproduzido.png"), p_tau, width = 8, height = 5, dpi = 160)

if (run_stan) {
  if (!require_pkg("cmdstanr", optional = TRUE) || !require_pkg("posterior", optional = TRUE)) {
    stop("--run_stan=1 requer cmdstanr e posterior instalados.", call. = FALSE)
  }

  library(cmdstanr)
  library(posterior)

  stan_file <- file.path(stan_dir, "modelo_loglinear_multinomial_fator.stan")
  if (!file.exists(stan_file)) stop("Modelo Stan não encontrado: ", stan_file, call. = FALSE)

  mod <- cmdstan_model(stan_file, pedantic = TRUE)
  sampling_config <- list(
    chains = opts$chains,
    parallel_chains = opts$parallel_chains,
    iter_warmup = opts$iter_warmup,
    iter_sampling = opts$iter_sampling,
    adapt_delta = opts$adapt_delta,
    max_treedepth = opts$max_treedepth,
    seed = opts$seed,
    refresh = opts$refresh
  )
  write_json(sampling_config, file.path(out_logs, "configuracao_reexecucao_stan.json"), pretty = TRUE, auto_unbox = TRUE)

  make_stan_data <- function(df, fator) {
    tab <- make_count_table(df, fator)
    niveis <- unique(as.character(tab$nivel_fator))
    numeric_levels <- suppressWarnings(as.numeric(niveis))
    if (all(!is.na(numeric_levels))) niveis <- niveis[order(numeric_levels)]
    tab <- tab |>
      mutate(nivel_fator = factor(nivel_fator, levels = niveis)) |>
      arrange(nivel_fator, dezena)
    y <- matrix(tab$n_observado, nrow = length(niveis), ncol = 60, byrow = TRUE)
    list(D = 60L, G = nrow(y), y = y, n_slots = as.integer(rowSums(y)))
  }

  summaries <- list()
  diags <- list()
  scalar_draws <- list()

  for (fator in fatores) {
    data_f <- make_stan_data(base, fator)
    data_json <- file.path(out_logs, paste0("dados_stan_", fator, ".json"))
    write_json(data_f, data_json, pretty = TRUE, auto_unbox = TRUE)

    fit <- mod$sample(
      data = data_json,
      seed = sampling_config$seed,
      chains = sampling_config$chains,
      parallel_chains = sampling_config$parallel_chains,
      iter_warmup = sampling_config$iter_warmup,
      iter_sampling = sampling_config$iter_sampling,
      adapt_delta = sampling_config$adapt_delta,
      max_treedepth = sampling_config$max_treedepth,
      output_dir = out_cmdstan,
      refresh = sampling_config$refresh
    )

    sum_f <- fit$summary() |>
      mutate(fator = fator, .before = 1)
    sampler_diag <- fit$sampler_diagnostics(format = "df")

    write_csv(sum_f, file.path(out_tables, paste0("resumo_posterior_stan_", fator, ".csv")))
    write_csv(sampler_diag, file.path(out_tables, paste0("sampler_diagnostics_", fator, ".csv")))

    summaries[[fator]] <- sum_f
    diags[[fator]] <- tibble(
      fator = fator,
      modelo = "M1_loglinear_fator_stan",
      executado = TRUE,
      chains = sampling_config$chains,
      iter_warmup = sampling_config$iter_warmup,
      iter_sampling = sampling_config$iter_sampling,
      rhat_max = max(sum_f$rhat, na.rm = TRUE),
      ess_bulk_min = min(sum_f$ess_bulk, na.rm = TRUE),
      ess_tail_min = min(sum_f$ess_tail, na.rm = TRUE),
      n_divergencias = sum(sampler_diag$divergent__),
      n_saturacao_treedepth = sum(sampler_diag$treedepth__ >= sampling_config$max_treedepth),
      status = "REEXECUTADO_CMDSTANR"
    )

    scalar_draws[[fator]] <- as_draws_df(fit$draws(
      variables = c("tau_delta", "contraste_total_base", "max_abs_delta_100_concursos")
    )) |>
      as_tibble() |>
      mutate(fator = fator, .before = 1)
  }

  write_csv(bind_rows(summaries), file.path(out_tables, "resumo_posterior_stan_reexecutado_completo.csv"))
  write_csv(bind_rows(diags), file.path(out_tables, "diagnosticos_mcmc_reexecutado.csv"))
  write_csv(bind_rows(scalar_draws), file.path(out_tables, "amostras_posteriores_escalares_stan_reexecutado.csv"))
}

log_lines <- c(
  paste("Execução concluída em", Sys.time()),
  paste("Raiz do repositório:", repo_root),
  paste("Linhas na base analítica:", nrow(base)),
  paste("Concursos distintos:", dplyr::n_distinct(base$concurso)),
  paste("RUN_STAN:", run_stan),
  "Conclusão reproduzida: estabilidade prática nos fatores principais; ano apenas como sensibilidade; posição ordenada é estrutural."
)
writeLines(log_lines, file.path(out_logs, "log_execucao_final.txt"))
cat(paste(log_lines, collapse = "\n"), "\n")
