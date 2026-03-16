#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

############################################################
# DETER mensal por bioma-UF
# 1 modelo STAN + verificação forte
#
# Padrão do repo:
# - rodar a partir do root do repositório
# - sem setwd()
# - entrada padrão em data/raw/
# - resultados finais em console por padrão
# - artefatos opcionais em outputs/
#
# Exemplos:
#   Rscript scripts/deter_mensal_bioma_uf/deter_mensal_bioma_uf_cmdstanr.R
#   Rscript scripts/deter_mensal_bioma_uf/deter_mensal_bioma_uf_cmdstanr.R --input_csv=data/raw/deter_mensal_bioma_uf.csv
#   Rscript scripts/deter_mensal_bioma_uf/deter_mensal_bioma_uf_cmdstanr.R --target_cycles=2024/25,2025/26 --save_plots=1 --save_report=1
############################################################

args <- commandArgs(trailingOnly = TRUE)
AUTO_INSTALL_PKGS <- "--auto-install" %in% args

parse_args <- function(args) {
  out <- list(
    input_csv = file.path("data", "raw", "deter_mensal_bioma_uf.csv"),
    target_cycles = "2024/25,2025/26",

    seed = 20260316L,
    chains = 4L,
    parallel_chains = 4L,
    iter_warmup = 1500L,
    iter_sampling = 1500L,
    adapt_delta = 0.99,
    max_treedepth = 15L,
    refresh = 100L,
    keep_temp_files = 0L,

    run_prior_predictive = 1L,
    prior_draws = 1000L,
    posterior_draws_for_ppc = 300L,

    save_plots = 0L,
    save_report = 0L,
    output_tables_dir = file.path("outputs", "tables", "deter_mensal_bioma_uf"),
    output_figures_dir = file.path("outputs", "figures", "deter_mensal_bioma_uf"),
    output_prefix = "deter_mensal_bioma_uf"
  )

  if (length(args) == 0) return(out)

  for (a in args) {
    if (!startsWith(a, "--")) next
    a2 <- sub("^--", "", a)
    if (!grepl("=", a2, fixed = TRUE)) next

    key <- sub("=.*$", "", a2)
    val <- sub("^.*=", "", a2)

    if (key %in% names(out)) out[[key]] <- val
  }

  out$seed <- as.integer(out$seed)
  out$chains <- as.integer(out$chains)
  out$parallel_chains <- as.integer(out$parallel_chains)
  out$iter_warmup <- as.integer(out$iter_warmup)
  out$iter_sampling <- as.integer(out$iter_sampling)
  out$adapt_delta <- as.numeric(out$adapt_delta)
  out$max_treedepth <- as.integer(out$max_treedepth)
  out$refresh <- as.integer(out$refresh)
  out$keep_temp_files <- as.integer(out$keep_temp_files)
  out$run_prior_predictive <- as.integer(out$run_prior_predictive)
  out$prior_draws <- as.integer(out$prior_draws)
  out$posterior_draws_for_ppc <- as.integer(out$posterior_draws_for_ppc)
  out$save_plots <- as.integer(out$save_plots)
  out$save_report <- as.integer(out$save_report)

  out$target_cycles <- trimws(unlist(strsplit(as.character(out$target_cycles), ",", fixed = TRUE)))
  out$target_cycles <- out$target_cycles[nzchar(out$target_cycles)]

  if (length(out$target_cycles) != 2) {
    stop(
      "'--target_cycles' deve informar exatamente 2 ciclos, separados por vírgula.\n",
      "Exemplo: --target_cycles=2024/25,2025/26",
      call. = FALSE
    )
  }

  out
}

opts <- parse_args(args)

ensure_repos_for_cmdstanr <- function() {
  repos <- getOption("repos")
  if (is.null(repos) || length(repos) == 0) repos <- c()
  if (identical(repos, "@CRAN@")) repos <- c()
  if (length(repos) > 0 && is.null(names(repos))) names(repos) <- paste0("repo", seq_along(repos))

  if (!("CRAN" %in% names(repos)) || is.null(repos[["CRAN"]]) || repos[["CRAN"]] == "" || repos[["CRAN"]] == "@CRAN@") {
    repos[["CRAN"]] <- "https://cloud.r-project.org"
  }
  if (!("stan" %in% names(repos)) || is.null(repos[["stan"]]) || repos[["stan"]] == "") {
    repos[["stan"]] <- "https://stan-dev.r-universe.dev"
  }
  options(repos = repos)
}

ensure_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (!AUTO_INSTALL_PKGS) {
      stop(
        "Pacote '", pkg, "' não está instalado.\n\n",
        "Como resolver (recomendado):\n",
        "  Rscript scripts/_setup/install_deps.R --all\n\n",
        "Alternativa (manual, dentro do R):\n",
        "  install.packages('", pkg, "')\n\n",
        "Se você realmente quer auto-instalar neste script:\n",
        "  Rscript scripts/deter_mensal_bioma_uf/deter_mensal_bioma_uf_cmdstanr.R --auto-install\n",
        call. = FALSE
      )
    }
    message("Instalando pacote ausente: ", pkg)
    ensure_repos_for_cmdstanr()
    install.packages(pkg, dependencies = TRUE)
  }
  invisible(TRUE)
}

pkgs <- c(
  "cmdstanr", "posterior", "bayesplot", "ggplot2", "dplyr", "tidyr",
  "readr", "tibble", "stringr", "forcats", "purrr", "withr", "hexbin"
)
invisible(lapply(pkgs, ensure_pkg))

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(bayesplot)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(stringr)
  library(forcats)
  library(purrr)
  library(withr)
})

bayesplot::color_scheme_set("mix-blue-pink")

fail <- function(...) stop(paste0(...), call. = FALSE)

first_existing_col <- function(df, candidates, field_name) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) {
    fail(
      "Coluna obrigatória ausente para '", field_name,
      "'. Candidatas testadas: ", paste(candidates, collapse = ", ")
    )
  }
  hit[[1]]
}

coerce_month <- function(x) {
  if (is.numeric(x)) return(as.integer(x))
  x_chr <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  map <- c(
    "1" = 1L, "01" = 1L, "jan" = 1L, "janeiro" = 1L,
    "2" = 2L, "02" = 2L, "fev" = 2L, "fevereiro" = 2L,
    "3" = 3L, "03" = 3L, "mar" = 3L, "marco" = 3L, "março" = 3L,
    "4" = 4L, "04" = 4L, "abr" = 4L, "abril" = 4L,
    "5" = 5L, "05" = 5L, "mai" = 5L, "maio" = 5L,
    "6" = 6L, "06" = 6L, "jun" = 6L, "junho" = 6L,
    "7" = 7L, "07" = 7L, "jul" = 7L, "julho" = 7L,
    "8" = 8L, "08" = 8L, "ago" = 8L, "agosto" = 8L,
    "9" = 9L, "09" = 9L, "set" = 9L, "setembro" = 9L,
    "10" = 10L, "out" = 10L, "outubro" = 10L,
    "11" = 11L, "nov" = 11L, "novembro" = 11L,
    "12" = 12L, "dez" = 12L, "dezembro" = 12L
  )
  out <- unname(map[x_chr])
  out <- as.integer(out)
  if (anyNA(out)) {
    bad <- unique(x_chr[is.na(out)])
    fail("Mês não reconhecido: ", paste(bad, collapse = ", "))
  }
  out
}

compute_cycle <- function(ano, mes) {
  start_year <- ifelse(mes >= 8L, ano, ano - 1L)
  paste0(start_year, "/", substr(start_year + 1L, 3, 4))
}

cleanup_paths <- character(0)
register_cleanup <- function(paths) {
  paths <- paths[!is.na(paths) & nzchar(paths)]
  if (length(paths) == 0) return(invisible(NULL))
  cleanup_paths <<- unique(c(cleanup_paths, paths))
  invisible(NULL)
}

cleanup_all <- function() {
  for (p in rev(cleanup_paths)) {
    if (dir.exists(p)) {
      try(unlink(p, recursive = TRUE, force = TRUE), silent = TRUE)
    } else if (file.exists(p)) {
      try(unlink(p, force = TRUE), silent = TRUE)
    }
  }
}

extract_draws_matrix <- function(fit, variable) {
  posterior::as_draws_matrix(fit$draws(variables = variable))
}

interval_coverage <- function(y, yrep_mat, probs = c(0.5, 0.8, 0.95)) {
  purrr::map_dfr(probs, function(p) {
    alpha <- (1 - p) / 2
    lower <- apply(yrep_mat, 2, stats::quantile, probs = alpha)
    upper <- apply(yrep_mat, 2, stats::quantile, probs = 1 - alpha)
    tibble(
      intervalo = paste0(round(p * 100), "%"),
      cobertura = mean(y >= lower & y <= upper)
    )
  })
}

pit_values <- function(y, yrep_mat) {
  vapply(seq_along(y), function(i) mean(yrep_mat[, i] <= y[i]), numeric(1))
}

guess_delim <- function(path) {
  first_line <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
  if (length(first_line) == 0) return(",")

  candidates <- c(",", ";", "\t", "|")
  counts <- vapply(candidates, function(d) stringr::str_count(first_line, stringr::fixed(d)), integer(1))
  candidates[[which.max(counts)]]
}

make_output_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

report_lines <- function(base,
                         dup_check,
                         divergences,
                         max_treedepth_hits,
                         eb_fmi_by_chain,
                         rhat_bad,
                         ess_bulk_bad,
                         ess_tail_bad,
                         pars_summary,
                         cycle_effect,
                         prob_cycle_drop,
                         prob_cycle_rise,
                         coverage_tbl,
                         coverage_by_cycle,
                         coverage_by_month,
                         coverage_by_stratum,
                         model_diag,
                         run_prior_predictive) {
  capture.output({
    cat(
      "============================================================\n",
      "RELATÓRIO FINAL — MODELO STAN (1 modelo, sem comparação)\n",
      "============================================================\n",
      sep = ""
    )

    cat("\n[1] Base analítica\n")
    cat("Linhas:", nrow(base), "\n")
    cat("Estratos bioma::UF:", n_distinct(base$stratum), "\n")
    cat("Meses:", paste(levels(base$mes_fator), collapse = ", "), "\n")
    cat("Ciclos:", paste(sort(unique(base$ciclo)), collapse = " vs "), "\n")
    cat("Proporção de zeros na área:", round(mean(base$area_alerta_km2 == 0), 4), "\n")
    if (nrow(dup_check) > 0) {
      cat("Duplicidades detectadas e agregadas antes do ajuste:", nrow(dup_check), "\n")
    } else {
      cat("Duplicidades bioma-UF-ano-mês-ciclo: nenhuma\n")
    }

    cat("\n[2] Diagnóstico de amostragem\n")
    cat("Divergências:", divergences, "\n")
    cat("Hits em max_treedepth:", max_treedepth_hits, "\n")
    cat("E-BFMI por cadeia:", paste(round(eb_fmi_by_chain, 3), collapse = ", "), "\n")
    cat("Parâmetros com R-hat > 1.01:", rhat_bad, "\n")
    cat("Parâmetros com ESS bulk < 400:", ess_bulk_bad, "\n")
    cat("Parâmetros com ESS tail < 400:", ess_tail_bad, "\n")

    cat("\n[3] Parâmetros principais\n")
    print(pars_summary, n = Inf)

    cat("\n[4] Efeito global do ciclo\n")
    print(cycle_effect, n = Inf)
    cat("Probabilidade posterior de queda no ciclo mais recente (razão < 1): ", round(prob_cycle_drop, 4), "\n", sep = "")
    cat("Probabilidade posterior de alta no ciclo mais recente (razão > 1): ", round(prob_cycle_rise, 4), "\n", sep = "")

    cat("\n[5] Cobertura preditiva global\n")
    print(coverage_tbl, n = Inf)

    cat("\n[6] Cobertura por ciclo\n")
    print(coverage_by_cycle, n = Inf)

    cat("\n[7] Cobertura por mês\n")
    print(coverage_by_month, n = Inf)

    cat("\n[8] Estratos com maior erro absoluto médio\n")
    print(head(coverage_by_stratum, 10), n = 10)

    cat("\n[9] Resumo do NUTS de cmdstanr\n")
    print(model_diag)

    cat("\n[10] Objetos disponíveis em memória\n")
    cat(
      paste(
        c(
          "fit",
          if (run_prior_predictive == 1L) "prior_fit" else NULL,
          "base",
          "pars_summary",
          "coverage_tbl",
          "coverage_by_cycle",
          "coverage_by_month",
          "coverage_by_stratum",
          "cycle_effect",
          "trace_plot",
          "rank_plot",
          "pairs_plot",
          "ppc_dens_plot",
          "ppc_scatter_plot",
          "pit_plot",
          "residual_plot",
          "month_interval_plot",
          if (run_prior_predictive == 1L) "prior_ppc_plot" else NULL
        ),
        collapse = ", "
      ),
      "\n"
    )
  })
}

if (!file.exists(opts$input_csv)) {
  fail(
    "Arquivo CSV não encontrado em: ",
    normalizePath(opts$input_csv, winslash = "/", mustWork = FALSE), "\n",
    "Coloque o arquivo no caminho padrão 'data/raw/deter_mensal_bioma_uf.csv' ou informe '--input_csv=/caminho/arquivo.csv'."
  )
}

delim <- guess_delim(opts$input_csv)
df_raw <- suppressMessages(readr::read_delim(
  opts$input_csv,
  delim = delim,
  show_col_types = FALSE,
  progress = FALSE,
  trim_ws = TRUE
))
if (nrow(df_raw) == 0) fail("O CSV está vazio.")

col_bioma <- first_existing_col(df_raw, c("bioma", "BIOMA"), "bioma")
col_uf    <- first_existing_col(df_raw, c("uf", "UF"), "uf")
col_ano   <- first_existing_col(df_raw, c("ano", "ANO", "year", "YEAR"), "ano")
col_mes   <- first_existing_col(df_raw, c("mes", "MES", "month", "MONTH"), "mes")
col_area  <- first_existing_col(df_raw, c("area_alerta_km2", "AREA_ALERTA_KM2", "area_km2", "AREAMUNKM", "area"), "area")
col_ciclo <- names(df_raw)[names(df_raw) %in% c("ciclo", "CICLO", "cycle", "CYCLE")]
col_ciclo <- if (length(col_ciclo) == 0) NA_character_ else col_ciclo[[1]]

base <- df_raw %>%
  transmute(
    bioma = stringr::str_squish(as.character(.data[[col_bioma]])),
    uf = stringr::str_squish(as.character(.data[[col_uf]])),
    ano = as.integer(.data[[col_ano]]),
    mes = coerce_month(.data[[col_mes]]),
    area_alerta_km2 = suppressWarnings(as.numeric(.data[[col_area]])),
    ciclo = if (!is.na(col_ciclo)) stringr::str_squish(as.character(.data[[col_ciclo]])) else NA_character_
  )

if (anyNA(base$ano)) fail("Há valores inválidos em 'ano'.")
if (anyNA(base$mes)) fail("Há valores inválidos em 'mes'.")
if (anyNA(base$area_alerta_km2)) fail("Há valores inválidos em 'area_alerta_km2'.")
if (any(base$area_alerta_km2 < 0)) fail("Há áreas negativas no CSV. Isso não é aceitável.")

base <- base %>%
  mutate(
    bioma = stringr::str_to_upper(stringr::str_replace_all(bioma, "Á|À|Â|Ã", "A")),
    bioma = stringr::str_replace_all(bioma, "É|Ê", "E"),
    bioma = stringr::str_replace_all(bioma, "Í", "I"),
    bioma = stringr::str_replace_all(bioma, "Ó|Ô|Õ", "O"),
    bioma = stringr::str_replace_all(bioma, "Ú", "U"),
    uf = stringr::str_to_upper(uf),
    ciclo = dplyr::if_else(is.na(ciclo) | ciclo == "", compute_cycle(ano, mes), ciclo)
  )

wanted_months <- c(8L, 9L, 10L, 11L, 12L, 1L)
base <- base %>%
  filter(mes %in% wanted_months, ciclo %in% opts$target_cycles)

if (nrow(base) == 0) {
  fail(
    "Após os filtros de ciclo/mês, não sobrou nenhuma linha.\n",
    "Verifique '--target_cycles=' e o conteúdo do CSV."
  )
}

dup_check <- base %>% count(bioma, uf, ano, mes, ciclo, name = "n") %>% filter(n > 1)
if (nrow(dup_check) > 0) {
  base <- base %>%
    group_by(bioma, uf, ano, mes, ciclo) %>%
    summarise(area_alerta_km2 = sum(area_alerta_km2), .groups = "drop")
}

base <- base %>%
  mutate(
    cycle_start_year = ifelse(mes >= 8L, ano, ano - 1L),
    cycle01 = ifelse(cycle_start_year == max(cycle_start_year), 1L, 0L),
    mes_fator = factor(mes, levels = c(8L, 9L, 10L, 11L, 12L, 1L), labels = c("Ago", "Set", "Out", "Nov", "Dez", "Jan")),
    stratum = interaction(bioma, uf, sep = "::", drop = TRUE, lex.order = TRUE),
    y = log1p(area_alerta_km2)
  ) %>%
  arrange(bioma, uf, cycle_start_year, factor(mes, levels = c(8L, 9L, 10L, 11L, 12L, 1L)))

if (n_distinct(base$cycle01) != 2) {
  fail("O script espera exatamente dois ciclos comparáveis. Verifique '--target_cycles=' e a base.")
}
if (n_distinct(base$mes_fator) < 2) fail("É preciso haver pelo menos 2 meses distintos no recorte.")
if (n_distinct(base$stratum) < 2) fail("É preciso haver pelo menos 2 estratos bioma::UF no recorte.")

base <- base %>%
  mutate(
    stratum_id = as.integer(fct_inorder(stratum)),
    month_id = as.integer(fct_inorder(mes_fator))
  )

stan_data <- list(
  N = nrow(base),
  J_stratum = n_distinct(base$stratum_id),
  J_month = n_distinct(base$month_id),
  y = base$y,
  stratum = base$stratum_id,
  month_id = base$month_id,
  cycle01 = base$cycle01,
  y_bar = mean(base$y),
  y_sd = stats::sd(base$y),
  prior_only = 0L
)

stan_code <- '
data {
  int<lower=1> N;
  int<lower=1> J_stratum;
  int<lower=1> J_month;
  vector[N] y;
  array[N] int<lower=1, upper=J_stratum> stratum;
  array[N] int<lower=1, upper=J_month> month_id;
  array[N] int<lower=0, upper=1> cycle01;
  real y_bar;
  real<lower=0> y_sd;
  int<lower=0, upper=1> prior_only;
}
parameters {
  real alpha;
  real beta_cycle;
  vector[J_stratum] z_stratum_raw;
  vector[J_month] month_raw;
  vector[J_stratum] z_cycle_raw;
  real<lower=0> sigma;
  real<lower=0> sigma_stratum;
  real<lower=0> sigma_month;
  real<lower=0> sigma_cycle_stratum;
}
transformed parameters {
  vector[J_stratum] a_stratum = sigma_stratum * z_stratum_raw;
  vector[J_stratum] b_cycle_stratum = sigma_cycle_stratum * z_cycle_raw;
  vector[J_month] month_eff;
  vector[N] mu;

  month_eff = sigma_month * (month_raw - mean(month_raw));

  for (n in 1:N) {
    mu[n] = alpha
            + a_stratum[stratum[n]]
            + month_eff[month_id[n]]
            + (beta_cycle + b_cycle_stratum[stratum[n]]) * cycle01[n];
  }
}
model {
  alpha ~ normal(y_bar, fmax(1.5, 2 * y_sd));
  beta_cycle ~ normal(0, 1);

  z_stratum_raw ~ normal(0, 1);
  z_cycle_raw ~ normal(0, 1);
  month_raw ~ normal(0, 1);

  sigma ~ exponential(1);
  sigma_stratum ~ exponential(1);
  sigma_month ~ exponential(1);
  sigma_cycle_stratum ~ exponential(1);

  if (!prior_only) {
    y ~ normal(mu, sigma);
  }
}
generated quantities {
  vector[N] log_lik;
  vector[N] y_rep_log1p;
  vector[N] y_rep_km2;
  real mean_cycle0_log1p = 0;
  real mean_cycle1_log1p = 0;
  real delta_cycle_log1p;
  real ratio_cycle1_cycle0_km2;
  int n0 = 0;
  int n1 = 0;

  for (n in 1:N) {
    y_rep_log1p[n] = normal_rng(mu[n], sigma);
    y_rep_km2[n] = fmax(exp(y_rep_log1p[n]) - 1, 0);
    log_lik[n] = prior_only ? 0 : normal_lpdf(y[n] | mu[n], sigma);

    if (cycle01[n] == 0) {
      mean_cycle0_log1p += y_rep_log1p[n];
      n0 += 1;
    } else {
      mean_cycle1_log1p += y_rep_log1p[n];
      n1 += 1;
    }
  }

  mean_cycle0_log1p /= n0;
  mean_cycle1_log1p /= n1;
  delta_cycle_log1p = mean_cycle1_log1p - mean_cycle0_log1p;
  ratio_cycle1_cycle0_km2 = exp(mean_cycle1_log1p) / exp(mean_cycle0_log1p);
}
'

stan_dir <- tempfile(pattern = "stan_work_")
dir.create(stan_dir, recursive = TRUE, showWarnings = FALSE)
register_cleanup(stan_dir)
stan_file <- file.path(stan_dir, "deter_model.stan")
writeLines(stan_code, con = stan_file, useBytes = TRUE)
register_cleanup(stan_file)

mod <- cmdstanr::cmdstan_model(
  stan_file = stan_file,
  stanc_options = list("O1" = TRUE),
  cpp_options = list(stan_threads = FALSE),
  quiet = TRUE
)
try(register_cleanup(mod$exe_file()), silent = TRUE)

prior_fit <- NULL
if (opts$run_prior_predictive == 1L) {
  prior_dir <- tempfile(pattern = "prior_draws_")
  dir.create(prior_dir, recursive = TRUE, showWarnings = FALSE)
  register_cleanup(prior_dir)

  prior_fit <- mod$sample(
    data = modifyList(stan_data, list(prior_only = 1L)),
    seed = opts$seed,
    chains = opts$chains,
    parallel_chains = opts$parallel_chains,
    iter_warmup = 0,
    iter_sampling = opts$prior_draws,
    refresh = 0,
    fixed_param = TRUE,
    output_dir = prior_dir,
    show_messages = FALSE,
    show_exceptions = FALSE
  )
  register_cleanup(prior_fit$output_files())
}

posterior_dir <- tempfile(pattern = "posterior_draws_")
dir.create(posterior_dir, recursive = TRUE, showWarnings = FALSE)
register_cleanup(posterior_dir)

fit <- mod$sample(
  data = stan_data,
  seed = opts$seed,
  chains = opts$chains,
  parallel_chains = opts$parallel_chains,
  iter_warmup = opts$iter_warmup,
  iter_sampling = opts$iter_sampling,
  refresh = opts$refresh,
  adapt_delta = opts$adapt_delta,
  max_treedepth = opts$max_treedepth,
  output_dir = posterior_dir,
  show_messages = FALSE,
  show_exceptions = FALSE
)
register_cleanup(fit$output_files())

key_pars <- c("alpha", "beta_cycle", "sigma", "sigma_stratum", "sigma_month", "sigma_cycle_stratum")
pars_summary <- fit$summary(variables = key_pars) %>%
  as_tibble() %>%
  select(variable, mean, median, sd, q5, q95, rhat, ess_bulk, ess_tail)

model_diag <- fit$diagnostic_summary()

sampler_diag <- fit$sampler_diagnostics(format = "matrix")
divergences <- sum(sampler_diag[, "divergent__"])
max_treedepth_hits <- sum(sampler_diag[, "treedepth__"] >= opts$max_treedepth)
energy <- sampler_diag[, "energy__"]
energy_by_chain <- split(energy, rep(seq_len(opts$chains), each = nrow(sampler_diag) / opts$chains))
eb_fmi_by_chain <- vapply(energy_by_chain, function(e) {
  if (length(e) < 2) return(NA_real_)
  mean(diff(e)^2) / stats::var(e)
}, numeric(1))

full_summary <- fit$summary()
rhat_bad <- full_summary %>% filter(!is.na(rhat), rhat > 1.01) %>% nrow()
ess_bulk_bad <- full_summary %>% filter(!is.na(ess_bulk), ess_bulk < 400) %>% nrow()
ess_tail_bad <- full_summary %>% filter(!is.na(ess_tail), ess_tail < 400) %>% nrow()

set.seed(opts$seed)
yrep_all <- extract_draws_matrix(fit, "y_rep_km2")
if (nrow(yrep_all) > opts$posterior_draws_for_ppc) {
  keep_idx <- sample.int(nrow(yrep_all), opts$posterior_draws_for_ppc)
  yrep_ppc <- yrep_all[keep_idx, , drop = FALSE]
} else {
  yrep_ppc <- yrep_all
}

coverage_tbl <- interval_coverage(base$area_alerta_km2, yrep_all, probs = c(0.5, 0.8, 0.95))
pit <- pit_values(base$area_alerta_km2, yrep_all)

pred_q <- tibble(
  obs_id = seq_len(ncol(yrep_all)),
  q05 = apply(yrep_all, 2, stats::quantile, probs = 0.05),
  q10 = apply(yrep_all, 2, stats::quantile, probs = 0.10),
  q50 = apply(yrep_all, 2, stats::quantile, probs = 0.50),
  q90 = apply(yrep_all, 2, stats::quantile, probs = 0.90),
  q95 = apply(yrep_all, 2, stats::quantile, probs = 0.95),
  mean_pred = colMeans(yrep_all)
)

obs_tbl <- base %>%
  mutate(obs_id = row_number()) %>%
  select(obs_id, bioma, uf, ciclo, mes_fator, area_alerta_km2)

calibration_tbl <- obs_tbl %>%
  left_join(pred_q, by = "obs_id") %>%
  mutate(
    covered_80 = area_alerta_km2 >= q10 & area_alerta_km2 <= q90,
    covered_95 = area_alerta_km2 >= q05 & area_alerta_km2 <= q95,
    resid = area_alerta_km2 - mean_pred
  )

coverage_by_cycle <- calibration_tbl %>%
  group_by(ciclo) %>%
  summarise(
    cobertura_80 = mean(covered_80),
    cobertura_95 = mean(covered_95),
    mae = mean(abs(resid)),
    .groups = "drop"
  )

coverage_by_month <- calibration_tbl %>%
  group_by(mes_fator) %>%
  summarise(
    cobertura_80 = mean(covered_80),
    cobertura_95 = mean(covered_95),
    mae = mean(abs(resid)),
    .groups = "drop"
  )

coverage_by_stratum <- calibration_tbl %>%
  unite("stratum", bioma, uf, sep = "::", remove = FALSE) %>%
  group_by(stratum) %>%
  summarise(
    cobertura_80 = mean(covered_80),
    cobertura_95 = mean(covered_95),
    mae = mean(abs(resid)),
    .groups = "drop"
  ) %>%
  arrange(desc(mae))

cycle_effect <- fit$summary(variables = c("beta_cycle", "delta_cycle_log1p", "ratio_cycle1_cycle0_km2")) %>%
  as_tibble() %>%
  select(variable, mean, median, sd, q5, q95)

ratio_draws <- posterior::as_draws_matrix(fit$draws("ratio_cycle1_cycle0_km2"))[, 1]
prob_cycle_drop <- mean(ratio_draws < 1)
prob_cycle_rise <- mean(ratio_draws > 1)

trace_plot <- bayesplot::mcmc_trace(
  fit$draws(variables = key_pars),
  facet_args = list(ncol = 2)
) + ggtitle("Trace plots — parâmetros principais")

rank_plot <- bayesplot::mcmc_rank_overlay(
  fit$draws(variables = key_pars)
) + ggtitle("Rank plots — parâmetros principais")

pairs_plot <- bayesplot::mcmc_pairs(
  fit$draws(variables = c("alpha", "beta_cycle", "sigma", "sigma_stratum", "sigma_month", "sigma_cycle_stratum")),
  diag_fun = "dens",
  off_diag_fun = "hex"
)

ppc_dens_plot <- bayesplot::ppc_dens_overlay(
  y = base$area_alerta_km2,
  yrep = yrep_ppc
) + ggtitle("PPC — densidade observada vs predita (km²)")

ppc_scatter_plot <- calibration_tbl %>%
  ggplot(aes(x = mean_pred, y = area_alerta_km2, color = ciclo)) +
  geom_point(alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  labs(
    title = "Observado vs média predita",
    x = "Média predita (km²)",
    y = "Observado (km²)"
  )

pit_plot <- tibble(pit = pit) %>%
  ggplot(aes(x = pit)) +
  geom_histogram(bins = 20) +
  labs(title = "PIT aproximado", x = "PIT", y = "Frequência")

residual_plot <- calibration_tbl %>%
  ggplot(aes(x = mean_pred, y = resid, color = ciclo)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(alpha = 0.8) +
  labs(title = "Resíduos vs média predita", x = "Média predita (km²)", y = "Resíduo (obs - pred)")

month_interval_plot <- calibration_tbl %>%
  mutate(mes_fator = factor(mes_fator, levels = c("Ago", "Set", "Out", "Nov", "Dez", "Jan"))) %>%
  ggplot(aes(x = mes_fator, y = area_alerta_km2, ymin = q10, ymax = q90, color = ciclo)) +
  geom_pointrange(position = position_dodge(width = 0.3)) +
  labs(title = "Observado com intervalo preditivo de 80% por mês", x = "Mês", y = "Área (km²)")

prior_ppc_plot <- NULL
if (!is.null(prior_fit)) {
  prior_yrep_all <- extract_draws_matrix(prior_fit, "y_rep_km2")
  if (nrow(prior_yrep_all) > opts$posterior_draws_for_ppc) {
    keep_idx <- sample.int(nrow(prior_yrep_all), opts$posterior_draws_for_ppc)
    prior_yrep_ppc <- prior_yrep_all[keep_idx, , drop = FALSE]
  } else {
    prior_yrep_ppc <- prior_yrep_all
  }
  prior_ppc_plot <- bayesplot::ppc_dens_overlay(
    y = base$area_alerta_km2,
    yrep = prior_yrep_ppc
  ) + ggtitle("Prior predictive check — escala km²")
}

lines_out <- report_lines(
  base = base,
  dup_check = dup_check,
  divergences = divergences,
  max_treedepth_hits = max_treedepth_hits,
  eb_fmi_by_chain = eb_fmi_by_chain,
  rhat_bad = rhat_bad,
  ess_bulk_bad = ess_bulk_bad,
  ess_tail_bad = ess_tail_bad,
  pars_summary = pars_summary,
  cycle_effect = cycle_effect,
  prob_cycle_drop = prob_cycle_drop,
  prob_cycle_rise = prob_cycle_rise,
  coverage_tbl = coverage_tbl,
  coverage_by_cycle = coverage_by_cycle,
  coverage_by_month = coverage_by_month,
  coverage_by_stratum = coverage_by_stratum,
  model_diag = model_diag,
  run_prior_predictive = opts$run_prior_predictive
)

cat(paste(lines_out, collapse = "\n"), sep = "\n")

plot_list <- Filter(Negate(is.null), list(
  trace_plot,
  rank_plot,
  pairs_plot,
  prior_ppc_plot,
  ppc_dens_plot,
  ppc_scatter_plot,
  pit_plot,
  residual_plot,
  month_interval_plot
))

if (opts$save_report == 1L) {
  make_output_dir(opts$output_tables_dir)
  report_path <- file.path(opts$output_tables_dir, paste0(opts$output_prefix, "_report.txt"))
  writeLines(lines_out, con = report_path, useBytes = TRUE)
  cat("\nRelatório salvo em: ", report_path, "\n", sep = "")
}

if (opts$save_plots == 1L) {
  make_output_dir(opts$output_figures_dir)
  plot_path <- file.path(opts$output_figures_dir, paste0(opts$output_prefix, "_plots.pdf"))
  grDevices::pdf(plot_path, width = 11, height = 8.5)
  for (p in plot_list) print(p)
  grDevices::dev.off()
  cat("Gráficos salvos em: ", plot_path, "\n", sep = "")
} else if (interactive()) {
  for (p in plot_list) print(p)
} else {
  cat("\nGráficos não foram salvos. Use '--save_plots=1' se quiser exportar PDF em outputs/figures/.\n")
}

if (opts$keep_temp_files != 1L) {
  try(register_cleanup(fit$output_files()), silent = TRUE)
  if (!is.null(prior_fit)) try(register_cleanup(prior_fit$output_files()), silent = TRUE)
  cleanup_all()
}

invisible(list(
  fit = fit,
  prior_fit = prior_fit,
  base = base,
  pars_summary = pars_summary,
  coverage_tbl = coverage_tbl,
  coverage_by_cycle = coverage_by_cycle,
  coverage_by_month = coverage_by_month,
  coverage_by_stratum = coverage_by_stratum,
  cycle_effect = cycle_effect,
  trace_plot = trace_plot,
  rank_plot = rank_plot,
  pairs_plot = pairs_plot,
  prior_ppc_plot = prior_ppc_plot,
  ppc_dens_plot = ppc_dens_plot,
  ppc_scatter_plot = ppc_scatter_plot,
  pit_plot = pit_plot,
  residual_plot = residual_plot,
  month_interval_plot = month_interval_plot
))
