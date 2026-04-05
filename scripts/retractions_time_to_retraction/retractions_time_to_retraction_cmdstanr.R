#!/usr/bin/env Rscript

###############################################################################
# Tempo ate retratacao de artigos cientificos — R + Stan via cmdstanr
# Base externa: Global Scientific Retractions 1927–2026 (Kaggle)
# Padrao do repositorio:
#   - sem setwd()
#   - execucao a partir da raiz do repo
#   - dados brutos fora do versionamento
#   - artefatos regeneraveis em outputs/
#
# Como rodar:
#   Rscript scripts/retractions_time_to_retraction/retractions_time_to_retraction_cmdstanr.R
#
# Exemplo com caminho explicito para o CSV:
#   Rscript scripts/retractions_time_to_retraction/retractions_time_to_retraction_cmdstanr.R \
#     --input_csv=data/raw/retractions_time_to_retraction/global_scientific_retractions_1927_2026.csv
#
# Dependencias:
#   Rscript scripts/_setup/install_deps.R --all
#   Rscript scripts/_setup/install_cmdstan.R
###############################################################################

options(stringsAsFactors = FALSE)
options(mc.cores = max(1L, parallel::detectCores(logical = TRUE)))
set.seed(42)

parse_args <- function(args) {
  out <- list(
    input_csv = file.path("data", "raw", "retractions_time_to_retraction", "global_scientific_retractions_1927_2026.csv"),
    figures_dir = file.path("outputs", "figures", "retractions_time_to_retraction"),
    tables_dir = file.path("outputs", "tables", "retractions_time_to_retraction"),
    models_dir = file.path("outputs", "models", "retractions_time_to_retraction"),
    cutoff_original_year = 2021L,
    publisher_min_n = 500L,
    subject_min_n = 1000L,
    reason_min_n = 1000L,
    type_min_n = 500L,
    chains = 2L,
    parallel_chains = 2L,
    iter_warmup = 1000L,
    iter_sampling = 1000L,
    adapt_delta = 0.95,
    max_treedepth = 11L,
    refresh = 50L,
    save_tables = 1L,
    save_plots = 1L,
    save_report = 1L,
    save_fit_object = 0L
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

  int_keys <- c(
    "cutoff_original_year", "publisher_min_n", "subject_min_n", "reason_min_n", "type_min_n",
    "chains", "parallel_chains", "iter_warmup", "iter_sampling", "max_treedepth",
    "refresh", "save_tables", "save_plots", "save_report", "save_fit_object"
  )
  num_keys <- c("adapt_delta")

  for (k in int_keys) out[[k]] <- as.integer(out[[k]])
  for (k in num_keys) out[[k]] <- as.numeric(out[[k]])

  out
}

require_pkgs <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Pacotes faltando: ", paste(missing, collapse = ", "),
      "\nRode primeiro: Rscript scripts/_setup/install_deps.R --all",
      call. = FALSE
    )
  }
}

check_cmdstan <- function() {
  ok <- TRUE
  tryCatch(cmdstanr::cmdstan_version(), error = function(e) ok <<- FALSE)
  if (!ok) {
    stop(
      "CmdStan nao encontrado/configurado.\n",
      "Rode: Rscript scripts/_setup/install_cmdstan.R\n",
      "Depois tente novamente.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

write_txt <- function(lines, path) {
  con <- file(path, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con = con, useBytes = TRUE)
}

first_token <- function(x) {
  x <- as.character(x)
  x <- ifelse(is.na(x), NA_character_, x)
  x <- stringr::str_replace_all(x, "\\s+", " ")
  x <- stringr::str_trim(x)
  x <- stringr::str_replace(x, ";+$", "")
  out <- stringr::str_extract(x, "^[^;]+")
  out <- stringr::str_trim(out)
  out[out == ""] <- NA_character_
  out
}

count_tokens <- function(x) {
  x <- as.character(x)
  x <- ifelse(is.na(x), NA_character_, x)
  x <- stringr::str_replace_all(x, "\\s+", " ")
  x <- stringr::str_trim(x)
  x <- stringr::str_replace(x, ";+$", "")
  parts <- stringr::str_split(x, ";")
  vapply(parts, function(z) {
    z <- stringr::str_trim(z)
    z <- z[z != ""]
    if (length(z) == 0) 0L else length(z)
  }, integer(1))
}

fmt_num <- function(x, digits = 2) {
  format(round(x, digits), big.mark = ".", decimal.mark = ",", nsmall = digits)
}

summ_num <- function(x) {
  tibble::tibble(
    n = sum(!is.na(x)),
    media = mean(x, na.rm = TRUE),
    mediana = stats::median(x, na.rm = TRUE),
    p10 = stats::quantile(x, 0.10, na.rm = TRUE),
    p90 = stats::quantile(x, 0.90, na.rm = TRUE),
    p95 = stats::quantile(x, 0.95, na.rm = TRUE),
    p99 = stats::quantile(x, 0.99, na.rm = TRUE),
    maximo = max(x, na.rm = TRUE)
  )
}

relevel_other_first <- function(x) {
  x <- factor(x)
  if ("Other" %in% levels(x)) x <- forcats::fct_relevel(x, "Other")
  x
}

opt <- parse_args(commandArgs(trailingOnly = TRUE))

pkgs <- c(
  "readr", "dplyr", "stringr", "tidyr", "forcats", "lubridate",
  "cmdstanr", "posterior", "ggplot2", "tibble"
)
require_pkgs(pkgs)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(forcats)
  library(lubridate)
  library(cmdstanr)
  library(posterior)
  library(ggplot2)
  library(tibble)
})

check_cmdstan()

if (!file.exists(opt$input_csv)) {
  stop(
    "Arquivo CSV nao encontrado em: ", opt$input_csv,
    "\nBaixe a base externa indicada em data/raw/retractions_time_to_retraction/README.md ",
    "e salve o arquivo com esse nome, ou informe --input_csv=...",
    call. = FALSE
  )
}

if (opt$save_tables == 1L || opt$save_report == 1L) ensure_dir(opt$tables_dir)
if (opt$save_plots == 1L) ensure_dir(opt$figures_dir)
if (opt$save_fit_object == 1L) ensure_dir(opt$models_dir)

cat("\n================ BASE E CONFIGURACAO ================\n")
cat("CSV de entrada        :", opt$input_csv, "\n")
cat("cutoff_original_year  :", opt$cutoff_original_year, "\n")
cat("publisher_min_n       :", opt$publisher_min_n, "\n")
cat("subject_min_n         :", opt$subject_min_n, "\n")
cat("reason_min_n          :", opt$reason_min_n, "\n")
cat("type_min_n            :", opt$type_min_n, "\n")
cat("chains                :", opt$chains, "\n")
cat("parallel_chains       :", opt$parallel_chains, "\n")
cat("iter_warmup           :", opt$iter_warmup, "\n")
cat("iter_sampling         :", opt$iter_sampling, "\n")
cat("adapt_delta           :", opt$adapt_delta, "\n")
cat("max_treedepth         :", opt$max_treedepth, "\n")
cat("save_tables           :", opt$save_tables, "\n")
cat("save_plots            :", opt$save_plots, "\n")
cat("save_report           :", opt$save_report, "\n")
cat("save_fit_object       :", opt$save_fit_object, "\n")

raw <- readr::read_csv(opt$input_csv, show_col_types = FALSE, progress = FALSE)

expected_cols <- c(
  "record_id", "title", "journal", "publisher", "country", "author", "subject",
  "articletype", "reason", "retractiondate", "retraction_year", "retractiondoi",
  "retractionpubmedid", "originalpaperdate", "original_year", "originalpaperdoi",
  "originalpaperpubmedid", "years_to_retraction", "institution", "author_count",
  "reason_count", "subject_count", "title_len", "title_word_count",
  "has_retraction_doi", "has_original_doi", "row_quality_flag"
)

missing_cols <- setdiff(expected_cols, names(raw))
if (length(missing_cols) > 0) {
  stop("Colunas esperadas ausentes: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

df <- raw %>%
  mutate(
    retraction_date = mdy(retractiondate),
    original_date   = mdy(originalpaperdate)
  ) %>%
  mutate(
    lag_days_calc  = as.numeric(retraction_date - original_date),
    lag_years_calc = lag_days_calc / 365.25
  )

n_bad_retraction_date <- sum(is.na(df$retraction_date))
n_bad_original_date   <- sum(is.na(df$original_date))
n_negative_lag        <- sum(df$lag_days_calc < 0, na.rm = TRUE)
n_zero_lag            <- sum(df$lag_days_calc == 0, na.rm = TRUE)
n_bad_quality_flag    <- sum(df$row_quality_flag != "OK", na.rm = TRUE)

if (n_bad_retraction_date > 0 || n_bad_original_date > 0) {
  stop(
    "Falha ao converter datas. ",
    "retraction_date NA: ", n_bad_retraction_date, "; ",
    "original_date NA: ", n_bad_original_date,
    call. = FALSE
  )
}

if (n_negative_lag > 0) {
  stop("Existem ", n_negative_lag, " casos com lag negativo. Verifique a base.", call. = FALSE)
}

base_analitica <- df %>%
  mutate(
    publisher_primary    = first_token(publisher),
    country_primary      = first_token(country),
    subject_primary      = first_token(subject),
    reason_primary       = first_token(reason),
    article_type_primary = first_token(articletype),
    n_country_tokens      = count_tokens(country),
    n_subject_tokens      = count_tokens(subject),
    n_reason_tokens       = count_tokens(reason),
    n_article_type_tokens = count_tokens(articletype),
    is_multi_country = as.integer(n_country_tokens > 1),
    is_multi_subject = as.integer(n_subject_tokens > 1),
    is_multi_reason  = as.integer(n_reason_tokens > 1),
    lag_days     = lag_days_calc,
    lag_years    = lag_years_calc,
    lag_days_adj = lag_days + 1,
    log_lag_days = log(lag_days_adj)
  ) %>%
  filter(
    !is.na(original_year),
    !is.na(retraction_year),
    original_year <= opt$cutoff_original_year,
    !is.na(log_lag_days),
    is.finite(log_lag_days),
    !is.na(publisher_primary),
    !is.na(subject_primary),
    !is.na(reason_primary),
    !is.na(article_type_primary)
  )

if (nrow(base_analitica) == 0) {
  stop("A base analitica ficou vazia apos os filtros.", call. = FALSE)
}

base_analitica <- base_analitica %>%
  mutate(
    publisher_grp = fct_lump_min(factor(publisher_primary), min = opt$publisher_min_n, other_level = "Other") |> as.character(),
    subject_grp = fct_lump_min(factor(subject_primary), min = opt$subject_min_n, other_level = "Other") |> as.character(),
    reason_grp = fct_lump_min(factor(reason_primary), min = opt$reason_min_n, other_level = "Other") |> as.character(),
    article_type_grp = fct_lump_min(factor(article_type_primary), min = opt$type_min_n, other_level = "Other") |> as.character()
  )

base_analitica <- base_analitica %>%
  mutate(
    publisher_grp = relevel_other_first(publisher_grp),
    subject_grp = relevel_other_first(subject_grp),
    reason_grp = relevel_other_first(reason_grp),
    article_type_grp = relevel_other_first(article_type_grp)
  ) %>%
  mutate(
    publisher_id = as.integer(publisher_grp)
  )

publisher_levels <- levels(base_analitica$publisher_grp)
subject_levels   <- levels(base_analitica$subject_grp)
reason_levels    <- levels(base_analitica$reason_grp)
type_levels      <- levels(base_analitica$article_type_grp)

base_analitica <- base_analitica %>%
  mutate(log_lag_days = as.numeric(log_lag_days))

y_mean <- mean(base_analitica$log_lag_days)
y_sd   <- stats::sd(base_analitica$log_lag_days)

if (!is.finite(y_mean) || !is.finite(y_sd) || y_sd <= 0) {
  stop("Falha na padronizacao de log_lag_days: media/DP invalidos.", call. = FALSE)
}

base_analitica <- base_analitica %>%
  mutate(y_std = (log_lag_days - y_mean) / y_sd)

if (any(!is.finite(base_analitica$y_std))) {
  stop("Existem valores nao finitos em y_std apos a padronizacao.", call. = FALSE)
}

X_mm <- model.matrix(~ reason_grp + subject_grp + article_type_grp, data = base_analitica)
X <- X_mm[, colnames(X_mm) != "(Intercept)", drop = FALSE]
X <- unname(X)
x_colnames <- colnames(X_mm)[colnames(X_mm) != "(Intercept)"]

if (!is.matrix(X) || nrow(X) != nrow(base_analitica)) {
  stop("Falha na construcao da matriz de covariaveis X.", call. = FALSE)
}
if (any(!is.finite(X))) {
  stop("A matriz X contem valores nao finitos.", call. = FALSE)
}

lag_summary <- summ_num(base_analitica$lag_years)
top_publishers <- base_analitica %>% count(publisher_grp, sort = TRUE) %>% slice_head(n = 10)
top_reasons    <- base_analitica %>% count(reason_grp, sort = TRUE) %>% slice_head(n = 10)
top_subjects   <- base_analitica %>% count(subject_grp, sort = TRUE) %>% slice_head(n = 10)
by_type <- base_analitica %>%
  group_by(article_type_grp) %>%
  summarise(
    n = dplyr::n(),
    media_anos = mean(lag_years, na.rm = TRUE),
    mediana_anos = stats::median(lag_years, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

cat("\n================ RELATORIO DESCRITIVO ================\n")
cat("Registros originais             :", nrow(df), "\n")
cat("Registros na base analitica     :", nrow(base_analitica), "\n")
cat("Filtro original_year <=         :", opt$cutoff_original_year, "\n")
cat("Datas invalidas (retratacao)    :", n_bad_retraction_date, "\n")
cat("Datas invalidas (original)      :", n_bad_original_date, "\n")
cat("Lags negativos                  :", n_negative_lag, "\n")
cat("Lags zero                       :", n_zero_lag, "\n")
cat("row_quality_flag != OK          :", n_bad_quality_flag, "\n")
cat("Padronizacao log_lag_days media :", fmt_num(y_mean, 3), "\n")
cat("Padronizacao log_lag_days DP    :", fmt_num(y_sd, 3), "\n")
cat("Editoras (efeito aleatorio)     :", length(publisher_levels), "\n")
cat("Niveis de subject               :", length(subject_levels), "\n")
cat("Niveis de reason                :", length(reason_levels), "\n")
cat("Niveis de article_type          :", length(type_levels), "\n")
cat("Covariaveis fixas (K)           :", ncol(X), "\n")
cat("Tamanho de X (MB)               :", fmt_num(as.numeric(object.size(X)) / 1024^2, 2), "\n")

print(lag_summary)
print(top_publishers)
print(top_reasons)
print(top_subjects)
print(by_type)

if (opt$save_tables == 1L) {
  readr::write_csv(lag_summary, file.path(opt$tables_dir, "lag_summary.csv"))
  readr::write_csv(top_publishers, file.path(opt$tables_dir, "top_publishers.csv"))
  readr::write_csv(top_reasons, file.path(opt$tables_dir, "top_reasons.csv"))
  readr::write_csv(top_subjects, file.path(opt$tables_dir, "top_subjects.csv"))
  readr::write_csv(by_type, file.path(opt$tables_dir, "lag_by_article_type.csv"))
}

standata <- list(
  N = nrow(base_analitica),
  K = ncol(X),
  X = X,
  y = as.vector(base_analitica$y_std),
  y_mean = y_mean,
  y_sd = y_sd,
  J_pub = length(publisher_levels),
  pub = as.array(base_analitica$publisher_id),
  is_multi_reason = as.array(base_analitica$is_multi_reason),
  is_multi_subject = as.array(base_analitica$is_multi_subject),
  is_multi_country = as.array(base_analitica$is_multi_country)
)

cat("\n[STANDATA]\n")
cat("Elementos da lista :", length(standata), "\n")
cat("N                  :", standata$N, "\n")
cat("K                  :", standata$K, "\n")
cat("J_pub              :", standata$J_pub, "\n")
cat("Tamanho em MB      :", fmt_num(as.numeric(object.size(standata)) / 1024^2, 2), "\n")

stan_code <- '
data {
  int<lower=1> N;
  int<lower=0> K;
  matrix[N, K] X;
  vector[N] y;
  real y_mean;
  real<lower=0> y_sd;
  int<lower=1> J_pub;
  array[N] int<lower=1, upper=J_pub> pub;
  array[N] int<lower=0, upper=1> is_multi_reason;
  array[N] int<lower=0, upper=1> is_multi_subject;
  array[N] int<lower=0, upper=1> is_multi_country;
}
parameters {
  real alpha;
  vector[K] beta;
  real beta_multi_reason;
  real beta_multi_subject;
  real beta_multi_country;
  real<lower=1e-4, upper=2> sigma;
  real<lower=1e-4, upper=1> sigma_pub;
  vector[J_pub] z_pub;
}
transformed parameters {
  vector[J_pub] a_pub = sigma_pub * z_pub;
}
model {
  vector[N] mu;

  alpha ~ normal(0, 0.35);
  beta ~ normal(0, 0.20);
  beta_multi_reason ~ normal(0, 0.15);
  beta_multi_subject ~ normal(0, 0.15);
  beta_multi_country ~ normal(0, 0.15);
  sigma ~ lognormal(log(0.6), 0.25);
  sigma_pub ~ lognormal(log(0.10), 0.35);
  z_pub ~ std_normal();

  mu = alpha + X * beta
       + beta_multi_reason * to_vector(is_multi_reason)
       + beta_multi_subject * to_vector(is_multi_subject)
       + beta_multi_country * to_vector(is_multi_country);

  for (n in 1:N) {
    mu[n] += a_pub[pub[n]];
  }

  y ~ normal(mu, sigma);
}
generated quantities {
  real y_rep_log_mean;
  real y_rep_log_sd;
  {
    vector[N] y_rep_log_tmp;
    vector[N] mu;

    mu = alpha + X * beta
         + beta_multi_reason * to_vector(is_multi_reason)
         + beta_multi_subject * to_vector(is_multi_subject)
         + beta_multi_country * to_vector(is_multi_country);

    for (n in 1:N) {
      mu[n] += a_pub[pub[n]];
      y_rep_log_tmp[n] = y_mean + y_sd * normal_rng(mu[n], sigma);
    }

    y_rep_log_mean = mean(y_rep_log_tmp);
    y_rep_log_sd = sd(y_rep_log_tmp);
  }
}
'

stan_file <- cmdstanr::write_stan_file(stan_code, dir = tempdir())
mod <- cmdstanr::cmdstan_model(stan_file, quiet = TRUE)

cat("\n[AJUSTE STAN]\n")
cat(
  "Config: chains=", opt$chains,
  ", warmup=", opt$iter_warmup,
  ", sampling=", opt$iter_sampling,
  ", adapt_delta=", opt$adapt_delta,
  ", max_treedepth=", opt$max_treedepth, "\n",
  sep = ""
)

fit <- mod$sample(
  data = standata,
  seed = 42,
  init = function() list(
    alpha = 0,
    beta = rep(0, standata$K),
    beta_multi_reason = 0,
    beta_multi_subject = 0,
    beta_multi_country = 0,
    sigma = 0.70,
    sigma_pub = 0.10,
    z_pub = rep(0, standata$J_pub)
  ),
  chains = opt$chains,
  parallel_chains = opt$parallel_chains,
  iter_warmup = opt$iter_warmup,
  iter_sampling = opt$iter_sampling,
  refresh = opt$refresh,
  adapt_delta = opt$adapt_delta,
  max_treedepth = opt$max_treedepth
)

if (opt$save_fit_object == 1L) {
  saveRDS(fit, file.path(opt$models_dir, "retractions_time_to_retraction_fit_cmdstanr.rds"))
}

parametros_principais <- c(
  "alpha", "beta_multi_reason", "beta_multi_subject", "beta_multi_country",
  "sigma", "sigma_pub"
)

diag_core <- fit$summary(variables = parametros_principais)
diag_full <- fit$summary()
diag_sampler <- fit$diagnostic_summary()

bad_rhat <- diag_full %>% filter(!is.na(rhat), rhat > 1.01)
low_ess  <- diag_full %>% filter(!is.na(ess_bulk), ess_bulk < 400)

cat("\n================ DIAGNOSTICO DO MODELO ================\n")
print(diag_core)
cat("\n[DIVERGENCIAS E ALERTAS DO SAMPLER]\n")
print(diag_sampler)

if (nrow(bad_rhat) > 0) {
  cat("\n[ATENCAO] Existem parametros com Rhat > 1,01\n")
  print(bad_rhat %>% arrange(desc(rhat)) %>% slice_head(n = 20))
} else {
  cat("\nRhat: todos os parametros monitorados ficaram <= 1,01\n")
}

if (nrow(low_ess) > 0) {
  cat("\n[ATENCAO] Existem parametros com ESS bulk < 400\n")
  print(low_ess %>% arrange(ess_bulk) %>% slice_head(n = 20))
} else {
  cat("\nESS bulk: todos os parametros monitorados ficaram >= 400\n")
}

if (opt$save_tables == 1L) {
  readr::write_csv(diag_core, file.path(opt$tables_dir, "diagnostico_parametros_principais.csv"))
  readr::write_csv(diag_full, file.path(opt$tables_dir, "diagnostico_parametros_todos.csv"))
  readr::write_csv(as_tibble(diag_sampler), file.path(opt$tables_dir, "diagnostico_sampler.csv"))
}

draws <- posterior::as_draws_df(fit$draws())

alpha_log <- y_mean + y_sd * draws$alpha
alpha_days <- exp(alpha_log) - 1

beta_flags <- tibble(
  parametro = c("beta_multi_reason", "beta_multi_subject", "beta_multi_country"),
  media = c(mean(draws$beta_multi_reason), mean(draws$beta_multi_subject), mean(draws$beta_multi_country)),
  q05   = c(stats::quantile(draws$beta_multi_reason, 0.05), stats::quantile(draws$beta_multi_subject, 0.05), stats::quantile(draws$beta_multi_country, 0.05)),
  q50   = c(stats::quantile(draws$beta_multi_reason, 0.50), stats::quantile(draws$beta_multi_subject, 0.50), stats::quantile(draws$beta_multi_country, 0.50)),
  q95   = c(stats::quantile(draws$beta_multi_reason, 0.95), stats::quantile(draws$beta_multi_subject, 0.95), stats::quantile(draws$beta_multi_country, 0.95))
) %>%
  mutate(
    fator_media_lag = exp(y_sd * media),
    fator_q05_lag = exp(y_sd * q05),
    fator_q50_lag = exp(y_sd * q50),
    fator_q95_lag = exp(y_sd * q95)
  )

pub_effects <- lapply(seq_along(publisher_levels), function(i) {
  nm <- paste0("a_pub[", i, "]")
  mu_log_days <- y_mean + y_sd * (draws$alpha + draws[[nm]])
  mu_days <- exp(mu_log_days) - 1
  tibble(
    grupo = publisher_levels[i],
    media_dias = mean(mu_days),
    q05_dias = stats::quantile(mu_days, 0.05),
    q50_dias = stats::quantile(mu_days, 0.50),
    q95_dias = stats::quantile(mu_days, 0.95)
  )
}) |> bind_rows() |> arrange(q50_dias)

coef_fixos <- lapply(seq_along(x_colnames), function(i) {
  nm <- paste0("beta[", i, "]")
  tibble(
    covariavel = x_colnames[i],
    media = mean(draws[[nm]]),
    q05 = stats::quantile(draws[[nm]], 0.05),
    q50 = stats::quantile(draws[[nm]], 0.50),
    q95 = stats::quantile(draws[[nm]], 0.95)
  )
}) |> bind_rows() %>%
  mutate(
    fator_media_lag = exp(y_sd * media),
    fator_q05_lag = exp(y_sd * q05),
    fator_q50_lag = exp(y_sd * q50),
    fator_q95_lag = exp(y_sd * q95),
    abs_media = abs(media)
  ) %>%
  arrange(desc(abs_media))

coef_fixos_top <- bind_rows(
  coef_fixos %>% arrange(desc(q50)) %>% slice_head(n = 10),
  coef_fixos %>% arrange(q50) %>% slice_head(n = 10)
) %>% distinct(covariavel, .keep_all = TRUE)

ppc <- fit$summary(variables = c("y_rep_log_mean", "y_rep_log_sd"))
obs_log_mean <- mean(base_analitica$log_lag_days)
obs_log_sd   <- stats::sd(base_analitica$log_lag_days)

cat("\n================ RESULTADOS PRINCIPAIS ================\n")
cat("Lag tipico posterior (dias), cenario-base/referencia:\n")
cat(
  "media = ", fmt_num(mean(alpha_days), 1),
  "; q05 = ", fmt_num(stats::quantile(alpha_days, 0.05), 1),
  "; mediana = ", fmt_num(stats::quantile(alpha_days, 0.50), 1),
  "; q95 = ", fmt_num(stats::quantile(alpha_days, 0.95), 1), "\n", sep = ""
)
cat("\n[EFEITOS DAS FLAGS DE MULTIPLICIDADE]\n")
print(beta_flags)
cat("\n[TOP 15 EFEITOS FIXOS EM VALOR ABSOLUTO]\n")
print(coef_fixos %>% slice_head(n = 15))
cat("\n[EDITORAS COM MENOR LAG POSTERIOR — TOP 10]\n")
print(pub_effects %>% slice_head(n = 10))
cat("\n[EDITORAS COM MAIOR LAG POSTERIOR — TOP 10]\n")
print(pub_effects %>% slice_tail(n = 10))
cat("\n================ CHECAGEM PREDITIVA POSTERIOR ================\n")
cat(
  "Observado - media(log lag dias): ", fmt_num(obs_log_mean, 3),
  "; dp(log lag dias): ", fmt_num(obs_log_sd, 3), "\n", sep = ""
)
cat(
  "Posterior - media(log lag dias): ", fmt_num(ppc$mean[ppc$variable == "y_rep_log_mean"], 3),
  "; dp(log lag dias): ", fmt_num(ppc$mean[ppc$variable == "y_rep_log_sd"], 3), "\n", sep = ""
)

if (opt$save_tables == 1L) {
  readr::write_csv(beta_flags, file.path(opt$tables_dir, "efeitos_flags_multiplos.csv"))
  readr::write_csv(pub_effects, file.path(opt$tables_dir, "efeitos_editoras.csv"))
  readr::write_csv(coef_fixos, file.path(opt$tables_dir, "efeitos_fixos.csv"))
  readr::write_csv(coef_fixos_top, file.path(opt$tables_dir, "efeitos_fixos_top.csv"))
  readr::write_csv(ppc, file.path(opt$tables_dir, "posterior_predictive_check.csv"))
}

if (opt$save_plots == 1L) {
  p_lag <- ggplot(base_analitica, aes(x = lag_years)) +
    geom_histogram(bins = 60) +
    labs(
      title = "Distribuicao do tempo ate retratacao",
      x = "Anos ate retratacao",
      y = "Frequencia"
    ) +
    theme_minimal(base_size = 11)

  ggplot2::ggsave(
    filename = file.path(opt$figures_dir, "grafico_distribuicao_lag_anos.png"),
    plot = p_lag,
    width = 9,
    height = 5,
    dpi = 150
  )

  plot_fixos <- coef_fixos_top %>%
    arrange(q50) %>%
    mutate(covariavel = factor(covariavel, levels = covariavel)) %>%
    ggplot(aes(x = q50, y = covariavel)) +
    geom_vline(xintercept = 0, linewidth = 0.4, linetype = 2) +
    geom_errorbarh(aes(xmin = q05, xmax = q95), height = 0.2) +
    geom_point() +
    labs(
      title = "Efeitos fixos mais extremos",
      x = "Efeito posterior mediano (escala padronizada)",
      y = NULL
    ) +
    theme_minimal(base_size = 11)

  ggplot2::ggsave(
    filename = file.path(opt$figures_dir, "grafico_efeitos_fixos_top.png"),
    plot = plot_fixos,
    width = 9,
    height = 6,
    dpi = 150
  )

  plot_editoras_df <- bind_rows(
    pub_effects %>% slice_head(n = 10),
    pub_effects %>% slice_tail(n = 10)
  ) %>%
    distinct(grupo, .keep_all = TRUE) %>%
    arrange(q50_dias) %>%
    mutate(grupo = factor(grupo, levels = grupo))

  plot_editoras <- plot_editoras_df %>%
    ggplot(aes(x = q50_dias, y = grupo)) +
    geom_errorbarh(aes(xmin = q05_dias, xmax = q95_dias), height = 0.2) +
    geom_point() +
    labs(
      title = "Editoras com menor e maior lag posterior",
      x = "Dias ate retratacao (posterior mediano)",
      y = NULL
    ) +
    theme_minimal(base_size = 11)

  ggplot2::ggsave(
    filename = file.path(opt$figures_dir, "grafico_efeitos_editoras.png"),
    plot = plot_editoras,
    width = 9,
    height = 6,
    dpi = 150
  )
}

report_lines <- c(
  "TEMPO ATE RETRATACAO DE ARTIGOS CIENTIFICOS",
  paste0("Data/hora de execucao: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("CSV utilizado: ", opt$input_csv),
  "Fonte da base bruta: Kaggle — Global Scientific Retractions 1927–2026",
  "URL documentada: https://www.kaggle.com/datasets/kanchana1990/global-scientific-retractions-19272026",
  paste0("N analitico: ", standata$N),
  paste0("K: ", standata$K),
  paste0("J_pub: ", standata$J_pub),
  paste0("Standata (MB): ", fmt_num(as.numeric(object.size(standata)) / 1024^2, 2)),
  paste0(
    "Config Stan: chains=", opt$chains,
    ", warmup=", opt$iter_warmup,
    ", sampling=", opt$iter_sampling,
    ", adapt_delta=", opt$adapt_delta,
    ", max_treedepth=", opt$max_treedepth
  ),
  "",
  "Lag tipico posterior no cenario-base (dias):",
  paste0("  media = ", fmt_num(mean(alpha_days), 1)),
  paste0("  q05   = ", fmt_num(stats::quantile(alpha_days, 0.05), 1)),
  paste0("  q50   = ", fmt_num(stats::quantile(alpha_days, 0.50), 1)),
  paste0("  q95   = ", fmt_num(stats::quantile(alpha_days, 0.95), 1)),
  "",
  "Checagem preditiva agregada:",
  paste0("  observado mean(log lag) = ", fmt_num(obs_log_mean, 3)),
  paste0("  observado sd(log lag)   = ", fmt_num(obs_log_sd, 3)),
  paste0("  posterior mean(log lag) = ", fmt_num(ppc$mean[ppc$variable == "y_rep_log_mean"], 3)),
  paste0("  posterior sd(log lag)   = ", fmt_num(ppc$mean[ppc$variable == "y_rep_log_sd"], 3)),
  "",
  "Arquivos exportados (quando habilitados):",
  "  - outputs/tables/retractions_time_to_retraction/*.csv",
  "  - outputs/tables/retractions_time_to_retraction/retractions_time_to_retraction_report.txt",
  "  - outputs/figures/retractions_time_to_retraction/*.png",
  "  - outputs/models/retractions_time_to_retraction/*.rds"
)

if (opt$save_report == 1L) {
  write_txt(report_lines, file.path(opt$tables_dir, "retractions_time_to_retraction_report.txt"))
}

cat("\nConcluido.\n")
if (opt$save_tables == 1L || opt$save_report == 1L) cat("Tabelas/relatorio :", opt$tables_dir, "\n")
if (opt$save_plots == 1L) cat("Figuras           :", opt$figures_dir, "\n")
if (opt$save_fit_object == 1L) cat("Modelo salvo      :", opt$models_dir, "\n")
