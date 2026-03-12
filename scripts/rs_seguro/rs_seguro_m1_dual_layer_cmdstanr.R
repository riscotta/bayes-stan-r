#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

############################################################
# RS Seguro — M1: série mensal por crime / macrocrime
# Stan via cmdstanr (NegBin hierárquico com efeito mensal)
#
# Padrão do repo:
# - rodar a partir do root do repositório
# - sem setwd()
# - entradas em data/raw/
# - saídas regeneráveis em outputs/
#
# Exemplos:
#   Rscript scripts/rs_seguro/rs_seguro_m1_dual_layer_cmdstanr.R
#   Rscript scripts/rs_seguro/rs_seguro_m1_dual_layer_cmdstanr.R --analysis_layer=macrocrime
#   Rscript scripts/rs_seguro/rs_seguro_m1_dual_layer_cmdstanr.R --input_csv=data/raw/rs_seguro/rs_month_macrocrime.csv --save_outputs=1
############################################################

args <- commandArgs(trailingOnly = TRUE)
AUTO_INSTALL_PKGS <- "--auto-install" %in% args

parse_args <- function(args) {
  out <- list(
    analysis_layer = "macrocrime",
    input_csv = "",
    data_version_id = "",

    seed = 20260310L,
    chains = 4L,
    parallel_chains = 4L,
    iter_warmup = 1000L,
    iter_sampling = 1000L,
    adapt_delta = 0.99,
    max_treedepth = 12L,
    refresh = 100L,
    keep_temp_files = 0L,

    save_outputs = if (interactive()) 0L else 1L,
    save_plots   = if (interactive()) 0L else 1L,
    output_tables_dir  = file.path("outputs", "tables", "rs_seguro"),
    output_figures_dir = file.path("outputs", "figures", "rs_seguro"),
    output_prefix = ""
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

  out$analysis_layer <- as.character(out$analysis_layer)
  out$seed <- as.integer(out$seed)
  out$chains <- as.integer(out$chains)
  out$parallel_chains <- as.integer(out$parallel_chains)
  out$iter_warmup <- as.integer(out$iter_warmup)
  out$iter_sampling <- as.integer(out$iter_sampling)
  out$adapt_delta <- as.numeric(out$adapt_delta)
  out$max_treedepth <- as.integer(out$max_treedepth)
  out$refresh <- as.integer(out$refresh)
  out$keep_temp_files <- as.integer(out$keep_temp_files)
  out$save_outputs <- as.integer(out$save_outputs)
  out$save_plots <- as.integer(out$save_plots)

  stopifnot(out$analysis_layer %in% c("crime", "macrocrime"))

  if (!nzchar(out$input_csv)) {
    out$input_csv <- file.path(
      "data", "raw", "rs_seguro",
      ifelse(out$analysis_layer == "macrocrime", "rs_month_macrocrime.csv", "rs_month_crime.csv")
    )
  }

  if (!nzchar(out$data_version_id)) {
    out$data_version_id <- paste0(
      "rs-seguro-dados-", format(Sys.Date(), "%Y-%m-%d"),
      "-occ-", out$analysis_layer, "-m1"
    )
  }

  if (!nzchar(out$output_prefix)) {
    out$output_prefix <- paste0("rs_seguro_m1_", out$analysis_layer)
  }

  out
}

opts <- parse_args(args)
series_label <- ifelse(opts$analysis_layer == "macrocrime", "macrocrime", "crime")

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
        "  Rscript scripts/_setup/install_deps.R\n\n",
        "Alternativa (manual, dentro do R):\n",
        "  install.packages('", pkg, "')\n\n",
        "Se você realmente quer auto-instalar neste script:\n",
        "  Rscript scripts/rs_seguro/rs_seguro_m1_dual_layer_cmdstanr.R --auto-install\n",
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
  "cmdstanr", "posterior", "bayesplot", "ggplot2",
  "dplyr", "readr", "tidyr", "stringr", "tibble"
)
invisible(lapply(pkgs, ensure_pkg))

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(bayesplot)
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(tibble)
})

stan_code <- '
data {
  int<lower=1> N;
  int<lower=1> C;
  int<lower=1> M;
  array[N] int<lower=1, upper=C> crime_id;
  array[N] int<lower=1, upper=M> month_id;
  vector[N] t_std;
  array[N] int<lower=0> y;
  real log_y_mean;
}

parameters {
  real alpha_global;
  real beta_global;

  real<lower=0> sigma_alpha_crime;
  real<lower=0> sigma_beta_crime;
  real<lower=0> sigma_gamma_month;
  real<lower=0> sigma_log_phi;

  vector[C] z_alpha_crime;
  vector[C] z_beta_crime;
  vector[M] z_gamma_month_raw;
  vector[C] z_log_phi;

  real<lower=-6, upper=6> log_phi_global;
}

transformed parameters {
  vector[C] alpha_crime;
  vector[C] beta_crime;
  vector[M] gamma_month;
  vector[C] log_phi_crime;
  vector<lower=1e-6, upper=3000>[C] phi_crime;
  vector[M] gamma_month_raw;
  vector[N] eta;

  alpha_crime = sigma_alpha_crime * z_alpha_crime;
  beta_crime  = sigma_beta_crime  * z_beta_crime;

  gamma_month_raw = sigma_gamma_month * z_gamma_month_raw;
  gamma_month = gamma_month_raw - mean(gamma_month_raw);

  for (c in 1:C) {
    real log_phi_raw;
    log_phi_raw = log_phi_global + sigma_log_phi * z_log_phi[c];
    log_phi_crime[c] = fmin(8.0, fmax(-8.0, log_phi_raw));
    phi_crime[c] = exp(log_phi_crime[c]);
  }

  for (n in 1:N) {
    real eta_raw;
    eta_raw = alpha_global
            + alpha_crime[crime_id[n]]
            + (beta_global + beta_crime[crime_id[n]]) * t_std[n]
            + gamma_month[month_id[n]];
    eta[n] = fmin(10.0, fmax(-10.0, eta_raw));
  }
}

model {
  alpha_global ~ normal(log_y_mean, 0.5);
  beta_global  ~ normal(0.0, 0.05);

  sigma_alpha_crime ~ normal(0, 0.25);
  sigma_beta_crime  ~ normal(0, 0.03);
  sigma_gamma_month ~ normal(0, 0.15);
  sigma_log_phi     ~ normal(0, 0.10);

  z_alpha_crime     ~ std_normal();
  z_beta_crime      ~ std_normal();
  z_gamma_month_raw ~ std_normal();
  z_log_phi         ~ std_normal();

  log_phi_global ~ normal(log(10), 0.3);

  y ~ neg_binomial_2_log(eta, phi_crime[crime_id]);
}

generated quantities {
  vector[N] log_lik;
  array[N] int y_rep;
  vector[C] beta_total_crime;

  for (c in 1:C) {
    beta_total_crime[c] = beta_global + beta_crime[c];
  }

  for (n in 1:N) {
    log_lik[n] = neg_binomial_2_log_lpmf(y[n] | eta[n], phi_crime[crime_id[n]]);
    y_rep[n]   = neg_binomial_2_log_rng(eta[n], phi_crime[crime_id[n]]);
  }
}
'

q_num <- function(x, probs) {
  as.numeric(stats::quantile(x, probs = probs, names = FALSE, type = 8, na.rm = TRUE))
}

safe_print <- function(x, n = 20) {
  if (inherits(x, c("tbl_df", "data.frame"))) {
    print(utils::head(x, n))
  } else {
    print(x)
  }
}

write_plot_pdf <- function(path, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  grDevices::pdf(path, width = 11, height = 8.5)
  on.exit(grDevices::dev.off(), add = TRUE)
  plots <- list(...)
  invisible(lapply(plots, print))
}

if (!file.exists(opts$input_csv)) {
  extra_hint <- if (identical(opts$analysis_layer, "crime") && identical(opts$input_csv, file.path("data", "raw", "rs_seguro", "rs_month_crime.csv"))) {
    paste0(
      "\n",
      "Observação: `data/raw/rs_seguro/rs_month_crime.csv` não está versionado no repo atual. ",
      "Forneça --input_csv=/caminho/para/rs_month_crime.csv ou adicione o arquivo localmente."
    )
  } else {
    ""
  }

  stop(
    "Arquivo de entrada não encontrado: ", opts$input_csv, "\n",
    "Ajuste --input_csv=... ou adicione o arquivo em data/raw/rs_seguro/.",
    extra_hint
  )
}

tmp_cmdstan_dir <- tempfile("m1_cmdstan_")
dir.create(tmp_cmdstan_dir, recursive = TRUE, showWarnings = FALSE)

if (!isTRUE(as.logical(opts$keep_temp_files))) {
  on.exit(unlink(tmp_cmdstan_dir, recursive = TRUE, force = TRUE), add = TRUE)
}

raw <- readr::read_csv(opts$input_csv, show_col_types = FALSE) %>%
  transmute(
    ym    = as.character(ym),
    crime = as.character(crime),
    occ   = as.integer(occ),
    vit   = as.integer(vit)
  )

required_cols <- c("ym", "crime", "occ", "vit")
missing_cols <- setdiff(required_cols, names(raw))
if (length(missing_cols) > 0) {
  stop("Faltam colunas no input: ", paste(missing_cols, collapse = ", "))
}

if (any(is.na(raw$ym)) || any(is.na(raw$crime)) || any(is.na(raw$occ))) {
  stop("Há NAs em ym/crime/occ. Corrija antes de ajustar o modelo.")
}
if (any(raw$occ < 0, na.rm = TRUE)) {
  stop("Há contagens negativas em occ.")
}

month_levels <- sort(unique(raw$ym))
crime_levels <- sort(unique(raw$crime))
expected_n <- length(month_levels) * length(crime_levels)
input_complete <- nrow(dplyr::distinct(raw, ym, crime)) == expected_n

panel <- raw %>%
  group_by(ym, crime) %>%
  summarise(
    occ = sum(occ, na.rm = TRUE),
    vit = sum(vit, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::complete(
    ym = month_levels,
    crime = crime_levels,
    fill = list(occ = 0L, vit = 0L)
  ) %>%
  mutate(
    date     = as.Date(paste0(ym, "-01")),
    t        = match(ym, month_levels),
    month_id = as.integer(format(date, "%m")),
    crime_id = match(crime, crime_levels)
  ) %>%
  arrange(t, crime_id) %>%
  mutate(t_std = as.numeric(scale(t)))

if (any(!is.finite(panel$t_std))) {
  stop("t_std contém valores não finitos. Verifique se há pelo menos 2 meses distintos.")
}

Tn <- length(month_levels)
Cn <- length(crime_levels)
panel_complete <- nrow(panel) == expected_n

snapshot <- panel %>%
  summarise(
    data_version_id = opts$data_version_id,
    analysis_layer = opts$analysis_layer,
    input_complete = input_complete,
    n_rows   = n(),
    n_months = n_distinct(ym),
    n_crimes = n_distinct(crime),
    ym_min   = min(ym),
    ym_max   = max(ym),
    occ_total = sum(occ),
    vit_total = sum(vit),
    occ_mean  = mean(occ),
    occ_var   = var(occ),
    zero_occ  = sum(occ == 0),
    panel_complete = panel_complete,
    expected_n = expected_n,
    observed_n = n()
  )

log_y_mean <- log(mean(panel$occ) + 0.1)

stan_data <- list(
  N          = nrow(panel),
  C          = Cn,
  M          = 12,
  crime_id   = panel$crime_id,
  month_id   = panel$month_id,
  t_std      = panel$t_std,
  y          = panel$occ,
  log_y_mean = log_y_mean
)

make_inits <- function(chain_id = 1) {
  list(
    alpha_global      = log_y_mean,
    beta_global       = 0,
    sigma_alpha_crime = 0.08,
    sigma_beta_crime  = 0.01,
    sigma_gamma_month = 0.02,
    sigma_log_phi     = 0.03,
    z_alpha_crime     = rep(0, Cn),
    z_beta_crime      = rep(0, Cn),
    z_gamma_month_raw = rep(0, 12),
    z_log_phi         = rep(0, Cn),
    log_phi_global    = log(10)
  )
}

stan_file_tmp <- cmdstanr::write_stan_file(stan_code, dir = tmp_cmdstan_dir)
mod <- cmdstanr::cmdstan_model(stan_file_tmp, quiet = TRUE)

fit <- mod$sample(
  data            = stan_data,
  seed            = opts$seed,
  chains          = opts$chains,
  parallel_chains = opts$parallel_chains,
  iter_warmup     = opts$iter_warmup,
  iter_sampling   = opts$iter_sampling,
  adapt_delta     = opts$adapt_delta,
  max_treedepth   = opts$max_treedepth,
  refresh         = opts$refresh,
  output_dir      = tmp_cmdstan_dir,
  init            = make_inits
)

diagn <- fit$diagnostic_summary() %>% as_tibble()
summ  <- fit$summary() %>% as_tibble()
draws_df <- fit$draws(format = "df") %>% as_tibble()

beta_global_draws <- draws_df$beta_global
overall_summary <- tibble(
  data_version_id = opts$data_version_id,
  analysis_layer = opts$analysis_layer,
  parameter = "beta_global",
  mean   = mean(beta_global_draws),
  median = median(beta_global_draws),
  q05    = q_num(beta_global_draws, 0.05),
  q50    = q_num(beta_global_draws, 0.50),
  q95    = q_num(beta_global_draws, 0.95),
  p_lt_0 = mean(beta_global_draws < 0),
  p_gt_0 = mean(beta_global_draws > 0)
)

beta_cols <- paste0("beta_total_crime[", seq_len(Cn), "]")
beta_mat <- as.matrix(draws_df[, beta_cols, drop = FALSE])
colnames(beta_mat) <- crime_levels

crime_trend <- tibble(
  crime  = crime_levels,
  mean   = apply(beta_mat, 2, mean),
  median = apply(beta_mat, 2, median),
  q05    = apply(beta_mat, 2, q_num, probs = 0.05),
  q50    = apply(beta_mat, 2, q_num, probs = 0.50),
  q95    = apply(beta_mat, 2, q_num, probs = 0.95),
  p_gt_0 = colMeans(beta_mat > 0),
  p_lt_0 = colMeans(beta_mat < 0)
) %>%
  mutate(
    classificacao = case_when(
      p_gt_0 > 0.90 ~ "aumentando",
      p_lt_0 > 0.90 ~ "diminuindo",
      TRUE ~ "incerto/estavel"
    )
  ) %>%
  arrange(desc(p_gt_0), desc(mean))

obs_total <- panel %>%
  group_by(ym, t) %>%
  summarise(y_obs_total = sum(occ), .groups = "drop") %>%
  arrange(t)

rep_cols <- grep("^y_rep\\[", names(draws_df), value = TRUE)
yrep_mat <- as.matrix(draws_df[, rep_cols, drop = FALSE])

month_index <- split(seq_len(nrow(panel)), panel$t)
rep_total_by_month <- sapply(month_index, function(idx) rowSums(yrep_mat[, idx, drop = FALSE]))
colnames(rep_total_by_month) <- month_levels

ppc_total <- tibble(
  ym = month_levels,
  y_obs_total = obs_total$y_obs_total,
  y_rep_mean = apply(rep_total_by_month, 2, mean),
  y_rep_q05  = apply(rep_total_by_month, 2, q_num, probs = 0.05),
  y_rep_q50  = apply(rep_total_by_month, 2, q_num, probs = 0.50),
  y_rep_q95  = apply(rep_total_by_month, 2, q_num, probs = 0.95)
)

obs_grand_total <- sum(panel$occ)
rep_grand_total <- rowSums(yrep_mat)
ppc_global_summary <- tibble(
  obs_grand_total = obs_grand_total,
  rep_mean = mean(rep_grand_total),
  rep_q05  = q_num(rep_grand_total, 0.05),
  rep_q50  = q_num(rep_grand_total, 0.50),
  rep_q95  = q_num(rep_grand_total, 0.95)
)

plot_trends <- crime_trend %>%
  mutate(crime = reorder(crime, mean)) %>%
  ggplot(aes(x = mean, y = crime, xmin = q05, xmax = q95, color = classificacao)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbarh(height = 0.2) +
  geom_point() +
  labs(
    title = paste0("M1 — Tendência posterior por ", series_label),
    subtitle = paste0("Inclinação total = beta_global + desvio por ", series_label),
    x = "Inclinação posterior (escala log por unidade de t padronizado)",
    y = NULL,
    color = "Classificação"
  ) +
  theme_minimal(base_size = 11)

plot_total <- ppc_total %>%
  ggplot(aes(x = ym, y = y_obs_total, group = 1)) +
  geom_ribbon(aes(ymin = y_rep_q05, ymax = y_rep_q95), alpha = 0.2) +
  geom_line(aes(y = y_rep_mean, group = 1), linetype = 2) +
  geom_line() +
  labs(
    title = "M1 — Série total observada vs posterior preditiva",
    x = NULL,
    y = "Ocorrências mensais (total no RS)"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

plot_ppc_global <- tibble(rep_grand_total = rep_grand_total) %>%
  ggplot(aes(x = rep_grand_total)) +
  geom_histogram(bins = 40) +
  geom_vline(xintercept = obs_grand_total, linetype = 2) +
  labs(
    title = "M1 — PPC do total global de ocorrências",
    subtitle = "Linha tracejada = total observado no período",
    x = "Total replicado no período",
    y = "Frequência posterior"
  ) +
  theme_minimal(base_size = 11)

results <- list(
  snapshot = snapshot,
  diagnostics = diagn,
  parameter_summary = summ,
  overall_summary = overall_summary,
  crime_trend = crime_trend,
  ppc_total = ppc_total,
  ppc_global_summary = ppc_global_summary,
  plots = list(
    plot_trends = plot_trends,
    plot_total = plot_total,
    plot_ppc_global = plot_ppc_global
  )
)

if (isTRUE(as.logical(opts$save_outputs))) {
  dir.create(opts$output_tables_dir, recursive = TRUE, showWarnings = FALSE)

  readr::write_csv(snapshot, file.path(opts$output_tables_dir, paste0(opts$output_prefix, "_snapshot.csv")))
  readr::write_csv(overall_summary, file.path(opts$output_tables_dir, paste0(opts$output_prefix, "_overall_summary.csv")))
  readr::write_csv(crime_trend, file.path(opts$output_tables_dir, paste0(opts$output_prefix, "_crime_trend.csv")))
  readr::write_csv(ppc_total, file.path(opts$output_tables_dir, paste0(opts$output_prefix, "_ppc_total.csv")))
  readr::write_csv(ppc_global_summary, file.path(opts$output_tables_dir, paste0(opts$output_prefix, "_ppc_global_summary.csv")))
  readr::write_csv(diagn, file.path(opts$output_tables_dir, paste0(opts$output_prefix, "_diagnostics.csv")))
  readr::write_csv(summ, file.path(opts$output_tables_dir, paste0(opts$output_prefix, "_parameter_summary.csv")))
}

if (isTRUE(as.logical(opts$save_plots))) {
  write_plot_pdf(
    file.path(opts$output_figures_dir, paste0(opts$output_prefix, "_plots.pdf")),
    plot_trends, plot_total, plot_ppc_global
  )
}

cat("\nM1 concluído.\n")
cat("Data version id:", opts$data_version_id, "\n")
cat("Input completo:", ifelse(input_complete, "SIM", "NÃO"), "\n")
cat("Painel completo:", ifelse(panel_complete, "SIM", "NÃO"), "\n")
cat("N meses:", Tn, "| N crimes:", Cn, "| N linhas:", nrow(panel), "\n")
cat("Média de occ:", round(mean(panel$occ), 3), "| Variância de occ:", round(var(panel$occ), 3), "\n")
cat("Zeros em occ:", sum(panel$occ == 0), "\n\n")

cat("===== SNAPSHOT =====\n")
safe_print(snapshot)
cat("\n===== OVERALL SUMMARY =====\n")
safe_print(overall_summary)
cat("\n===== TOP TENDÊNCIAS POR CRIME =====\n")
safe_print(crime_trend, n = min(20, nrow(crime_trend)))
cat("\n===== DIAGNÓSTICOS (primeiras linhas) =====\n")
safe_print(diagn, n = 20)
cat("\n===== PPC TOTAL MENSAL (primeiras linhas) =====\n")
safe_print(ppc_total, n = 12)
cat("\n===== PPC GLOBAL =====\n")
safe_print(ppc_global_summary)

if (interactive()) {
  print(plot_trends)
  print(plot_total)
  print(plot_ppc_global)
}

invisible(results)
