#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

############################################################
# RS Seguro — M2: perfil da vítima condicionado a macrocrimes-alvo
# Stan via cmdstanr (1 modelo por macrocrime-alvo)
#
# Padrão do repo:
# - rodar a partir do root do repositório
# - sem setwd()
# - entradas em data/raw/
# - saídas regeneráveis em outputs/
#
# Exemplos:
#   Rscript scripts/rs_seguro/rs_seguro_m2_macrocrime_conditional_cmdstanr.R --targets=VULNERAVEIS_E_CUIDADO,ORDEM_PUBLICA_E_OUTROS
#   Rscript scripts/rs_seguro/rs_seguro_m2_macrocrime_conditional_cmdstanr.R --m1_targets_csv=outputs/tables/rs_seguro/rs_seguro_m1_macrocrime_crime_trend.csv
############################################################

args <- commandArgs(trailingOnly = TRUE)
AUTO_INSTALL_PKGS <- "--auto-install" %in% args

parse_args <- function(args) {
  out <- list(
    input_csv = file.path("data", "raw", "rs_seguro", "rs_month_macrocrime_profile_v1_1vict.csv"),
    output_tables_dir = file.path("outputs", "tables", "rs_seguro", "m2_macrocrime_conditional"),
    output_figures_dir = file.path("outputs", "figures", "rs_seguro", "m2_macrocrime_conditional"),

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
    write_csv_outputs = if (interactive()) 0L else 1L,
    save_rds_results = 0L,
    save_plots = if (interactive()) 0L else 1L,

    targets = "",
    m1_targets_csv = file.path("outputs", "tables", "rs_seguro", "rs_seguro_m1_macrocrime_crime_trend.csv"),
    m1_prob_col = "p_gt_0",
    m1_name_col = "crime",
    m1_threshold = 0.90,

    collapse_rare_groups = 1L,
    rare_group_min_total_occ = 12L,
    rare_group_min_active_mon = 2L,
    rare_group_label = "OUTROS_PERFIS",

    min_total_occ_macrocrime = 20L,
    min_groups_after_filter = 2L,
    min_months_required = 6L
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
  out$write_csv_outputs <- as.integer(out$write_csv_outputs)
  out$save_rds_results <- as.integer(out$save_rds_results)
  out$save_plots <- as.integer(out$save_plots)
  out$m1_threshold <- as.numeric(out$m1_threshold)

  out$collapse_rare_groups <- as.integer(out$collapse_rare_groups)
  out$rare_group_min_total_occ <- as.integer(out$rare_group_min_total_occ)
  out$rare_group_min_active_mon <- as.integer(out$rare_group_min_active_mon)
  out$min_total_occ_macrocrime <- as.integer(out$min_total_occ_macrocrime)
  out$min_groups_after_filter <- as.integer(out$min_groups_after_filter)
  out$min_months_required <- as.integer(out$min_months_required)

  if (!nzchar(out$data_version_id)) {
    out$data_version_id <- paste0(
      "rs-seguro-dados-", format(Sys.Date(), "%Y-%m-%d"),
      "-occ-macrocrime-m2-conditional"
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
        "  Rscript scripts/_setup/install_deps.R\n\n",
        "Alternativa (manual, dentro do R):\n",
        "  install.packages('", pkg, "')\n\n",
        "Se você realmente quer auto-instalar neste script:\n",
        "  Rscript scripts/rs_seguro/rs_seguro_m2_macrocrime_conditional_cmdstanr.R --auto-install\n",
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
  "cmdstanr", "posterior", "bayesplot", "ggplot2", "dplyr", "readr",
  "tidyr", "stringr", "tibble", "purrr"
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
  library(purrr)
})

stan_code <- '
data {
  int<lower=1> N;
  int<lower=1> G;
  int<lower=1> M;
  array[N] int<lower=1, upper=G> group_id;
  array[N] int<lower=1, upper=M> month_id;
  vector[N] t_std;
  array[N] int<lower=0> y;
  real log_y_mean;
}

parameters {
  real alpha_global;
  real beta_global;

  real<lower=0> sigma_alpha_group;
  real<lower=0> sigma_beta_group;
  real<lower=0> sigma_gamma_month;
  real<lower=0> sigma_log_phi;

  vector[G] z_alpha_group;
  vector[G] z_beta_group;
  vector[M] z_gamma_month_raw;
  vector[G] z_log_phi;

  real<lower=-6, upper=6> log_phi_global;
}

transformed parameters {
  vector[G] alpha_group;
  vector[G] beta_group;
  vector[M] gamma_month;
  vector[M] gamma_month_raw;
  vector[G] log_phi_group;
  vector<lower=1e-6, upper=3000>[G] phi_group;
  vector[N] eta;

  alpha_group = sigma_alpha_group * z_alpha_group;
  beta_group  = sigma_beta_group  * z_beta_group;

  gamma_month_raw = sigma_gamma_month * z_gamma_month_raw;
  gamma_month = gamma_month_raw - mean(gamma_month_raw);

  for (g in 1:G) {
    real log_phi_raw;
    log_phi_raw = log_phi_global + sigma_log_phi * z_log_phi[g];
    log_phi_group[g] = fmin(8.0, fmax(-8.0, log_phi_raw));
    phi_group[g] = exp(log_phi_group[g]);
  }

  for (n in 1:N) {
    real eta_raw;
    eta_raw = alpha_global
            + alpha_group[group_id[n]]
            + (beta_global + beta_group[group_id[n]]) * t_std[n]
            + gamma_month[month_id[n]];
    eta[n] = fmin(10.0, fmax(-10.0, eta_raw));
  }
}

model {
  alpha_global ~ normal(log_y_mean, 0.6);
  beta_global  ~ normal(0.0, 0.08);

  sigma_alpha_group ~ normal(0, 0.35);
  sigma_beta_group  ~ normal(0, 0.05);
  sigma_gamma_month ~ normal(0, 0.20);
  sigma_log_phi     ~ normal(0, 0.12);

  z_alpha_group     ~ std_normal();
  z_beta_group      ~ std_normal();
  z_gamma_month_raw ~ std_normal();
  z_log_phi         ~ std_normal();

  log_phi_global ~ normal(log(8), 0.4);

  y ~ neg_binomial_2_log(eta, phi_group[group_id]);
}

generated quantities {
  vector[N] log_lik;
  array[N] int y_rep;
  vector[G] beta_total_group;

  for (g in 1:G) {
    beta_total_group[g] = beta_global + beta_group[g];
  }

  for (n in 1:N) {
    log_lik[n] = neg_binomial_2_log_lpmf(y[n] | eta[n], phi_group[group_id[n]]);
    y_rep[n]   = neg_binomial_2_log_rng(eta[n], phi_group[group_id[n]]);
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

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

bind_or_empty <- function(lst, field) {
  if (length(lst) == 0) return(tibble())
  out <- lapply(lst, `[[`, field)
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0) return(tibble())
  bind_rows(out)
}

slugify <- function(x) {
  x %>%
    stringr::str_to_upper() %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    stringr::str_replace_all("[^A-Z0-9]+", "_") %>%
    stringr::str_replace_all("(^_+|_+$)", "") %>%
    stringr::str_sub(1, 80)
}

expand_months <- function(ym_vec) {
  ym_vec <- sort(unique(as.character(ym_vec)))
  d0 <- as.Date(paste0(min(ym_vec), "-01"))
  d1 <- as.Date(paste0(max(ym_vec), "-01"))
  format(seq(d0, d1, by = "month"), "%Y-%m")
}

split_targets <- function(x) {
  x <- trimws(unlist(strsplit(x, ",", fixed = TRUE)))
  unique(x[nzchar(x)])
}

get_target_macrocrimes <- function(targets, m1_csv, name_col, prob_col, threshold) {
  manual_targets <- split_targets(targets)
  if (length(manual_targets) > 0) {
    return(manual_targets)
  }

  if (!nzchar(m1_csv) || !file.exists(m1_csv)) {
    stop(
      "Defina --targets=... ou informe --m1_targets_csv=... com um arquivo existente.\n",
      "Exemplo: --targets=VULNERAVEIS_E_CUIDADO,ORDEM_PUBLICA_E_OUTROS"
    )
  }

  m1_tbl <- readr::read_csv(m1_csv, show_col_types = FALSE)
  miss <- setdiff(c(name_col, prob_col), names(m1_tbl))
  if (length(miss) > 0) {
    stop("Faltam colunas em m1_targets_csv: ", paste(miss, collapse = ", "))
  }

  alvos <- m1_tbl %>%
    filter(.data[[prob_col]] > threshold) %>%
    pull(.data[[name_col]]) %>%
    as.character() %>%
    unique()

  if (length(alvos) == 0) {
    stop("Nenhum macrocrime-alvo encontrado em m1_targets_csv com limiar > ", threshold)
  }

  alvos
}

collapse_groups <- function(df, min_total_occ, min_active_mon, rare_label) {
  totals <- df %>%
    group_by(group) %>%
    summarise(
      total_occ = sum(occ_1_vitima, na.rm = TRUE),
      active_months = n_distinct(ym[occ_1_vitima > 0]),
      .groups = "drop"
    )

  keep_groups <- totals %>%
    filter(total_occ >= min_total_occ, active_months >= min_active_mon) %>%
    pull(group)

  if (length(keep_groups) == 0) {
    keep_groups <- totals %>%
      arrange(desc(total_occ), group) %>%
      slice_head(n = min(3, n())) %>%
      pull(group)
  }

  df2 <- df %>%
    mutate(group = if_else(group %in% keep_groups, group, rare_label)) %>%
    group_by(ym, crime, group) %>%
    summarise(occ_1_vitima = sum(occ_1_vitima), .groups = "drop")

  list(data = df2, keep_groups = keep_groups, totals_before = totals)
}

build_panel_for_macrocrime <- function(df_macro, global_month_levels) {
  stopifnot(length(unique(df_macro$crime)) == 1)
  macrocrime <- unique(df_macro$crime)

  group_totals <- df_macro %>%
    group_by(group) %>%
    summarise(total_occ = sum(occ_1_vitima), .groups = "drop") %>%
    arrange(desc(total_occ), group)

  group_levels <- group_totals$group

  panel <- tidyr::crossing(
    ym = global_month_levels,
    group = group_levels
  ) %>%
    mutate(crime = macrocrime) %>%
    left_join(df_macro, by = c("ym", "crime", "group")) %>%
    mutate(
      occ_1_vitima = dplyr::coalesce(as.integer(occ_1_vitima), 0L),
      date = as.Date(paste0(ym, "-01")),
      t = match(ym, global_month_levels),
      month_id = as.integer(format(date, "%m")),
      group_id = match(group, group_levels)
    ) %>%
    arrange(t, group_id) %>%
    mutate(t_std = as.numeric(scale(t)))

  if (any(!is.finite(panel$t_std))) {
    stop("t_std contém valores não finitos. Verifique o número de meses distintos.")
  }

  panel
}

make_inits_m2 <- function(G, log_y_mean) {
  function(chain_id = 1) {
    list(
      alpha_global      = log_y_mean,
      beta_global       = 0,
      sigma_alpha_group = 0.10,
      sigma_beta_group  = 0.02,
      sigma_gamma_month = 0.03,
      sigma_log_phi     = 0.04,
      z_alpha_group     = rep(0, G),
      z_beta_group      = rep(0, G),
      z_gamma_month_raw = rep(0, 12),
      z_log_phi         = rep(0, G),
      log_phi_global    = log(8)
    )
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
  stop("Arquivo de entrada não encontrado: ", opts$input_csv)
}

dir.create(opts$output_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(opts$output_figures_dir, recursive = TRUE, showWarnings = FALSE)

tmp_cmdstan_dir <- tempfile("m2_cmdstan_")
dir.create(tmp_cmdstan_dir, recursive = TRUE, showWarnings = FALSE)

if (!isTRUE(as.logical(opts$keep_temp_files))) {
  on.exit(unlink(tmp_cmdstan_dir, recursive = TRUE, force = TRUE), add = TRUE)
}

fit_one_macrocrime <- function(macrocrime, raw_profile, global_month_levels, mod, tmp_cmdstan_dir) {
  d0 <- raw_profile %>%
    filter(crime == macrocrime) %>%
    mutate(group = paste(sexo, faixa, sep = " | ")) %>%
    select(ym, crime, group, occ_1_vitima)

  if (nrow(d0) == 0) {
    return(list(status = "skip", reason = "macrocrime sem linhas no input"))
  }

  total_occ_macro <- sum(d0$occ_1_vitima, na.rm = TRUE)
  active_months_macro <- n_distinct(d0$ym)

  if (total_occ_macro < opts$min_total_occ_macrocrime) {
    return(list(status = "skip", reason = paste0("total_occ < ", opts$min_total_occ_macrocrime)))
  }

  if (active_months_macro < opts$min_months_required) {
    return(list(status = "skip", reason = paste0("meses_ativos < ", opts$min_months_required)))
  }

  collapse_info <- NULL
  if (isTRUE(as.logical(opts$collapse_rare_groups))) {
    collapse_info <- collapse_groups(
      df = d0,
      min_total_occ = opts$rare_group_min_total_occ,
      min_active_mon = opts$rare_group_min_active_mon,
      rare_label = opts$rare_group_label
    )
    d1 <- collapse_info$data
  } else {
    d1 <- d0
  }

  panel <- build_panel_for_macrocrime(d1, global_month_levels = global_month_levels)

  G <- n_distinct(panel$group)
  Tn <- n_distinct(panel$ym)
  expected_n <- G * Tn
  panel_complete <- nrow(panel) == expected_n

  if (G < opts$min_groups_after_filter) {
    return(list(status = "skip", reason = paste0("n_grupos < ", opts$min_groups_after_filter)))
  }

  log_y_mean <- log(mean(panel$occ_1_vitima) + 0.1)

  stan_data <- list(
    N          = nrow(panel),
    G          = G,
    M          = 12,
    group_id   = panel$group_id,
    month_id   = panel$month_id,
    t_std      = panel$t_std,
    y          = panel$occ_1_vitima,
    log_y_mean = log_y_mean
  )

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
    init            = make_inits_m2(G = G, log_y_mean = log_y_mean)
  )

  diagn <- fit$diagnostic_summary() %>% as_tibble()
  summ  <- fit$summary() %>% as_tibble()
  draws_df <- fit$draws(format = "df") %>% as_tibble()

  beta_global_draws <- draws_df$beta_global
  overall_summary <- tibble(
    crime = macrocrime,
    parameter = "beta_global",
    mean   = mean(beta_global_draws),
    median = median(beta_global_draws),
    q05    = q_num(beta_global_draws, 0.05),
    q50    = q_num(beta_global_draws, 0.50),
    q95    = q_num(beta_global_draws, 0.95),
    p_lt_0 = mean(beta_global_draws < 0),
    p_gt_0 = mean(beta_global_draws > 0)
  ) %>%
    mutate(
      classificacao = case_when(
        p_gt_0 > 0.90 ~ "aumentando",
        p_lt_0 > 0.90 ~ "diminuindo",
        TRUE ~ "incerto/estavel"
      )
    )

  group_levels <- panel %>% distinct(group_id, group) %>% arrange(group_id) %>% pull(group)
  beta_cols <- paste0("beta_total_group[", seq_len(G), "]")
  beta_mat <- as.matrix(draws_df[, beta_cols, drop = FALSE])
  colnames(beta_mat) <- group_levels

  group_volume <- panel %>%
    group_by(group) %>%
    summarise(
      total_occ = sum(occ_1_vitima),
      media_mensal = mean(occ_1_vitima),
      meses_com_occ = sum(occ_1_vitima > 0),
      .groups = "drop"
    ) %>%
    mutate(share_occ = total_occ / sum(total_occ))

  group_trend <- tibble(
    crime  = macrocrime,
    group  = group_levels,
    mean   = apply(beta_mat, 2, mean),
    median = apply(beta_mat, 2, median),
    q05    = apply(beta_mat, 2, q_num, probs = 0.05),
    q50    = apply(beta_mat, 2, q_num, probs = 0.50),
    q95    = apply(beta_mat, 2, q_num, probs = 0.95),
    p_gt_0 = colMeans(beta_mat > 0),
    p_lt_0 = colMeans(beta_mat < 0)
  ) %>%
    left_join(group_volume, by = "group") %>%
    mutate(
      classificacao = case_when(
        p_gt_0 > 0.90 ~ "aumentando",
        p_lt_0 > 0.90 ~ "diminuindo",
        TRUE ~ "incerto/estavel"
      )
    ) %>%
    arrange(desc(p_gt_0), desc(total_occ), desc(mean))

  obs_total <- panel %>%
    group_by(ym, t) %>%
    summarise(y_obs_total = sum(occ_1_vitima), .groups = "drop") %>%
    arrange(t)

  rep_cols <- grep("^y_rep\\[", names(draws_df), value = TRUE)
  yrep_mat <- as.matrix(draws_df[, rep_cols, drop = FALSE])
  month_index <- split(seq_len(nrow(panel)), panel$t)
  rep_total_by_month <- sapply(month_index, function(idx) rowSums(yrep_mat[, idx, drop = FALSE]))
  colnames(rep_total_by_month) <- global_month_levels

  ppc_total <- tibble(
    crime = macrocrime,
    ym = global_month_levels,
    y_obs_total = obs_total$y_obs_total,
    y_rep_mean = apply(rep_total_by_month, 2, mean),
    y_rep_q05  = apply(rep_total_by_month, 2, q_num, probs = 0.05),
    y_rep_q50  = apply(rep_total_by_month, 2, q_num, probs = 0.50),
    y_rep_q95  = apply(rep_total_by_month, 2, q_num, probs = 0.95)
  )

  obs_grand_total <- sum(panel$occ_1_vitima)
  rep_grand_total <- rowSums(yrep_mat)
  ppc_global_summary <- tibble(
    crime = macrocrime,
    obs_grand_total = obs_grand_total,
    rep_mean = mean(rep_grand_total),
    rep_q05  = q_num(rep_grand_total, 0.05),
    rep_q50  = q_num(rep_grand_total, 0.50),
    rep_q95  = q_num(rep_grand_total, 0.95)
  )

  snapshot <- tibble(
    crime = macrocrime,
    data_version_id = opts$data_version_id,
    n_rows_panel = nrow(panel),
    n_months = Tn,
    n_groups = G,
    ym_min = min(panel$ym),
    ym_max = max(panel$ym),
    total_occ = sum(panel$occ_1_vitima),
    occ_mean = mean(panel$occ_1_vitima),
    occ_var = var(panel$occ_1_vitima),
    zero_occ = sum(panel$occ_1_vitima == 0),
    panel_complete = panel_complete,
    expected_n = expected_n,
    observed_n = nrow(panel)
  )

  plot_groups <- group_trend %>%
    mutate(group = reorder(group, mean)) %>%
    ggplot(aes(x = mean, y = group, xmin = q05, xmax = q95, color = classificacao)) +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_errorbarh(height = 0.2) +
    geom_point() +
    labs(
      title = paste0("M2 — Tendência por grupo em ", macrocrime),
      subtitle = "Inclinação total = beta_global + desvio por grupo",
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
      title = paste0("M2 — Série observada vs posterior preditiva: ", macrocrime),
      x = NULL,
      y = "Ocorrências mensais (1 vítima, perfil v1)"
    ) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

  out <- list(
    status = "ok",
    crime = macrocrime,
    panel = panel,
    snapshot = snapshot,
    diagnostics = diagn,
    parameter_summary = summ,
    overall_summary = overall_summary,
    group_trend = group_trend,
    ppc_total = ppc_total,
    ppc_global_summary = ppc_global_summary,
    collapse_info = collapse_info,
    plots = list(
      plot_groups = plot_groups,
      plot_total = plot_total
    )
  )

  out
}

raw_profile <- read_csv(opts$input_csv, show_col_types = FALSE) %>%
  transmute(
    ym           = as.character(ym),
    crime        = as.character(crime),
    sexo         = as.character(sexo),
    faixa        = as.character(faixa),
    occ_1_vitima = as.integer(occ_1_vitima)
  )

required_cols <- c("ym", "crime", "sexo", "faixa", "occ_1_vitima")
missing_cols <- setdiff(required_cols, names(raw_profile))
if (length(missing_cols) > 0) {
  stop("Faltam colunas no input: ", paste(missing_cols, collapse = ", "))
}

if (any(is.na(raw_profile$ym)) || any(is.na(raw_profile$crime)) || any(is.na(raw_profile$occ_1_vitima))) {
  stop("Há NAs em ym/crime/occ_1_vitima. Corrija antes de ajustar o modelo.")
}
if (any(raw_profile$occ_1_vitima < 0, na.rm = TRUE)) {
  stop("Há contagens negativas em occ_1_vitima.")
}

macrocrimes_alvo <- get_target_macrocrimes(
  targets = opts$targets,
  m1_csv = opts$m1_targets_csv,
  name_col = opts$m1_name_col,
  prob_col = opts$m1_prob_col,
  threshold = opts$m1_threshold
)

macrocrimes_disponiveis <- sort(unique(raw_profile$crime))
macrocrimes_invalidos <- setdiff(macrocrimes_alvo, macrocrimes_disponiveis)
if (length(macrocrimes_invalidos) > 0) {
  stop(
    "Os seguintes macrocrimes-alvo não existem no input: ",
    paste(macrocrimes_invalidos, collapse = ", ")
  )
}

raw_profile <- raw_profile %>%
  filter(crime %in% macrocrimes_alvo)

global_month_levels <- expand_months(raw_profile$ym)

stan_file_tmp <- cmdstanr::write_stan_file(stan_code, dir = tmp_cmdstan_dir)
mod <- cmdstanr::cmdstan_model(stan_file_tmp, quiet = TRUE)

results_by_crime <- vector("list", length = length(macrocrimes_alvo))
names(results_by_crime) <- macrocrimes_alvo

for (i in seq_along(macrocrimes_alvo)) {
  alvo <- macrocrimes_alvo[i]
  cat("\n==============================\n")
  cat("M2 ajustando macrocrime:", alvo, "\n")
  cat("==============================\n")

  res_i <- tryCatch(
    fit_one_macrocrime(
      macrocrime = alvo,
      raw_profile = raw_profile,
      global_month_levels = global_month_levels,
      mod = mod,
      tmp_cmdstan_dir = tmp_cmdstan_dir
    ),
    error = function(e) list(status = "error", crime = alvo, reason = e$message)
  )

  results_by_crime[[alvo]] <- res_i

  if (!identical(res_i$status, "ok")) {
    cat("Status:", res_i$status, "| Motivo:", res_i$reason, "\n")
    next
  }

  cat("Status: ok\n")
  cat(
    "N meses:", res_i$snapshot$n_months,
    "| N grupos:", res_i$snapshot$n_groups,
    "| N linhas:", res_i$snapshot$n_rows_panel, "\n"
  )
  cat(
    "Total occ_1_vitima:", res_i$snapshot$total_occ,
    "| Zeros:", res_i$snapshot$zero_occ, "\n"
  )

  top_up <- res_i$group_trend %>%
    filter(classificacao == "aumentando") %>%
    slice_head(n = 5) %>%
    select(group, total_occ, p_gt_0, q05, q95)

  if (nrow(top_up) > 0) {
    cat("Top grupos em alta:\n")
    print(top_up)
  } else {
    cat("Nenhum grupo com p_gt_0 > 0.90 neste macrocrime.\n")
  }

  if (isTRUE(as.logical(opts$write_csv_outputs))) {
    crime_slug <- slugify(alvo)
    readr::write_csv(res_i$snapshot, file.path(opts$output_tables_dir, paste0("snapshot_", crime_slug, ".csv")))
    readr::write_csv(res_i$overall_summary, file.path(opts$output_tables_dir, paste0("overall_summary_", crime_slug, ".csv")))
    readr::write_csv(res_i$group_trend, file.path(opts$output_tables_dir, paste0("group_trend_", crime_slug, ".csv")))
    readr::write_csv(res_i$ppc_total, file.path(opts$output_tables_dir, paste0("ppc_total_", crime_slug, ".csv")))
    readr::write_csv(res_i$diagnostics, file.path(opts$output_tables_dir, paste0("diagnostics_", crime_slug, ".csv")))
    readr::write_csv(res_i$parameter_summary, file.path(opts$output_tables_dir, paste0("parameter_summary_", crime_slug, ".csv")))
  }

  if (isTRUE(as.logical(opts$save_plots))) {
    crime_slug <- slugify(alvo)
    write_plot_pdf(
      file.path(opts$output_figures_dir, paste0("plots_", crime_slug, ".pdf")),
      res_i$plots$plot_groups,
      res_i$plots$plot_total
    )
  }
}

results_ok <- results_by_crime[vapply(results_by_crime, function(x) identical(x$status, "ok"), logical(1))]

snapshot_all <- bind_or_empty(results_ok, "snapshot")
overall_all  <- bind_or_empty(results_ok, "overall_summary")
group_trend_all <- bind_or_empty(results_ok, "group_trend")
ppc_global_all  <- bind_or_empty(results_ok, "ppc_global_summary")

status_table <- tibble(
  crime = names(results_by_crime),
  status = vapply(results_by_crime, function(x) x$status %||% "unknown", character(1)),
  reason = vapply(results_by_crime, function(x) x$reason %||% NA_character_, character(1))
)

results <- list(
  data_version_id = opts$data_version_id,
  macrocrimes_alvo = macrocrimes_alvo,
  status_table = status_table,
  snapshot_all = snapshot_all,
  overall_all = overall_all,
  group_trend_all = group_trend_all,
  ppc_global_all = ppc_global_all,
  results_by_crime = results_by_crime
)

if (isTRUE(as.logical(opts$write_csv_outputs))) {
  readr::write_csv(status_table, file.path(opts$output_tables_dir, "status_table.csv"))
  if (ncol(snapshot_all) > 0) readr::write_csv(snapshot_all, file.path(opts$output_tables_dir, "snapshot_all.csv"))
  if (ncol(overall_all) > 0) readr::write_csv(overall_all, file.path(opts$output_tables_dir, "overall_all.csv"))
  if (ncol(group_trend_all) > 0) readr::write_csv(group_trend_all, file.path(opts$output_tables_dir, "group_trend_all.csv"))
  if (ncol(ppc_global_all) > 0) readr::write_csv(ppc_global_all, file.path(opts$output_tables_dir, "ppc_global_all.csv"))
}

if (isTRUE(as.logical(opts$save_rds_results))) {
  saveRDS(results, file = file.path(opts$output_tables_dir, "m2_macrocrime_conditional_results.rds"))
}

cat("\nM2 concluído.\n")
cat("Data version id:", opts$data_version_id, "\n")
cat("Macrocrimes-alvo:", paste(macrocrimes_alvo, collapse = " | "), "\n")
cat(
  "Resultados OK:", nrow(status_table %>% filter(status == "ok")),
  "| Skip/erro:", nrow(status_table %>% filter(status != "ok")), "\n\n"
)

cat("===== STATUS TABLE =====\n")
safe_print(status_table)

if (nrow(snapshot_all) > 0) {
  cat("\n===== SNAPSHOT ALL =====\n")
  safe_print(snapshot_all)
}

if (nrow(overall_all) > 0) {
  cat("\n===== OVERALL ALL =====\n")
  safe_print(overall_all)
}

if (nrow(group_trend_all) > 0) {
  cat("\n===== TOP GRUPOS EM ALTA =====\n")
  safe_print(
    group_trend_all %>%
      filter(classificacao == "aumentando") %>%
      arrange(desc(p_gt_0), desc(total_occ), desc(mean)) %>%
      select(crime, group, total_occ, share_occ, mean, q05, q95, p_gt_0) %>%
      slice_head(n = 20)
  )
}

invisible(results)
