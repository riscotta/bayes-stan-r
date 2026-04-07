#!/usr/bin/env Rscript

###############################################################################
# Tendência temporal das retratações científicas — R + Stan via rstan
# Base externa: Global Scientific Retractions 1927–2026 (Kaggle)
# Padrão do repositório:
#   - sem setwd()
#   - execução a partir da raiz do repo
#   - dados brutos fora do versionamento
#   - saída principal no console (sem artefatos do estudo por padrão)
#
# Como rodar:
#   Rscript scripts/retractions_trend/retractions_trend_rstan.R
#
# Exemplo com parâmetros:
#   Rscript scripts/retractions_trend/retractions_trend_rstan.R \
#     --group_var=publisher --top_n_groups=15 --min_group_total=100 \
#     --chains=4 --iter_warmup=1000 --iter_sampling=1000
#
# Dependências:
#   Rscript scripts/_setup/install_deps.R --all
###############################################################################

options(stringsAsFactors = FALSE)
set.seed(20260407)

parse_args <- function(args) {
  out <- list(
    input_csv = file.path(
      'data', 'raw', 'retractions_time_to_retraction',
      'global_scientific_retractions_1927_2026.csv'
    ),
    group_var = 'publisher',
    top_n_groups = 15L,
    min_group_total = 100L,
    start_year = NA_integer_,
    end_year_complete = 2025L,
    recent_incomplete_years = '2026',
    chains = 4L,
    iter_warmup = 1000L,
    iter_sampling = 1000L,
    adapt_delta = 0.95,
    max_treedepth = 12L,
    seed = 20260407L
  )

  if (length(args) == 0) return(out)

  for (a in args) {
    if (!startsWith(a, '--')) next
    a2 <- sub('^--', '', a)
    if (!grepl('=', a2, fixed = TRUE)) next

    key <- sub('=.*$', '', a2)
    val <- sub('^.*=', '', a2)

    if (key %in% names(out)) out[[key]] <- val
  }

  int_keys <- c(
    'top_n_groups', 'min_group_total', 'start_year', 'end_year_complete',
    'chains', 'iter_warmup', 'iter_sampling', 'max_treedepth', 'seed'
  )
  num_keys <- c('adapt_delta')

  for (k in int_keys) {
    out[[k]] <- if (is.na(suppressWarnings(as.integer(out[[k]])))) NA_integer_ else as.integer(out[[k]])
  }
  for (k in num_keys) out[[k]] <- as.numeric(out[[k]])

  out
}

require_pkgs <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      'Pacotes obrigatórios ausentes: ', paste(missing, collapse = ', '),
      '\nInstale-os antes de rodar o script com: Rscript scripts/_setup/install_deps.R --all',
      call. = FALSE
    )
  }
}

safe_group_label <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[x == '' | is.na(x)] <- 'Desconhecido'
  x
}

summarise_draw_vector <- function(x, probs = c(0.025, 0.50, 0.975)) {
  q <- stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE)
  tibble::tibble(
    mean = mean(x, na.rm = TRUE),
    median = q[2],
    q025 = q[1],
    q975 = q[3]
  )
}

print_section <- function(title) {
  cat('\n', strrep('=', 78), '\n', sep = '')
  cat(title, '\n')
  cat(strrep('=', 78), '\n', sep = '')
}

extract_draws_df <- function(fit_object, variables) {
  posterior::as_draws_df(
    posterior::subset_draws(
      posterior::as_draws_array(fit_object),
      variable = variables
    )
  )
}

extract_divergences <- function(fit_object) {
  sp <- rstan::get_sampler_params(fit_object, inc_warmup = FALSE)
  sum(vapply(sp, function(x) sum(x[, 'divergent__']), numeric(1)))
}

extract_max_treedepth_hits <- function(fit_object, max_treedepth) {
  sp <- rstan::get_sampler_params(fit_object, inc_warmup = FALSE)
  sum(vapply(sp, function(x) sum(x[, 'treedepth__'] >= max_treedepth), numeric(1)))
}

build_mu_matrix <- function(draws_df, panel_df, G, T) {
  alpha <- draws_df$alpha
  a_mat <- as.matrix(draws_df[, paste0('a[', seq_len(G), ']'), drop = FALSE])
  b_mat <- as.matrix(draws_df[, paste0('b[', seq_len(G), ']'), drop = FALSE])
  rw_mat <- as.matrix(draws_df[, paste0('rw[', seq_len(T), ']'), drop = FALSE])

  S <- length(alpha)
  N <- nrow(panel_df)
  mu_mat <- matrix(NA_real_, nrow = S, ncol = N)

  for (n in seq_len(N)) {
    g <- panel_df$group_id[n]
    t <- panel_df$year_id[n]
    mu_mat[, n] <- exp(alpha + a_mat[, g] + b_mat[, g] * panel_df$t_std[n] + rw_mat[, t])
  }

  mu_mat
}

build_yrep_matrix_from_mu_phi <- function(mu_mat, phi_draws) {
  stopifnot(is.matrix(mu_mat))
  stopifnot(length(phi_draws) == nrow(mu_mat))

  if (any(!is.finite(mu_mat))) {
    stop('mu_mat contém valores não finitos; verifique o ajuste antes de gerar PPC.', call. = FALSE)
  }
  if (any(phi_draws <= 0 | !is.finite(phi_draws))) {
    stop('phi_draws contém valores inválidos; verifique o ajuste antes de gerar PPC.', call. = FALSE)
  }

  yrep_vec <- stats::rnbinom(
    n = length(mu_mat),
    size = rep(phi_draws, times = ncol(mu_mat)),
    mu = as.vector(mu_mat)
  )

  matrix(yrep_vec, nrow = nrow(mu_mat), ncol = ncol(mu_mat))
}

summarise_curve_matrix <- function(mat, id_values, id_name) {
  stopifnot(ncol(mat) == length(id_values))
  pieces <- lapply(seq_along(id_values), function(i) {
    sm <- summarise_draw_vector(mat[, i])
    sm[[id_name]] <- id_values[i]
    sm
  })
  dplyr::bind_rows(pieces) %>%
    dplyr::select(dplyr::all_of(id_name), dplyr::everything())
}

opt <- parse_args(commandArgs(trailingOnly = TRUE))

required_pkgs <- c(
  'dplyr', 'tidyr', 'readr', 'stringr', 'forcats', 'posterior', 'tibble', 'rstan'
)
require_pkgs(required_pkgs)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(forcats)
  library(posterior)
  library(tibble)
  library(rstan)
})

rstan::rstan_options(auto_write = FALSE)
available_cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
if (is.na(available_cores) || available_cores < 1L) available_cores <- 1L
options(mc.cores = max(1L, min(4L, available_cores)))

if (!file.exists(opt$input_csv)) {
  stop(
    'Arquivo CSV não encontrado em: ', opt$input_csv,
    '\nBaixe a base externa indicada em data/raw/retractions_time_to_retraction/README.md ',
    'e salve o arquivo com esse nome, ou informe --input_csv=...',
    call. = FALSE
  )
}

if (!opt$group_var %in% c('publisher', 'country')) {
  stop(
    "Parâmetro inválido em --group_var. Use 'publisher' ou 'country'.",
    call. = FALSE
  )
}

# -----------------------------------------------------------------------------
# Leitura e preparação da base
# -----------------------------------------------------------------------------

raw_df <- readr::read_csv(opt$input_csv, show_col_types = FALSE)

required_cols <- c('retraction_year', opt$group_var)
missing_cols <- setdiff(required_cols, names(raw_df))
if (length(missing_cols) > 0) {
  stop('Colunas obrigatórias ausentes no CSV: ', paste(missing_cols, collapse = ', '), call. = FALSE)
}

start_year <- opt$start_year
if (is.na(start_year)) {
  start_year <- suppressWarnings(min(as.integer(raw_df$retraction_year), na.rm = TRUE))
}
if (!is.finite(start_year)) {
  stop('Não foi possível determinar start_year.', call. = FALSE)
}

if ('row_quality_flag' %in% names(raw_df)) {
  df <- raw_df %>%
    mutate(
      retraction_year = suppressWarnings(as.integer(retraction_year)),
      group_raw = safe_group_label(.data[[opt$group_var]]),
      row_quality_flag = safe_group_label(row_quality_flag)
    ) %>%
    filter(!is.na(retraction_year)) %>%
    filter(row_quality_flag == 'OK')
} else {
  warning("Coluna 'row_quality_flag' não encontrada; usando todas as linhas com ano válido.")
  df <- raw_df %>%
    mutate(
      retraction_year = suppressWarnings(as.integer(retraction_year)),
      group_raw = safe_group_label(.data[[opt$group_var]])
    ) %>%
    filter(!is.na(retraction_year))
}

df_fit <- df %>%
  filter(retraction_year >= start_year, retraction_year <= opt$end_year_complete)

if (nrow(df_fit) == 0) {
  stop('Não há dados no intervalo de ajuste definido.', call. = FALSE)
}

year_summary <- df_fit %>%
  count(retraction_year, name = 'n_year') %>%
  arrange(retraction_year)

overdispersion_ratio <- stats::var(year_summary$n_year) / mean(year_summary$n_year)

group_totals_raw <- df_fit %>%
  count(group_raw, name = 'n_total') %>%
  arrange(desc(n_total), group_raw)

group_keep <- group_totals_raw %>%
  filter(n_total >= opt$min_group_total) %>%
  slice_head(n = opt$top_n_groups) %>%
  pull(group_raw)

if (length(group_keep) == 0) {
  group_keep <- group_totals_raw %>%
    slice_head(n = opt$top_n_groups) %>%
    pull(group_raw)
}

df_fit <- df_fit %>%
  mutate(group_collapsed = ifelse(group_raw %in% group_keep, group_raw, 'Outros'))

group_order <- df_fit %>%
  count(group_collapsed, name = 'n_total') %>%
  arrange(desc(n_total), group_collapsed) %>%
  pull(group_collapsed) %>%
  unique()

if ('Outros' %in% group_order) {
  group_order <- c(setdiff(group_order, 'Outros'), 'Outros')
}

years_fit <- seq(from = start_year, to = opt$end_year_complete, by = 1L)
t_std_map <- as.numeric(scale(years_fit))

panel <- df_fit %>%
  count(retraction_year, group_collapsed, name = 'y') %>%
  tidyr::complete(
    retraction_year = years_fit,
    group_collapsed = group_order,
    fill = list(y = 0L)
  ) %>%
  arrange(retraction_year, group_collapsed) %>%
  mutate(
    year_id = match(retraction_year, years_fit),
    group_collapsed = factor(group_collapsed, levels = group_order),
    group_id = as.integer(group_collapsed),
    t_std = t_std_map[year_id]
  )

panel_global_obs <- panel %>%
  group_by(retraction_year) %>%
  summarise(y_obs = sum(y), .groups = 'drop')

group_table <- df_fit %>%
  count(group_collapsed, name = 'n_total') %>%
  mutate(share = n_total / sum(n_total)) %>%
  arrange(desc(n_total))

prep_summary <- tibble(
  input_csv = opt$input_csv,
  total_rows_raw = nrow(raw_df),
  total_rows_used = nrow(df_fit),
  start_year = start_year,
  end_year_complete = opt$end_year_complete,
  excluded_recent_years = opt$recent_incomplete_years,
  n_years = length(years_fit),
  n_groups = length(group_order),
  group_var = opt$group_var,
  overdispersion_ratio = overdispersion_ratio
)

# -----------------------------------------------------------------------------
# Modelo STAN
# -----------------------------------------------------------------------------

stan_code <- '
data {
  int<lower=1> N;
  int<lower=1> T;
  int<lower=1> G;
  array[N] int<lower=1, upper=T> year_id;
  array[N] int<lower=1, upper=G> group_id;
  array[N] int<lower=0> y;
  vector[N] t_std;
}
transformed data {
  real log_y_mean = log((sum(y) + 0.5) / N);
}
parameters {
  real alpha;
  vector[G] z_a;
  real<lower=0> sigma_a;
  vector[G] z_b;
  real<lower=0> sigma_b;
  vector[T - 1] z_rw;
  real<lower=0> sigma_rw;
  real<lower=0> phi;
}
transformed parameters {
  vector[G] a;
  vector[G] b;
  vector[T] rw_raw;
  vector[T] rw;
  vector[N] eta;
  vector[N] mu;

  a = sigma_a * z_a;
  b = sigma_b * z_b;

  rw_raw[1] = 0;
  for (t in 2:T) {
    rw_raw[t] = rw_raw[t - 1] + sigma_rw * z_rw[t - 1];
  }

  rw = rw_raw - mean(rw_raw);

  for (n in 1:N) {
    eta[n] = alpha + a[group_id[n]] + b[group_id[n]] * t_std[n] + rw[year_id[n]];
    mu[n] = exp(eta[n]);
  }
}
model {
  alpha ~ normal(log_y_mean, 1.5);

  z_a ~ std_normal();
  sigma_a ~ normal(0, 0.8);

  z_b ~ std_normal();
  sigma_b ~ normal(0, 0.3);

  z_rw ~ std_normal();
  sigma_rw ~ normal(0, 0.35);

  phi ~ exponential(1);

  y ~ neg_binomial_2(mu, phi);
}
generated quantities {
  vector[N] log_lik;

  for (n in 1:N) {
    log_lik[n] = neg_binomial_2_lpmf(y[n] | mu[n], phi);
  }
}
'

stan_data <- list(
  N = nrow(panel),
  T = length(years_fit),
  G = length(group_order),
  year_id = panel$year_id,
  group_id = panel$group_id,
  y = panel$y,
  t_std = panel$t_std
)

# -----------------------------------------------------------------------------
# Ajuste
# -----------------------------------------------------------------------------

print_section('AJUSTE DO MODELO')
cat('Arquivo de entrada: ', opt$input_csv, '\n', sep = '')
cat('Grupo: ', opt$group_var, '\n', sep = '')
cat('Linhas usadas: ', nrow(df_fit), '\n', sep = '')
cat('Anos no ajuste: ', start_year, '–', opt$end_year_complete, '\n', sep = '')
cat('Número de grupos no modelo: ', length(group_order), '\n', sep = '')
cat('Iniciando compilação e amostragem via rstan...\n')

stan_model_obj <- rstan::stan_model(
  model_code = stan_code,
  model_name = paste0('retractions_trend_', opt$group_var, '_nb_rw1_console')
)

fit <- rstan::sampling(
  object = stan_model_obj,
  data = stan_data,
  seed = opt$seed,
  chains = opt$chains,
  cores = getOption('mc.cores', 1L),
  iter = opt$iter_warmup + opt$iter_sampling,
  warmup = opt$iter_warmup,
  refresh = 100,
  control = list(
    adapt_delta = opt$adapt_delta,
    max_treedepth = opt$max_treedepth
  )
)

# -----------------------------------------------------------------------------
# Diagnósticos
# -----------------------------------------------------------------------------

monitor_pars <- c(
  'alpha', 'sigma_a', 'sigma_b', 'sigma_rw', 'phi',
  paste0('a[', seq_len(length(group_order)), ']'),
  paste0('b[', seq_len(length(group_order)), ']'),
  paste0('rw[', seq_len(length(years_fit)), ']')
)

summary_tbl <- as.data.frame(rstan::summary(fit, pars = monitor_pars)$summary)
summary_tbl$variable <- rownames(summary_tbl)
summary_tbl <- tibble::as_tibble(summary_tbl)

n_divergent <- extract_divergences(fit)
n_max_treedepth <- extract_max_treedepth_hits(fit, opt$max_treedepth)

diag_tbl <- tibble(
  chains = opt$chains,
  iter_warmup = opt$iter_warmup,
  iter_sampling = opt$iter_sampling,
  n_divergent = n_divergent,
  n_max_treedepth = n_max_treedepth,
  max_rhat = max(summary_tbl$Rhat, na.rm = TRUE),
  min_bulk_ess = min(summary_tbl$n_eff, na.rm = TRUE)
)

# -----------------------------------------------------------------------------
# Extração das draws e resumos posteriores
# -----------------------------------------------------------------------------

draw_vars <- c('alpha', 'a', 'b', 'rw', 'phi')
draws_df <- extract_draws_df(fit, draw_vars)

mu_mat <- build_mu_matrix(
  draws_df = draws_df,
  panel_df = panel,
  G = length(group_order),
  T = length(years_fit)
)

yrep_mat <- build_yrep_matrix_from_mu_phi(mu_mat, draws_df$phi)

global_mu_mat <- sapply(seq_along(years_fit), function(t) {
  rowSums(mu_mat[, panel$year_id == t, drop = FALSE])
})

global_yrep_mat <- sapply(seq_along(years_fit), function(t) {
  rowSums(yrep_mat[, panel$year_id == t, drop = FALSE])
})

if (is.vector(global_mu_mat)) global_mu_mat <- matrix(global_mu_mat, ncol = 1)
if (is.vector(global_yrep_mat)) global_yrep_mat <- matrix(global_yrep_mat, ncol = 1)

global_curve <- summarise_curve_matrix(global_mu_mat, years_fit, 'retraction_year') %>%
  left_join(panel_global_obs, by = 'retraction_year') %>%
  mutate(
    mean = round(mean, 2),
    median = round(median, 2),
    q025 = round(q025, 2),
    q975 = round(q975, 2)
  )

global_ppc <- summarise_curve_matrix(global_yrep_mat, years_fit, 'retraction_year') %>%
  left_join(panel_global_obs, by = 'retraction_year') %>%
  mutate(
    mean = round(mean, 2),
    median = round(median, 2),
    q025 = round(q025, 2),
    q975 = round(q975, 2)
  )

late_years <- tail(years_fit, 5)
prev_years <- tail(years_fit, 10)[1:5]
late_idx <- match(late_years, years_fit)
prev_idx <- match(prev_years, years_fit)

global_ratio_draw <- rowMeans(global_mu_mat[, late_idx, drop = FALSE]) /
  rowMeans(global_mu_mat[, prev_idx, drop = FALSE])

global_diff_draw <- rowMeans(global_mu_mat[, late_idx, drop = FALSE]) -
  rowMeans(global_mu_mat[, prev_idx, drop = FALSE])

contrast_global <- dplyr::bind_cols(
  tibble(
    contrast = 'late5_vs_prev5_global',
    period_prev = paste(range(prev_years), collapse = '-'),
    period_late = paste(range(late_years), collapse = '-')
  ),
  summarise_draw_vector(global_ratio_draw) %>%
    rename(
      ratio_mean = mean,
      ratio_median = median,
      ratio_q025 = q025,
      ratio_q975 = q975
    ),
  summarise_draw_vector(global_diff_draw) %>%
    rename(
      diff_mean = mean,
      diff_median = median,
      diff_q025 = q025,
      diff_q975 = q975
    ),
  tibble(
    prob_ratio_gt_1 = mean(global_ratio_draw > 1),
    prob_diff_gt_0 = mean(global_diff_draw > 0)
  )
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

b_mat <- as.matrix(draws_df[, paste0('b[', seq_len(length(group_order)), ']'), drop = FALSE])

group_slope_tbl <- dplyr::bind_rows(lapply(seq_along(group_order), function(g) {
  dplyr::bind_cols(
    tibble(group = group_order[g]),
    summarise_draw_vector(b_mat[, g]) %>%
      rename(
        slope_dev_mean = mean,
        slope_dev_median = median,
        slope_dev_q025 = q025,
        slope_dev_q975 = q975
      ),
    tibble(prob_slope_dev_gt_0 = mean(b_mat[, g] > 0))
  )
})) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  arrange(desc(slope_dev_median))

selected_groups <- group_table %>%
  filter(group_collapsed != 'Outros') %>%
  slice_head(n = 6) %>%
  pull(group_collapsed)

if (length(selected_groups) == 0) {
  selected_groups <- group_table %>%
    slice_head(n = 6) %>%
    pull(group_collapsed)
}

group_curve_list <- vector('list', length(selected_groups))
for (i in seq_along(selected_groups)) {
  g_name <- selected_groups[i]
  g_id <- match(g_name, group_order)

  group_mu_mat <- sapply(seq_along(years_fit), function(t) {
    idx <- which(panel$year_id == t & panel$group_id == g_id)
    mu_mat[, idx]
  })

  if (is.vector(group_mu_mat)) group_mu_mat <- matrix(group_mu_mat, ncol = 1)

  group_curve_list[[i]] <- summarise_curve_matrix(group_mu_mat, years_fit, 'retraction_year') %>%
    mutate(group = g_name)
}

group_curves <- dplyr::bind_rows(group_curve_list)

group_obs <- panel %>%
  filter(as.character(group_collapsed) %in% selected_groups) %>%
  transmute(
    retraction_year = retraction_year,
    group = as.character(group_collapsed),
    y_obs = y
  )

group_curves <- group_curves %>%
  left_join(group_obs, by = c('retraction_year', 'group')) %>%
  mutate(across(c(mean, median, q025, q975), ~ round(.x, 2)))

group_last_year <- group_curves %>%
  filter(retraction_year == opt$end_year_complete) %>%
  arrange(desc(mean)) %>%
  select(group, retraction_year, y_obs, mean, q025, q975)

phi_summary <- summarise_draw_vector(draws_df$phi) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

key_params_tbl <- summary_tbl %>%
  filter(variable %in% c('alpha', 'sigma_a', 'sigma_b', 'sigma_rw', 'phi')) %>%
  transmute(
    parameter = variable,
    mean = round(mean, 4),
    sd = round(sd, 4),
    q2.5 = round(`2.5%`, 4),
    median = round(`50%`, 4),
    q97.5 = round(`97.5%`, 4),
    n_eff = round(n_eff, 1),
    Rhat = round(Rhat, 4)
  )

# -----------------------------------------------------------------------------
# Impressão final no console
# -----------------------------------------------------------------------------

print_section('RESUMO DA PREPARAÇÃO')
print(prep_summary, n = Inf, width = Inf)

cat('\nTop grupos usados no modelo:\n')
print(group_table %>% mutate(share = round(share, 4)), n = min(20L, nrow(group_table)), width = Inf)

print_section('DIAGNÓSTICOS MCMC')
print(diag_tbl, n = Inf, width = Inf)

print_section('PARÂMETROS-CHAVE')
print(key_params_tbl, n = Inf, width = Inf)

cat('\nResumo posterior de phi (superdispersão):\n')
print(phi_summary, n = Inf, width = Inf)

print_section('CURVA GLOBAL — ÚLTIMOS 10 ANOS')
print(global_curve %>% tail(10), n = Inf, width = Inf)

print_section('PPC GLOBAL — ÚLTIMOS 10 ANOS')
print(global_ppc %>% tail(10), n = Inf, width = Inf)

print_section('CONTRASTE GLOBAL: ÚLTIMOS 5 ANOS VS 5 ANOS ANTERIORES')
print(contrast_global, n = Inf, width = Inf)

print_section('DESVIO DE TENDÊNCIA POR GRUPO — TOP 10')
print(group_slope_tbl %>% slice_head(n = min(10L, nrow(group_slope_tbl))), n = Inf, width = Inf)

print_section(paste0('GRUPOS SELECIONADOS — ANO ', opt$end_year_complete))
print(group_last_year, n = Inf, width = Inf)

print_section('OBJETOS DISPONÍVEIS EM MEMÓRIA')
cat(
  paste(
    c(
      '- prep_summary',
      '- group_table',
      '- panel',
      '- fit',
      '- diag_tbl',
      '- key_params_tbl',
      '- global_curve',
      '- global_ppc',
      '- contrast_global',
      '- group_slope_tbl',
      '- group_curves',
      '- group_last_year'
    ),
    collapse = '\n'
  ),
  '\n',
  sep = ''
)

invisible(list(
  prep_summary = prep_summary,
  group_table = group_table,
  panel = panel,
  fit = fit,
  diag_tbl = diag_tbl,
  key_params_tbl = key_params_tbl,
  global_curve = global_curve,
  global_ppc = global_ppc,
  contrast_global = contrast_global,
  group_slope_tbl = group_slope_tbl,
  group_curves = group_curves,
  group_last_year = group_last_year
))
