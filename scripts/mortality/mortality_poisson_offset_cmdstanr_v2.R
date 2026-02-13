#!/usr/bin/env Rscript

############################################################
# Mortality — Poisson hierárquico com offset (população)
# Console-only (nada é gravado em arquivo)
# Stan via cmdstanr (Stan inline via write_stan_file)
#
# Rode a partir do ROOT do repo:
#   Rscript scripts/mortality/mortality_poisson_offset_cmdstanr_v2.R
#
# Opções (formato --chave=valor):
#   --excel_path=data/raw/mortality/Dados_Mortalidade.xlsx
#   --sheet_name=Resumo
#   --col_y=Contagem
#   --col_pop=POPULACAO
#   --col_mun=CODMUNRES
#
#   --seed=1234
#   --chains=4
#   --iter_warmup=1000
#   --iter_sampling=1000
#   --adapt_delta=0.95
#   --max_treedepth=12
#   --refresh=0
#
#   --threshold_prob=0.95
#   --alpha_scale=1.5
#   --sigma_scale=1.0
#
#   --show_full_table=0|1
#   --top_k=20
############################################################

# ----------------------------
# 0) Helpers (args + checks)
# ----------------------------
parse_args <- function(args) {
  out <- list(
    excel_path = file.path("data", "raw", "mortality", "Dados_Mortalidade.xlsx"),
    sheet_name = "Resumo",
    col_y   = "Contagem",
    col_pop = "POPULACAO",
    col_mun = "CODMUNRES",

    # MCMC
    seed = 1234L,
    chains = 4L,
    iter_warmup = 1000L,
    iter_sampling = 1000L,
    adapt_delta = 0.95,
    max_treedepth = 12L,
    refresh = 0L,

    # Regra de flag
    threshold_prob = 0.95,

    # Priors
    alpha_scale = 1.5,
    sigma_scale = 1.0,

    # Saídas
    show_full_table = 1L,   # 0|1 (padrão do repo)
    top_k = 20L
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

  # coerções
  out$seed <- as.integer(out$seed)
  out$chains <- as.integer(out$chains)
  out$iter_warmup <- as.integer(out$iter_warmup)
  out$iter_sampling <- as.integer(out$iter_sampling)
  out$adapt_delta <- as.numeric(out$adapt_delta)
  out$max_treedepth <- as.integer(out$max_treedepth)
  out$refresh <- as.integer(out$refresh)

  out$threshold_prob <- as.numeric(out$threshold_prob)
  out$alpha_scale <- as.numeric(out$alpha_scale)
  out$sigma_scale <- as.numeric(out$sigma_scale)

  out$show_full_table <- as.integer(out$show_full_table)
  out$top_k <- as.integer(out$top_k)

  out
}

require_pkgs <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Pacotes faltando: ", paste(missing, collapse = ", "),
      "\nRode primeiro: Rscript scripts/_setup/install_deps.R",
      call. = FALSE
    )
  }
}

check_cmdstan <- function() {
  ok <- TRUE
  tryCatch(cmdstanr::cmdstan_version(), error = function(e) ok <<- FALSE)
  if (!ok) {
    stop(
      "CmdStan não encontrado/configurado.\n",
      "Rode: Rscript scripts/_setup/install_cmdstan.R\n",
      "Depois tente novamente.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

args <- commandArgs(trailingOnly = TRUE)
cfg <- parse_args(args)

pkgs <- c("cmdstanr", "posterior", "dplyr", "tidyr", "ggplot2", "readxl", "tibble")
require_pkgs(pkgs)

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readxl)
  library(tibble)
})

options(mc.cores = parallel::detectCores())
set.seed(cfg$seed)
check_cmdstan()

# -----------------------------
# 1) CARREGAR E VALIDAR DADOS
# -----------------------------
if (!file.exists(cfg$excel_path)) {
  stop(
    "Arquivo Excel não encontrado: ", cfg$excel_path, "\n",
    "Sugestão: coloque o XLSX em data/raw/mortality/Dados_Mortalidade.xlsx\n",
    "ou rode com: --excel_path=caminho/para/Dados_Mortalidade.xlsx",
    call. = FALSE
  )
}

dados_raw <- read_excel(cfg$excel_path, sheet = cfg$sheet_name)

stopifnot(all(c(cfg$col_y, cfg$col_pop, cfg$col_mun) %in% names(dados_raw)))

df <- dados_raw %>%
  transmute(
    mun = as.character(.data[[cfg$col_mun]]),
    pop = as.numeric(.data[[cfg$col_pop]]),
    y   = as.integer(.data[[cfg$col_y]])
  )

# Checagens básicas
if (any(is.na(df$mun) | is.na(df$pop) | is.na(df$y))) {
  stop("Há NA em mun/pop/y. Limpe ou trate a planilha antes de rodar.", call. = FALSE)
}
if (any(df$pop <= 0)) stop("Há pop <= 0. Offset log(pop) exige pop > 0.", call. = FALSE)
if (any(df$y < 0))    stop("Há y < 0. Contagens devem ser >= 0.", call. = FALSE)

# Se houver município repetido, agrego por muni.
dup_mun <- any(duplicated(df$mun))
if (dup_mun) {
  df <- df %>%
    group_by(mun) %>%
    summarise(
      pop = sum(pop),
      y   = sum(y),
      .groups = "drop"
    )
}

N <- nrow(df)
y <- df$y
pop <- df$pop
mun <- df$mun

crude_rate_obs <- sum(y) / sum(pop)
crude_for_prior <- max(crude_rate_obs, 1e-12)
alpha_loc <- log(crude_for_prior)

cat("\n==============================\n")
cat("V2 — MODELO BAYESIANO HIERÁRQUICO (POISSON + OFFSET)\n")
cat("==============================\n")
cat("Arquivo Excel :", cfg$excel_path, "\n")
cat("Sheet        :", cfg$sheet_name, "\n")
cat("N municípios :", N, "\n")
cat("Duplicados agregados:", dup_mun, "\n")
cat("Crude rate   :", format(crude_rate_obs, digits = 10), "\n")
cat("==============================\n\n")

# -----------------------------
# 2) MODELO STAN (NÃO-CENTRADO) + PPC
# -----------------------------
stan_code_v2 <- "
data {
  int<lower=1> N;
  array[N] int<lower=0> y;
  vector<lower=0>[N] pop;

  real alpha_loc;
  real<lower=0> alpha_scale;
  real<lower=0> sigma_scale;
}
parameters {
  real alpha;               // intercept: log baseline rate per person
  vector[N] eta_raw;        // non-centered RE
  real<lower=0> sigma;      // sd do RE
}
transformed parameters {
  vector[N] eta;
  vector[N] log_lambda;
  eta = sigma * eta_raw;
  log_lambda = alpha + eta;
}
model {
  // priors
  alpha ~ normal(alpha_loc, alpha_scale);
  sigma ~ normal(0, sigma_scale);      // half-normal via truncamento em 0
  eta_raw ~ normal(0, 1);

  // likelihood with offset log(pop)
  y ~ poisson_log(log_lambda + log(pop));
}
generated quantities {
  vector[N] rate;
  array[N] int y_rep;
  vector[N] log_lik;

  for (i in 1:N) {
    rate[i] = exp(log_lambda[i]);
    y_rep[i] = poisson_log_rng(log_lambda[i] + log(pop[i]));
    log_lik[i] = poisson_log_lpmf(y[i] | log_lambda[i] + log(pop[i]));
  }
}
"

mod <- cmdstan_model(write_stan_file(stan_code_v2), quiet = TRUE)

stan_data <- list(
  N = N,
  y = as.integer(y),
  pop = as.numeric(pop),
  alpha_loc = alpha_loc,
  alpha_scale = cfg$alpha_scale,
  sigma_scale = cfg$sigma_scale
)

fit <- mod$sample(
  data = stan_data,
  seed = cfg$seed,
  chains = cfg$chains,
  parallel_chains = cfg$chains,
  iter_warmup = cfg$iter_warmup,
  iter_sampling = cfg$iter_sampling,
  adapt_delta = cfg$adapt_delta,
  max_treedepth = cfg$max_treedepth,
  refresh = cfg$refresh,
  output_dir = tempdir()
)

# -----------------------------
# 3) EXTRAIR DRAWS E DIAGNÓSTICOS
# -----------------------------
fit_summary <- fit$summary()
diag_sum <- fit$diagnostic_summary()

sd_df <- as_draws_df(fit$sampler_diagnostics())

n_div <- sum(sd_df$divergent__)
max_td <- max(sd_df$treedepth__)
td_limit_hits <- sum(sd_df$treedepth__ >= cfg$max_treedepth)

# BFMI por cadeia (energia): regra prática: < 0.3 é suspeito
bfmi_by_chain <- function(energy_vec) {
  numer <- mean(diff(energy_vec)^2)
  denom <- var(energy_vec)
  numer / denom
}

bfmi_tbl <- sd_df %>%
  as_tibble() %>%
  group_by(.chain) %>%
  summarise(bfmi = bfmi_by_chain(energy__), .groups = "drop") %>%
  arrange(.chain)

draws_alpha_sigma <- as_draws_df(fit$draws(variables = c("alpha", "sigma")))
alpha_draw <- draws_alpha_sigma$alpha
sigma_draw <- draws_alpha_sigma$sigma

rate_mat <- fit$draws(variables = "rate", format = "draws_matrix")
colnames(rate_mat) <- gsub("^rate\\[|\\]$", "", colnames(rate_mat))

yrep_mat <- fit$draws(variables = "y_rep", format = "draws_matrix")
colnames(yrep_mat) <- gsub("^y_rep\\[|\\]$", "", colnames(yrep_mat))

# -----------------------------
# 4) BENCHMARKS E PROBABILIDADES DE EXCESSO
# -----------------------------
state_median_draw <- exp(alpha_draw)
state_mean_draw   <- exp(alpha_draw + 0.5 * sigma_draw^2)

prob_above_crude <- colMeans(rate_mat > crude_rate_obs)
prob_above_state_mean <- colMeans(rate_mat > state_mean_draw)

rate_summ <- tibble(
  mun_idx = 1:N,
  post_mean   = colMeans(rate_mat),
  post_median = apply(rate_mat, 2, median),
  post_sd     = apply(rate_mat, 2, sd),
  post_q025   = apply(rate_mat, 2, quantile, probs = 0.025),
  post_q975   = apply(rate_mat, 2, quantile, probs = 0.975),
  prob_above_state = as.numeric(prob_above_state_mean),
  prob_above_crude = as.numeric(prob_above_crude)
) %>%
  mutate(flag = prob_above_state > cfg$threshold_prob) %>%
  arrange(desc(prob_above_state))

results <- df %>%
  mutate(
    mun_idx = 1:N,
    obs_rate = y / pop
  ) %>%
  left_join(rate_summ, by = "mun_idx") %>%
  mutate(
    post_median_mu = pop * post_median,
    rr_vs_crude = post_median / crude_rate_obs
  ) %>%
  select(
    mun, pop, y, obs_rate,
    post_median, post_mean, post_sd, post_q025, post_q975,
    prob_above_state, prob_above_crude, rr_vs_crude,
    flag
  )

# -----------------------------
# 5) PPC (POSTERIOR PREDICTIVE CHECKS)
# -----------------------------
ppc_tbl <- tibble(
  mun_idx = 1:N,
  y_obs = y,
  yrep_mean = colMeans(yrep_mat),
  yrep_q025 = apply(yrep_mat, 2, quantile, probs = 0.025),
  yrep_q975 = apply(yrep_mat, 2, quantile, probs = 0.975),
  p_hi = colMeans(yrep_mat >= y),
  p_lo = colMeans(yrep_mat <= y)
) %>%
  mutate(
    p_two = 2 * pmin(p_hi, p_lo),
    covered_95 = (y_obs >= yrep_q025) & (y_obs <= yrep_q975)
  )

results <- results %>%
  mutate(mun_idx = 1:N) %>%
  left_join(ppc_tbl %>% select(mun_idx, yrep_q025, yrep_q975, p_two, covered_95), by = "mun_idx") %>%
  select(-mun_idx)

y_total_rep <- rowSums(yrep_mat)
y_total_obs <- sum(y)

zero_prop_rep <- rowMeans(yrep_mat == 0)
zero_prop_obs <- mean(y == 0)

ppc_global <- list(
  total_obs = y_total_obs,
  total_rep_mean = mean(y_total_rep),
  total_rep_q = quantile(y_total_rep, probs = c(0.025, 0.5, 0.975)),
  p_total_hi = mean(y_total_rep >= y_total_obs),
  p_total_lo = mean(y_total_rep <= y_total_obs),
  zero_prop_obs = zero_prop_obs,
  zero_prop_rep_mean = mean(zero_prop_rep),
  zero_prop_rep_q = quantile(zero_prop_rep, probs = c(0.025, 0.5, 0.975))
)

# -----------------------------
# 6) SUMÁRIOS DO "ESTADO"
# -----------------------------
state_tbl <- tibble(
  state_median_rate = state_median_draw,
  state_mean_rate   = state_mean_draw
)

state_summary <- list(
  crude_rate_obs = crude_rate_obs,
  alpha_posterior = list(
    mean = mean(alpha_draw),
    q = quantile(alpha_draw, c(0.025, 0.5, 0.975))
  ),
  sigma_posterior = list(
    mean = mean(sigma_draw),
    q = quantile(sigma_draw, c(0.025, 0.5, 0.975))
  ),
  state_median_rate = list(
    mean = mean(state_tbl$state_median_rate),
    q = quantile(state_tbl$state_median_rate, c(0.025, 0.5, 0.975))
  ),
  state_mean_rate = list(
    mean = mean(state_tbl$state_mean_rate),
    q = quantile(state_tbl$state_mean_rate, c(0.025, 0.5, 0.975))
  )
)

# -----------------------------
# 7) PLOTS (OBJETOS) — serão impressos no final
# -----------------------------
plot_rate_vs_pop <- ggplot(results, aes(x = pop, y = post_median, color = flag)) +
  geom_point(alpha = 0.85) +
  scale_x_log10() +
  labs(
    x = "População (log10)",
    y = "Taxa posterior (mediana)",
    title = "Taxas hierárquicas por município (mediana posterior)",
    subtitle = sprintf(
      "Flag: P(rate_i > state_mean_draw) > %.2f | crude=%.6g",
      cfg$threshold_prob, crude_rate_obs
    )
  ) +
  theme_minimal()

plot_prob_hist <- ggplot(results, aes(x = prob_above_state)) +
  geom_histogram(bins = 30) +
  labs(
    x = "P(rate_i > state_mean_draw)",
    y = "Nº municípios",
    title = "Distribuição das probabilidades posteriores de excesso (benchmark Bayesiano)"
  ) +
  theme_minimal()

# -----------------------------
# 8) OUTPUTS AGRUPADOS NO FINAL (CONSOLE)
# -----------------------------
out <- list()

out$data_summary <- list(
  N = N,
  duplicated_municipality_aggregated = dup_mun,
  total_pop = sum(pop),
  total_y = sum(y),
  crude_rate_obs = crude_rate_obs
)

par_focus <- fit_summary %>%
  filter(variable %in% c("alpha", "sigma")) %>%
  select(any_of(c("variable", "mean", "sd", "q5", "median", "q95", "rhat", "ess_bulk", "ess_tail")))

rhat_overview <- fit_summary %>%
  filter(!is.na(rhat)) %>%
  summarise(
    rhat_max = max(rhat),
    rhat_p99 = quantile(rhat, 0.99),
    rhat_p95 = quantile(rhat, 0.95),
    frac_rhat_gt_1_01 = mean(rhat > 1.01),
    frac_rhat_gt_1_05 = mean(rhat > 1.05)
  )

out$model_diagnostics <- list(
  cmdstan_diagnostic_summary = diag_sum,
  n_divergences = n_div,
  max_treedepth = max_td,
  treedepth_limit_hits = td_limit_hits,
  bfmi_by_chain = bfmi_tbl,
  rhat_overview = rhat_overview,
  param_focus = par_focus
)

out$top_by_excess <- results %>%
  arrange(desc(prob_above_state)) %>%
  slice_head(n = min(cfg$top_k, N))

out$top_ppc_weird <- results %>%
  arrange(p_two) %>%
  slice_head(n = min(cfg$top_k, N))

out$flags <- list(
  threshold = cfg$threshold_prob,
  n_flagged = sum(results$flag),
  frac_flagged = mean(results$flag)
)

out$state_summary <- state_summary
out$ppc_global <- ppc_global
out$results_table <- results

# -----------------------------
# 9) PRINT FINAL (TUDO JUNTO)
# -----------------------------
cat("\n--- 1) RESUMO DOS DADOS ---\n")
print(out$data_summary)

cat("\n--- 2) DIAGNÓSTICOS DO AJUSTE (MCMC) ---\n")
cat("\n> Parâmetros globais (alpha, sigma):\n")
print(out$model_diagnostics$param_focus)

cat("\n> Visão geral de Rhat:\n")
print(out$model_diagnostics$rhat_overview)

cat("\n> Divergências / Treedepth:\n")
print(list(
  n_divergences = out$model_diagnostics$n_divergences,
  max_treedepth = out$model_diagnostics$max_treedepth,
  treedepth_limit_hits = out$model_diagnostics$treedepth_limit_hits
))

cat("\n> BFMI por cadeia (regra prática: < 0.30 é suspeito):\n")
print(out$model_diagnostics$bfmi_by_chain)

cat("\n--- 3) RESUMO 'ESTADO' / BENCHMARKS ---\n")
cat(sprintf("\nCrude rate observada (global): %.8g\n", out$state_summary$crude_rate_obs))

cat("\nAlpha (log-rate baseline) posterior:\n")
print(out$state_summary$alpha_posterior)

cat("\nSigma (heterogeneidade entre municípios) posterior:\n")
print(out$state_summary$sigma_posterior)

cat("\nTaxa 'típica' (mediana marginal ~ exp(alpha)):\n")
print(out$state_summary$state_median_rate)

cat("\nTaxa média marginal entre municípios (E[rate] = exp(alpha + 0.5*sigma^2)):\n")
print(out$state_summary$state_mean_rate)

cat("\n--- 4) FLAGS (EXCESSO) ---\n")
cat(sprintf("\nRegra: flag = P(rate_i > state_mean_draw) > %.2f\n", out$flags$threshold))
print(out$flags)

cat("\n--- 5) TOP MUNICÍPIOS POR EXCESSO (P(rate_i > state_mean_draw)) ---\n")
print(out$top_by_excess)

cat("\n--- 6) PPC GLOBAL ---\n")
cat("\nTotal de óbitos:\n")
print(list(
  obs = out$ppc_global$total_obs,
  rep_mean = out$ppc_global$total_rep_mean,
  rep_q025 = out$ppc_global$total_rep_q[[1]],
  rep_q50  = out$ppc_global$total_rep_q[[2]],
  rep_q975 = out$ppc_global$total_rep_q[[3]],
  p_hi = out$ppc_global$p_total_hi,
  p_lo = out$ppc_global$p_total_lo
))

cat("\nProporção de municípios com zero óbitos:\n")
print(list(
  obs = out$ppc_global$zero_prop_obs,
  rep_mean = out$ppc_global$zero_prop_rep_mean,
  rep_q025 = out$ppc_global$zero_prop_rep_q[[1]],
  rep_q50  = out$ppc_global$zero_prop_rep_q[[2]],
  rep_q975 = out$ppc_global$zero_prop_rep_q[[3]]
))

cat("\n--- 7) TOP 'ESTRANHOS' NO PPC (menor p_two) ---\n")
print(out$top_ppc_weird)

cat("\n--- 8) TABELA COMPLETA (RESULTADOS) ---\n")
if (cfg$show_full_table == 1L) {
  print(out$results_table, n = N)
} else {
  print(out$results_table, n = min(50, N))
  cat("\n(Dica: --show_full_table=1 para imprimir tudo.)\n")
}

cat("\n--- 9) PLOTS (serão desenhados no device gráfico) ---\n")
print(plot_rate_vs_pop)
print(plot_prob_hist)

cat("\n==============================\n")
cat("FIM — V2\n")
cat("==============================\n")
