# bayes_mortality_cmdstanr_real_V2.R
# V2 — Modelo Bayesiano hierárquico Poisson com offset (população)
# Melhorias vs V1:
#   - parametrização não-centrada (melhor amostragem)
#   - priors configuráveis (alpha centrado na crude rate, com clamp)
#   - diagnósticos completos (Rhat/ESS + divergências + treedepth + BFMI)
#   - benchmark Bayesiano: P(rate_i > state_mean_draw) onde state_mean_draw = exp(alpha + 0.5*sigma^2)
#   - PPC: y_rep por município + p-values e checagens globais
#   - NADA é gravado em disco pelo script (sem CSV/plots exportados).
#     Obs: cmdstanr/Stan por natureza usa arquivos temporários para compilar/rodar; aqui vai para tempdir().

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readxl)
})

# -----------------------------
# 0) CONFIG
# -----------------------------
cfg <- list(
  excel_path = "C:\\Users\\rs44925\\Downloads\\projetos\\SIM\\Dados.xlsx",
  sheet_name = "Resumo",
  col_y   = "Contagem",
  col_pop = "POPULACAO",
  col_mun = "CODMUNRES",
  
  # MCMC
  seed = 1234,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  adapt_delta = 0.95,
  max_treedepth = 12,
  
  # Regra de flag
  threshold_prob = 0.95,
  
  # Priors (alpha será centrado na crude rate; veja abaixo)
  alpha_scale = 1.5,   # prior sd no log-rate
  sigma_scale = 1.0,   # half-normal efetivo: sigma ~ Normal(0, sigma_scale) truncado em 0
  
  # PPC / saídas
  show_full_table = TRUE,     # TRUE imprime a tabela inteira (pode ser grandeo se N for grande)
  top_k = 20                  # Top K para rankings/resumos
)

# -----------------------------
# 1) CARREGAR E VALIDAR DADOS
# -----------------------------
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
  stop("Há NA em mun/pop/y. Limpe ou trate a planilha antes de rodar.")
}
if (any(df$pop <= 0)) stop("Há pop <= 0. Offset log(pop) exige pop > 0.")
if (any(df$y < 0))    stop("Há y < 0. Contagens devem ser >= 0.")

# Se houver município repetido, agrego por muni (melhor do que tratar cada linha como um município distinto).
# População aqui entra como 'exposição' no período/recorte da linha; somar é o default mais defensável.
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
# clamp para evitar log(0) se não houver óbitos (caso raro, mas possível)
crude_for_prior <- max(crude_rate_obs, 1e-12)

# Prior para alpha centrado na crude (data-informed, mas ainda bem amplo).
alpha_loc <- log(crude_for_prior)

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
  refresh = 0,                    # reduz spam no console
  output_dir = tempdir()          # arquivos temporários do CmdStan
)

# -----------------------------
# 3) EXTRAIR DRAWS E DIAGNÓSTICOS
# -----------------------------
# Sumário padrão: Rhat/ESS
fit_summary <- fit$summary()

# Diagnósticos do sampler (divergências, treedepth, BFMI)
diag_sum <- fit$diagnostic_summary()

# Sampler diagnostics (para métricas detalhadas)
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
# separa por cadeia usando .chain (coluna do posterior draws_df do sampler_diagnostics)
bfmi_tbl <- sd_df %>%
  as_tibble() %>%
  group_by(.chain) %>%
  summarise(bfmi = bfmi_by_chain(energy__), .groups = "drop") %>%
  arrange(.chain)

# Draws principais
draws_alpha_sigma <- as_draws_df(fit$draws(variables = c("alpha", "sigma")))
alpha_draw <- draws_alpha_sigma$alpha
sigma_draw <- draws_alpha_sigma$sigma

# Taxas por município (matriz iters x N)
rate_mat <- fit$draws(variables = "rate", format = "draws_matrix")
colnames(rate_mat) <- gsub("^rate\\[|\\]$", "", colnames(rate_mat))

# PPC: y_rep (matriz iters x N)
yrep_mat <- fit$draws(variables = "y_rep", format = "draws_matrix")
colnames(yrep_mat) <- gsub("^y_rep\\[|\\]$", "", colnames(yrep_mat))

# -----------------------------
# 4) BENCHMARKS E PROBABILIDADES DE EXCESSO
# -----------------------------
# Benchmark Bayesiano:
#   state_median_draw = exp(alpha)
#   state_mean_draw   = exp(alpha + 0.5*sigma^2)  (média marginal sobre eta ~ N(0,sigma))
state_median_draw <- exp(alpha_draw)
state_mean_draw   <- exp(alpha_draw + 0.5 * sigma_draw^2)

# P(rate_i > crude) e P(rate_i > state_mean_draw)
prob_above_crude <- colMeans(rate_mat > crude_rate_obs)
prob_above_state_mean <- colMeans(rate_mat > state_mean_draw)  # reciclagem por linha funciona

# Sumarização por município (taxa)
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

# Junta com dados observados
results <- df %>%
  mutate(
    mun_idx = 1:N,
    obs_rate = y / pop
  ) %>%
  left_join(rate_summ, by = "mun_idx") %>%
  mutate(
    # expectativa posterior (usando mediana da taxa)
    post_median_mu = pop * post_median,
    # "razão" simples vs crude
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
# PPC por município:
# - intervalo preditivo 95% de y_rep
# - p-values (one-sided e two-sided)
ppc_tbl <- tibble(
  mun_idx = 1:N,
  y_obs = y,
  yrep_mean = colMeans(yrep_mat),
  yrep_q025 = apply(yrep_mat, 2, quantile, probs = 0.025),
  yrep_q975 = apply(yrep_mat, 2, quantile, probs = 0.975),
  p_hi = colMeans(yrep_mat >= y),   # P(y_rep >= y_obs)
  p_lo = colMeans(yrep_mat <= y)    # P(y_rep <= y_obs)
) %>%
  mutate(
    p_two = 2 * pmin(p_hi, p_lo),
    covered_95 = (y_obs >= yrep_q025) & (y_obs <= yrep_q975)
  )

results <- results %>%
  mutate(mun_idx = 1:N) %>%
  left_join(ppc_tbl %>% select(mun_idx, yrep_q025, yrep_q975, p_two, covered_95), by = "mun_idx") %>%
  select(-mun_idx)

# PPC global:
# - total de óbitos (distribuição preditiva)
# - proporção de zeros por município (distribuição)
y_total_rep <- rowSums(yrep_mat)
y_total_obs <- sum(y)

zero_prop_rep <- rowMeans(yrep_mat == 0)  # por draw: fração de municípios com 0
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
    subtitle = sprintf("Flag: P(rate_i > state_mean_draw) > %.2f | crude=%.6g", cfg$threshold_prob, crude_rate_obs)
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

# Diagnósticos principais de convergência (Rhat/ESS) — foco nos parâmetros globais e dispersão geral
par_focus <- fit_summary %>%
  filter(variable %in% c("alpha", "sigma")) %>%
  select(any_of(c("variable", "mean", "sd", "q5", "median", "q95", "rhat", "ess_bulk", "ess_tail")))

# Visão geral de Rhat e divergências
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

# Ranking: top municípios por prob_above_state
out$top_by_excess <- results %>%
  arrange(desc(prob_above_state)) %>%
  slice_head(n = min(cfg$top_k, N))

# Ranking: mais "estranhos" no PPC (p_two muito pequeno)
out$top_ppc_weird <- results %>%
  arrange(p_two) %>%
  slice_head(n = min(cfg$top_k, N))

# Contagem de flags
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
cat("\n==============================\n")
cat("V2 — MODELO BAYESIANO HIERÁRQUICO (POISSON + OFFSET)\n")
cat("==============================\n")

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
if (isTRUE(cfg$show_full_table)) {
  print(out$results_table, n = N)
} else {
  print(out$results_table, n = min(50, N))
  cat("\n(Dica: cfg$show_full_table = TRUE para imprimir tudo.)\n")
}

cat("\n--- 9) PLOTS (serão desenhados no device gráfico) ---\n")
print(plot_rate_vs_pop)
print(plot_prob_hist)

cat("\n==============================\n")
cat("FIM — V2\n")
cat("==============================\n")
