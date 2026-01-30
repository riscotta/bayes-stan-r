############################################################
# BattingAverage.csv — 3 níveis: Geral -> Posição -> Jogador
# Console-only (nada é gravado em arquivo)
# Stan via cmdstanr
############################################################

# ----------------------------
# 0) Pacotes
# ----------------------------
pkgs <- c("cmdstanr", "posterior", "bayesplot", "dplyr", "readr", "tibble", "stringr", "loo")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install)

library(cmdstanr)
library(posterior)
library(bayesplot)
library(dplyr)
library(readr)
library(tibble)
library(stringr)
library(loo)

options(mc.cores = parallel::detectCores())
set.seed(123)

# ----------------------------
# 1) Ler dados
# ----------------------------
# Ajuste aqui se necessário:
data_file <- "C:\\Users\\rs44925\\Downloads\\BattingAverage.csv"
# data_file <- "BattingAverage.csv"

stopifnot(file.exists(data_file))

df <- read_csv(data_file, show_col_types = FALSE) %>%
  mutate(
    PriPos = as.factor(PriPos),
    PriPosNumber = as.integer(PriPosNumber),
    PlayerNumber = as.integer(PlayerNumber),
    BA_obs = Hits / AtBats
  )

# Checagens
stopifnot(all(df$AtBats >= 0), all(df$Hits >= 0), all(df$Hits <= df$AtBats))
stopifnot(!any(is.na(df$PriPosNumber)))

# Reindexar posições para 1..K (robusto se faltar alguma posição)
pos_levels <- sort(unique(df$PriPosNumber))
K <- length(pos_levels)

pos_map <- df %>%
  distinct(PriPosNumber, PriPos) %>%
  filter(PriPosNumber %in% pos_levels) %>%
  group_by(PriPosNumber) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(match(PriPosNumber, pos_levels))

df <- df %>%
  mutate(pos_id = match(PriPosNumber, pos_levels))

N <- nrow(df)
pos_names <- as.character(pos_map$PriPos)

cat("\n============================\n")
cat("Leitura do arquivo OK\n")
cat("N jogadores:", N, "\n")
cat("K posições :", K, "\n")
cat("============================\n\n")

# ----------------------------
# 2) EDA rápida (console)
# ----------------------------
eda_pos <- df %>%
  group_by(pos_id, PriPosNumber, PriPos) %>%
  summarise(
    n = n(),
    hits = sum(Hits),
    ab = sum(AtBats),
    BA_total = hits / ab,
    AB_mean = mean(AtBats),
    AB_med  = median(AtBats),
    BA_med  = median(BA_obs),
    .groups = "drop"
  ) %>%
  arrange(pos_id)

cat("=== Resumo por posição (observado) ===\n")
print(eda_pos)
cat("\n")

# ----------------------------
# 3) Stan — 3 níveis: geral -> posição -> jogador
# ----------------------------
stan_code <- "
data {
  int<lower=1> N;
  int<lower=1> K;
  array[N] int<lower=0> y;
  array[N] int<lower=0> ab;
  array[N] int<lower=1, upper=K> pos;
  int<lower=0, upper=1> prior_only;
}

parameters {
  // Nível geral para médias de posição
  real mu_alpha;
  real<lower=0> sigma_alpha;
  vector[K] alpha_raw;

  // Nível geral para dispersão dentro da posição (sigma_pos)
  real mu_log_sigma;
  real<lower=0> sigma_log_sigma;
  vector[K] sigma_raw;

  // Nível jogador
  vector[N] z_player;
}

transformed parameters {
  vector[K] alpha;
  vector<lower=0>[K] sigma_pos;
  vector[N] theta;
  vector[N] p;

  alpha = mu_alpha + sigma_alpha * alpha_raw;
  sigma_pos = exp(mu_log_sigma + sigma_log_sigma * sigma_raw);

  for (n in 1:N) {
    theta[n] = alpha[pos[n]] + sigma_pos[pos[n]] * z_player[n];
    p[n] = inv_logit(theta[n]);
  }
}

model {
  // Priors (bem razoáveis para BA em baseball; ajustáveis)
  mu_alpha ~ normal(logit(0.25), 1.0);   // BA típica ~ 0.25
  sigma_alpha ~ exponential(1.0);

  mu_log_sigma ~ normal(log(0.5), 0.75); // dispersão típica no logit (escala), fraca
  sigma_log_sigma ~ exponential(1.0);

  alpha_raw ~ normal(0, 1);
  sigma_raw ~ normal(0, 1);
  z_player ~ normal(0, 1);

  if (prior_only == 0) {
    y ~ binomial_logit(ab, theta);
  }
}

generated quantities {
  array[N] int y_rep;
  vector[N] log_lik;
  vector[K] p_pos_mean;

  for (n in 1:N) {
    y_rep[n] = binomial_rng(ab[n], p[n]);
    log_lik[n] = binomial_logit_lpmf(y[n] | ab[n], theta[n]);
  }

  for (k in 1:K) {
    p_pos_mean[k] = inv_logit(alpha[k]);
  }
}
"

# Como você pediu:
stan_file <- write_stan_file(stan_code)

mod <- cmdstan_model(stan_file)

# ----------------------------
# 4) Dados para Stan
# ----------------------------
stan_data <- list(
  N = N,
  K = K,
  y = df$Hits,
  ab = df$AtBats,
  pos = df$pos_id,
  prior_only = 0
)

# ----------------------------
# 5) Amostragem
# ----------------------------
fit <- mod$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  seed = 123,
  adapt_delta = 0.95,
  max_treedepth = 12,
  refresh = 200
)

# ----------------------------
# 6) RESULTADOS (console)
# ----------------------------
cat("\n============================\n")
cat("RESULTADOS — parâmetros globais e por posição\n")
cat("============================\n\n")

# Resumo dos principais parâmetros (PATCH robusto para q50)
sum_main_raw <- fit$summary(variables = c(
  "mu_alpha","sigma_alpha","mu_log_sigma","sigma_log_sigma",
  "alpha","sigma_pos","p_pos_mean"
))

# cmdstanr pode retornar 'median' em vez de 'q50' dependendo da versão
if (!("q50" %in% names(sum_main_raw)) && ("median" %in% names(sum_main_raw))) {
  sum_main_raw <- dplyr::rename(sum_main_raw, q50 = median)
}

sum_main <- sum_main_raw %>%
  select(variable, mean, sd, q5, q50, q95, rhat, ess_bulk, ess_tail)

print(sum_main)

# Posterior por posição (p_pos_mean)
ppos <- fit$draws("p_pos_mean", format = "draws_matrix")  # draws x K

pos_res <- tibble(
  pos_id = 1:K,
  PriPosNumber = pos_levels,
  PriPos = pos_names,
  mean = colMeans(ppos),
  q05  = apply(ppos, 2, quantile, 0.05),
  q50  = apply(ppos, 2, quantile, 0.50),
  q95  = apply(ppos, 2, quantile, 0.95)
) %>% arrange(desc(q50))

cat("\n=== Posição: habilidade média (p_pos_mean = inv_logit(alpha_pos)) ===\n")
print(pos_res)

# Ranking de jogadores por habilidade posterior média (p_i)
p_draws <- fit$draws("p", format = "draws_matrix")  # draws x N

df_players <- df %>%
  mutate(
    p_mean = colMeans(p_draws),
    p_q05  = apply(p_draws, 2, quantile, 0.05),
    p_q50  = apply(p_draws, 2, quantile, 0.50),
    p_q95  = apply(p_draws, 2, quantile, 0.95)
  ) %>%
  arrange(desc(p_mean)) %>%
  select(Player, PriPos, Hits, AtBats, BA_obs, p_mean, p_q05, p_q50, p_q95)

cat("\n=== Top 15 jogadores por p_mean (posterior) ===\n")
print(head(df_players, 15))

# ----------------------------
# 7) PPC e LOO (console)
# ----------------------------
cat("\n============================\n")
cat("PPC (checagem preditiva posterior) — console\n")
cat("============================\n\n")

yrep <- fit$draws("y_rep", format = "draws_matrix")  # draws x N

# BA replicado (para um subconjunto de draws)
set.seed(10)
nd <- nrow(yrep)
idx <- sample.int(nd, size = min(300, nd))
BA_rep <- sweep(yrep[idx, , drop = FALSE], 2, df$AtBats, "/")

obs_stats <- quantile(df$BA_obs, probs = c(.01,.05,.10,.25,.50,.75,.90,.95,.99), na.rm = TRUE)
rep_stats <- apply(BA_rep, 1, quantile, probs = c(.01,.05,.10,.25,.50,.75,.90,.95,.99), na.rm = TRUE)

cat("Quantis do BA observado:\n")
print(obs_stats)

cat("\nQuantis do BA replicado (mediana dos quantis entre draws):\n")
print(apply(rep_stats, 1, median))

cat("\nQuantis do BA replicado (intervalo 5%–95% dos quantis entre draws):\n")
q_rep_lo <- apply(rep_stats, 1, quantile, 0.05)
q_rep_hi <- apply(rep_stats, 1, quantile, 0.95)
print(rbind(q05=q_rep_lo, q95=q_rep_hi))

cat("\n============================\n")
cat("LOO (opcional, mas útil) — console\n")
cat("============================\n\n")

log_lik <- fit$draws("log_lik", format = "draws_matrix")
loo_res <- loo(log_lik)
print(loo_res)

# ----------------------------
# 8) CONTROLE DE QUALIDADE (console)
# ----------------------------
cat("\n============================\n")
cat("CONTROLE DE QUALIDADE (QC)\n")
cat("============================\n\n")

# 8.1 Diagnóstico do CmdStan (texto) — PATCH
cat("--- Diagnostic summary (cmdstanr) ---\n")
print(fit$diagnostic_summary())

cat("\n--- CmdStan diagnose utility (opcional) ---\n")
# Esse método imprime no console; em algumas versões retorna invisível/NULL.
# Não use print(fit$cmdstan_diagnose()) — chame direto:
fit$cmdstan_diagnose()

# 8.2 Contagens de divergências e treedepth
sd <- fit$sampler_diagnostics(format = "draws_array")

divergent <- sum(sd[, , "divergent__"])
treedepth_hit <- sum(sd[, , "treedepth__"] >= 12)  # max_treedepth = 12
cat("\n--- Sampler diagnostics ---\n")
cat("Divergências (divergent__):", divergent, "\n")
cat("Atingiu max_treedepth (>=12):", treedepth_hit, "\n")

# 8.3 E-BFMI por cadeia (regra prática: >= 0.3 é ok)
energy <- sd[, , "energy__"]
ebfmi_chain <- apply(energy, 2, function(e) mean(diff(e)^2) / var(e))
cat("\nE-BFMI por cadeia:\n")
print(ebfmi_chain)

# 8.4 Rhat/ESS globais (pior caso)
sum_all <- fit$summary()
max_rhat <- max(sum_all$rhat, na.rm = TRUE)
min_ess_bulk <- min(sum_all$ess_bulk, na.rm = TRUE)
min_ess_tail <- min(sum_all$ess_tail, na.rm = TRUE)

cat("\n--- Convergência e eficiência ---\n")
cat("Max R-hat:", max_rhat, "\n")
cat("Min ESS bulk:", min_ess_bulk, "\n")
cat("Min ESS tail:", min_ess_tail, "\n")

# 8.5 Interpretação simples do QC
cat("\n--- Interpretação rápida (heurística) ---\n")
if (divergent > 0) {
  cat("* ALERTA: há divergências. Sugestão: aumentar adapt_delta (ex: 0.99) e/ou reavaliar priors.\n")
} else {
  cat("* OK: zero divergências.\n")
}
if (treedepth_hit > 0) {
  cat("* ALERTA: atingiu max_treedepth. Sugestão: aumentar max_treedepth (ex: 15) e/ou melhorar parametrização/priors.\n")
} else {
  cat("* OK: treedepth não saturou.\n")
}
if (any(ebfmi_chain < 0.3)) {
  cat("* ALERTA: E-BFMI baixo em alguma cadeia (<0.3). Pode indicar má exploração da energia.\n")
} else {
  cat("* OK: E-BFMI adequado (>=0.3 em todas as cadeias).\n")
}
if (max_rhat > 1.01) {
  cat("* ALERTA: R-hat acima do ideal (>1.01). Considere mais amostras e checar parametrização.\n")
} else {
  cat("* OK: R-hat dentro do ideal (<=1.01).\n")
}
if (min_ess_bulk < 200 || min_ess_tail < 200) {
  cat("* ALERTA: ESS baixo (<200) em algum parâmetro. Considere mais iterações/amostras.\n")
} else {
  cat("* OK: ESS razoável (>=200) no pior caso.\n")
}

cat("\n============================\n")
cat("FIM\n")
cat("============================\n")
