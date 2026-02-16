#!/usr/bin/env Rscript

############################################################
# Hidrômetro — degradação / erro de medição (hierárquico)
# Stan via cmdstanr
#
# - O script simula um dataset sintético e ajusta um modelo Bayesiano.
# - Por padrão: console-only (não grava arquivos).
# - Opcionalmente: salva PDF (plots) e/ou TXT (resumo) em outputs/.
#
# Rode a partir do ROOT do repo (sem setwd()):
#   Rscript scripts/hidrometro_degradacao/hidrometro_degradacao_cmdstanr.R
#
# Opções (formato --chave=valor):
#   --seed=20260213
#   --N=1800
#   --K_model=8
#   --K_region=12
#   --age_max_years=16
#   --ppc_draws=250
#
#   --chains=4
#   --iter_warmup=1000
#   --iter_sampling=1000
#   --adapt_delta=0.95
#   --max_treedepth=12
#   --refresh=100
#
#   --save_plots=0|1
#   --plots_path=outputs/figures/hidrometro_degradacao_plots.pdf
#   --save_report=0|1
#   --report_path=outputs/tables/hidrometro_degradacao_report.txt
############################################################

# ----------------------------
# 0) Helpers (args + checks)
# ----------------------------
parse_args <- function(args) {
  out <- list(
    # simulação
    seed = 20260213L,
    N = 1800L,
    K_model = 8L,
    K_region = 12L,
    age_max_years = 16,
    ppc_draws = 250L,

    # sampler
    chains = 4L,
    iter_warmup = 1000L,
    iter_sampling = 1000L,
    adapt_delta = 0.95,
    max_treedepth = 12L,
    refresh = 100L,

    # outputs
    save_plots = 0L,
    plots_path = file.path("outputs", "figures", "hidrometro_degradacao_plots.pdf"),
    save_report = 0L,
    report_path = file.path("outputs", "tables", "hidrometro_degradacao_report.txt")
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
  out$N <- as.integer(out$N)
  out$K_model <- as.integer(out$K_model)
  out$K_region <- as.integer(out$K_region)
  out$age_max_years <- as.numeric(out$age_max_years)
  out$ppc_draws <- as.integer(out$ppc_draws)

  out$chains <- as.integer(out$chains)
  out$iter_warmup <- as.integer(out$iter_warmup)
  out$iter_sampling <- as.integer(out$iter_sampling)
  out$adapt_delta <- as.numeric(out$adapt_delta)
  out$max_treedepth <- as.integer(out$max_treedepth)
  out$refresh <- as.integer(out$refresh)

  out$save_plots <- as.integer(out$save_plots)
  out$save_report <- as.integer(out$save_report)

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

bfmi <- function(energy) {
  if (length(energy) < 3) return(NA_real_)
  num <- mean(diff(energy)^2)
  den <- stats::var(energy)
  ifelse(den <= 0, NA_real_, num / den)
}

qtiles <- function(x) {
  qs <- stats::quantile(x, probs = c(0.05, 0.5, 0.95), names = FALSE)
  c(mean = mean(x), sd = sd(x), q05 = qs[1], q50 = qs[2], q95 = qs[3])
}

summ_par <- function(draws, name, truth = NA_real_) {
  out <- qtiles(draws)
  tibble::tibble(
    param = name,
    mean  = out[["mean"]],
    sd    = out[["sd"]],
    q05   = out[["q05"]],
    q50   = out[["q50"]],
    q95   = out[["q95"]],
    truth = truth
  )
}

safe_dir_create <- function(path) {
  dirp <- dirname(path)
  if (!dir.exists(dirp)) dir.create(dirp, recursive = TRUE, showWarnings = FALSE)
}

# ----------------------------
# 0.1) Bootstrap
# ----------------------------
args <- commandArgs(trailingOnly = TRUE)
opt <- parse_args(args)

pkgs <- c("cmdstanr", "posterior", "bayesplot", "ggplot2", "dplyr", "tibble", "stringr")
require_pkgs(pkgs)

library(cmdstanr)
library(posterior)
library(bayesplot)
library(ggplot2)
library(dplyr)
library(tibble)
library(stringr)

options(mc.cores = max(1L, parallel::detectCores()))
set.seed(opt$seed)

check_cmdstan()

# ----------------------------
# 1) Simulação de dados
# ----------------------------
# Interpretação:
# y_err = erro percentual de medição em aferição (negativo = submedição)
# mu_i = intercepto por modelo + efeito idade + efeito log(volume acumulado) + efeito região
# y ~ Normal(mu, sigma_y)

K_model  <- opt$K_model
K_region <- opt$K_region
N        <- opt$N

# IDs
model_id  <- sample.int(K_model,  N, replace = TRUE)
region_id <- sample.int(K_region, N, replace = TRUE)

# Covariáveis
age_years <- runif(N, min = 0, max = opt$age_max_years)

# volume acumulado correlacionado com idade (aprox.)
annual_use_m3 <- rgamma(N, shape = 10, rate = 10/180)     # média ~180 m3/ano
cum_volume_m3 <- pmax(10, age_years * annual_use_m3 + rnorm(N, 0, 60))
log_vol <- log(cum_volume_m3)

# "Verdade" (parâmetros reais) — apenas para validação do pipeline
true <- list(
  alpha0       = -0.5,   # baseline (%)
  beta_age     = -0.18,  # piora por ano (%/ano)
  beta_logvol  = -0.65,  # piora por log(volume)
  sigma_y      = 1.10,   # ruído de aferição
  sigma_model  = 1.25,   # variação entre modelos
  sigma_region = 0.60    # variação entre regiões
)

alpha_model_true <- true$alpha0 + rnorm(K_model, 0, true$sigma_model)
u_region_true    <- rnorm(K_region, 0, true$sigma_region)

age_c    <- age_years - mean(age_years)
logvol_c <- log_vol   - mean(log_vol)

mu_true <- alpha_model_true[model_id] +
  true$beta_age    * age_c +
  true$beta_logvol * logvol_c +
  u_region_true[region_id]

y_err <- rnorm(N, mean = mu_true, sd = true$sigma_y)

# clip leve só para não aparecer coisa absurda na simulação
y_err <- pmin(pmax(y_err, -20), 12)

df <- tibble(
  id        = 1:N,
  y_err     = y_err,
  age_years = age_years,
  cum_m3    = cum_volume_m3,
  log_vol   = log_vol,
  model_id  = model_id,
  region_id = region_id
) %>%
  mutate(
    model  = factor(model_id,  levels = 1:K_model,  labels = paste0("M", 1:K_model)),
    region = factor(region_id, levels = 1:K_region, labels = paste0("R", 1:K_region))
  )

stan_data <- list(
  N         = N,
  K_model   = K_model,
  K_region  = K_region,
  y         = df$y_err,
  age       = df$age_years,
  logvol    = df$log_vol,
  model_id  = df$model_id,
  region_id = df$region_id
)

# ----------------------------
# 2) Modelo Stan (inline) + ajuste (cmdstanr)
# ----------------------------
stan_code <- "
data {
  int<lower=1> N;
  int<lower=1> K_model;
  int<lower=1> K_region;

  vector[N] y;
  vector[N] age;
  vector[N] logvol;

  array[N] int<lower=1, upper=K_model> model_id;
  array[N] int<lower=1, upper=K_region> region_id;
}

transformed data {
  real age_mean = mean(age);
  real logvol_mean = mean(logvol);

  vector[N] age_c    = age    - age_mean;
  vector[N] logvol_c = logvol - logvol_mean;
}

parameters {
  real alpha0;
  real beta_age;
  real beta_logvol;

  vector[K_model]  alpha_model_raw;
  real<lower=0>    sigma_model;

  vector[K_region] u_region_raw;
  real<lower=0>    sigma_region;

  real<lower=0> sigma_y;
}

transformed parameters {
  vector[K_model]  alpha_model = alpha0 + sigma_model * alpha_model_raw;
  vector[K_region] u_region    = sigma_region * u_region_raw;
}

model {
  // Priors (moderadamente informativos)
  alpha0      ~ normal(0, 5);
  beta_age    ~ normal(0, 1);
  beta_logvol ~ normal(0, 1);

  alpha_model_raw ~ normal(0, 1);
  u_region_raw    ~ normal(0, 1);

  sigma_model  ~ exponential(1);
  sigma_region ~ exponential(1);
  sigma_y      ~ exponential(1);

  for (n in 1:N) {
    real mu = alpha_model[model_id[n]]
            + beta_age    * age_c[n]
            + beta_logvol * logvol_c[n]
            + u_region[region_id[n]];
    y[n] ~ normal(mu, sigma_y);
  }
}

generated quantities {
  vector[N] mu_hat;
  vector[N] y_rep;

  for (n in 1:N) {
    mu_hat[n] = alpha_model[model_id[n]]
              + beta_age    * age_c[n]
              + beta_logvol * logvol_c[n]
              + u_region[region_id[n]];
    y_rep[n] = normal_rng(mu_hat[n], sigma_y);
  }
}
";

stan_file <- cmdstanr::write_stan_file(stan_code)
mod <- cmdstanr::cmdstan_model(stan_file)

fit <- mod$sample(
  data = stan_data,
  seed = opt$seed,
  chains = opt$chains,
  parallel_chains = opt$chains,
  iter_warmup = opt$iter_warmup,
  iter_sampling = opt$iter_sampling,
  refresh = opt$refresh,
  adapt_delta = opt$adapt_delta,
  max_treedepth = opt$max_treedepth
)

# ----------------------------
# 3) Diagnósticos do sampler (divergências, treedepth, BFMI)
# ----------------------------
sd_df <- posterior::as_draws_df(fit$sampler_diagnostics())

diag_summary <- sd_df %>%
  group_by(.chain) %>%
  summarise(
    n = dplyr::n(),
    divergences = sum(.data$divergent__),
    max_treedepth = max(.data$treedepth__),
    bfmi = bfmi(.data$energy__),
    .groups = "drop"
  ) %>%
  mutate(chain = as.integer(.chain)) %>%
  select(chain, n, divergences, max_treedepth, bfmi)

sampler_overall <- diag_summary %>%
  summarise(
    chains = n(),
    total_divergences = sum(divergences),
    max_treedepth = max(max_treedepth),
    min_bfmi = min(bfmi, na.rm = TRUE)
  )

# ----------------------------
# 4) Draws + diagnósticos MCMC (Rhat/ESS)
# ----------------------------
draws_all <- fit$draws()

diag_mcmc <- posterior::summarise_draws(
  draws_all,
  posterior::rhat,
  posterior::ess_bulk,
  posterior::ess_tail
) %>%
  as_tibble()

pick_first_col <- function(df, pattern) {
  nm <- names(df)
  hit <- nm[stringr::str_detect(nm, pattern)]
  if (length(hit) == 0) NULL else hit[1]
}

rhat_col     <- pick_first_col(diag_mcmc, "rhat")
ess_bulk_col <- pick_first_col(diag_mcmc, "ess_bulk")
ess_tail_col <- pick_first_col(diag_mcmc, "ess_tail")

if (is.null(rhat_col) || is.null(ess_bulk_col) || is.null(ess_tail_col)) {
  stop(
    "Não encontrei colunas de rhat/ess na sua versão do posterior::summarise_draws(). ",
    "Colunas disponíveis: ", paste(names(diag_mcmc), collapse = ", "),
    call. = FALSE
  )
}

diag_mcmc <- diag_mcmc %>%
  rename(
    rhat     = all_of(rhat_col),
    ess_bulk = all_of(ess_bulk_col),
    ess_tail = all_of(ess_tail_col)
  )

# Diagnóstico rápido (foco)
diag_focus <- diag_mcmc %>%
  filter(str_detect(variable, "^alpha0$|^beta_age$|^beta_logvol$|^sigma_y$|^sigma_model$|^sigma_region$")) %>%
  select(variable, rhat, ess_bulk, ess_tail) %>%
  arrange(desc(rhat))

# ----------------------------
# 5) Extrações para PPC/PIT/resíduos
# ----------------------------
draws_pred_mat <- posterior::as_draws_matrix(fit$draws(variables = c("y_rep", "mu_hat", "alpha_model", "u_region", "alpha0", "beta_age", "beta_logvol", "sigma_y", "sigma_model", "sigma_region")))

# y_rep e mu_hat viram matrizes (S x N)
yrep_cols <- grep("^y_rep\\[", colnames(draws_pred_mat), value = TRUE)
mu_cols   <- grep("^mu_hat\\[", colnames(draws_pred_mat), value = TRUE)

if (length(yrep_cols) != N || length(mu_cols) != N) {
  stop(
    "Esperava N colunas para y_rep e mu_hat (N=", N, "). ",
    "Obtive y_rep=", length(yrep_cols), ", mu_hat=", length(mu_cols),
    call. = FALSE
  )
}

yrep_mat <- draws_pred_mat[, yrep_cols, drop = FALSE]
mu_mat   <- draws_pred_mat[, mu_cols,   drop = FALSE]

# alpha_model e u_region (S x K)
a_cols <- grep("^alpha_model\\[", colnames(draws_pred_mat), value = TRUE)
u_cols <- grep("^u_region\\[",    colnames(draws_pred_mat), value = TRUE)

alpha_model_post_mean <- colMeans(draws_pred_mat[, a_cols, drop = FALSE])
alpha_model_post_q05  <- apply(draws_pred_mat[, a_cols, drop = FALSE], 2, quantile, probs = 0.05)
alpha_model_post_q95  <- apply(draws_pred_mat[, a_cols, drop = FALSE], 2, quantile, probs = 0.95)

u_region_post_mean <- colMeans(draws_pred_mat[, u_cols, drop = FALSE])
u_region_post_q05  <- apply(draws_pred_mat[, u_cols, drop = FALSE], 2, quantile, probs = 0.05)
u_region_post_q95  <- apply(draws_pred_mat[, u_cols, drop = FALSE], 2, quantile, probs = 0.95)

# Subamostra de draws para PPC/PIT
S <- nrow(yrep_mat)
set.seed(opt$seed + 1L)
ppc_idx  <- sample.int(S, size = min(opt$ppc_draws, S), replace = FALSE)
yrep_sub <- yrep_mat[ppc_idx, , drop = FALSE]

# PIT aproximado: PIT_i = P(y_rep < y_obs) via Monte Carlo
pit <- vapply(seq_len(N), function(i) mean(yrep_sub[, i] < df$y_err[i]), numeric(1))

# PPC por bins de idade (média)
df_bins <- df %>%
  mutate(
    age_bin = cut(
      age_years,
      breaks = quantile(age_years, probs = seq(0, 1, by = 0.1)),
      include.lowest = TRUE,
      ordered_result = TRUE
    )
  )

obs_by_bin <- df_bins %>%
  group_by(age_bin) %>%
  summarise(
    obs_mean = mean(y_err),
    n = n(),
    age_mid = mean(age_years),
    .groups = "drop"
  )

bins_levels <- levels(df_bins$age_bin)
bin_index   <- as.integer(df_bins$age_bin)

yrep_bin_mean <- matrix(NA_real_, nrow = nrow(yrep_sub), ncol = length(bins_levels))
for (b in seq_along(bins_levels)) {
  idx <- which(bin_index == b)
  yrep_bin_mean[, b] <- rowMeans(yrep_sub[, idx, drop = FALSE])
}

ppc_by_bin <- tibble(
  age_bin  = factor(bins_levels, levels = bins_levels, ordered = TRUE),
  age_mid  = obs_by_bin$age_mid,
  obs_mean = obs_by_bin$obs_mean,
  pred_q05 = apply(yrep_bin_mean, 2, quantile, probs = 0.05),
  pred_q50 = apply(yrep_bin_mean, 2, quantile, probs = 0.50),
  pred_q95 = apply(yrep_bin_mean, 2, quantile, probs = 0.95),
  n        = obs_by_bin$n
)

# PPC média por modelo
obs_by_model <- df %>%
  group_by(model) %>%
  summarise(obs_mean = mean(y_err), n = n(), .groups = "drop") %>%
  arrange(model)

model_index <- as.integer(df$model)
yrep_model_mean <- matrix(NA_real_, nrow = nrow(yrep_sub), ncol = K_model)
for (k in 1:K_model) {
  idx <- which(model_index == k)
  yrep_model_mean[, k] <- rowMeans(yrep_sub[, idx, drop = FALSE])
}

ppc_by_model <- tibble(
  model   = factor(paste0("M", 1:K_model), levels = paste0("M", 1:K_model)),
  obs_mean = obs_by_model$obs_mean,
  pred_q05 = apply(yrep_model_mean, 2, quantile, probs = 0.05),
  pred_q50 = apply(yrep_model_mean, 2, quantile, probs = 0.50),
  pred_q95 = apply(yrep_model_mean, 2, quantile, probs = 0.95),
  n        = obs_by_model$n
)

# Resíduos: observado - E_post[mu_hat]
mu_mean <- colMeans(mu_mat)
df_res <- df %>% mutate(mu_mean = mu_mean, resid = y_err - mu_mean)

# ----------------------------
# 6) Recuperação (vs verdade) — útil para checar se o pipeline está OK
# ----------------------------
posterior_summ <- bind_rows(
  summ_par(as.numeric(draws_pred_mat[, "alpha0"]), "alpha0", true$alpha0),
  summ_par(as.numeric(draws_pred_mat[, "beta_age"]), "beta_age", true$beta_age),
  summ_par(as.numeric(draws_pred_mat[, "beta_logvol"]), "beta_logvol", true$beta_logvol),
  summ_par(as.numeric(draws_pred_mat[, "sigma_y"]), "sigma_y", true$sigma_y),
  summ_par(as.numeric(draws_pred_mat[, "sigma_model"]), "sigma_model", true$sigma_model),
  summ_par(as.numeric(draws_pred_mat[, "sigma_region"]), "sigma_region", true$sigma_region)
) %>%
  mutate(
    z = (mean - truth) / sd,
    covered_90 = (truth >= q05 & truth <= q95)
  )

tbl_model_recovery <- tibble(
  model = factor(1:K_model, levels = 1:K_model, labels = paste0("M", 1:K_model)),
  truth = alpha_model_true,
  mean  = alpha_model_post_mean,
  q05   = alpha_model_post_q05,
  q95   = alpha_model_post_q95,
  covered_90 = (truth >= q05 & truth <= q95)
) %>% arrange(model)

tbl_region_recovery <- tibble(
  region = factor(1:K_region, levels = 1:K_region, labels = paste0("R", 1:K_region)),
  truth = u_region_true,
  mean  = u_region_post_mean,
  q05   = u_region_post_q05,
  q95   = u_region_post_q95,
  covered_90 = (truth >= q05 & truth <= q95)
) %>% arrange(region)

# ----------------------------
# 7) Gráficos (apenas se salvar/rodar interativo)
# ----------------------------
p_ppc_dens <- bayesplot::ppc_dens_overlay(
  y = df$y_err,
  yrep = yrep_sub[1:min(100, nrow(yrep_sub)), , drop = FALSE]
) +
  ggtitle("PPC: densidade do erro (observado vs y_rep)") +
  theme_minimal()

p_ppc_agebin <- ggplot(ppc_by_bin, aes(x = age_mid)) +
  geom_ribbon(aes(ymin = pred_q05, ymax = pred_q95), alpha = 0.25) +
  geom_line(aes(y = pred_q50), linewidth = 0.8) +
  geom_point(aes(y = obs_mean), size = 2) +
  labs(
    title = "PPC: média do erro por bins de idade",
    x = "Idade (anos) — centro do bin",
    y = "Erro (%)"
  ) +
  theme_minimal()

p_ppc_model <- ggplot(ppc_by_model, aes(x = model)) +
  geom_linerange(aes(ymin = pred_q05, ymax = pred_q95), linewidth = 2, alpha = 0.6) +
  geom_point(aes(y = pred_q50), size = 2) +
  geom_point(aes(y = obs_mean), shape = 1, size = 3, stroke = 1.2) +
  labs(
    title = "PPC: média do erro por modelo (círculo vazio = observado)",
    x = "Modelo",
    y = "Erro (%)"
  ) +
  theme_minimal()

p_pit <- ggplot(tibble(pit = pit), aes(x = pit)) +
  geom_histogram(bins = 20) +
  labs(
    title = "PIT (calibração do preditivo): ideal ~ uniforme",
    x = "PIT = P(y_rep < y_obs)",
    y = "Contagem"
  ) +
  theme_minimal()

p_param_recovery <- posterior_summ %>%
  mutate(param = factor(param, levels = param)) %>%
  ggplot(aes(x = param, y = mean)) +
  geom_linerange(aes(ymin = q05, ymax = q95), linewidth = 2, alpha = 0.7) +
  geom_point(size = 2) +
  geom_point(aes(y = truth), shape = 4, size = 3, stroke = 1.2) +
  coord_flip() +
  labs(
    title = "Recuperação (1 dataset): IC90% posterior (•) vs verdade (×)",
    x = NULL,
    y = "Valor"
  ) +
  theme_minimal()

p_res_age <- ggplot(df_res, aes(x = age_years, y = resid)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Resíduos (y - E[mu]) vs idade",
    x = "Idade (anos)",
    y = "Resíduo (%)"
  ) +
  theme_minimal()

p_res_logv <- ggplot(df_res, aes(x = log_vol, y = resid)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Resíduos (y - E[mu]) vs log(volume acumulado)",
    x = "log(volume acumulado)",
    y = "Resíduo (%)"
  ) +
  theme_minimal()

p_model_recovery <- ggplot(tbl_model_recovery, aes(x = model, y = mean)) +
  geom_linerange(aes(ymin = q05, ymax = q95), linewidth = 2, alpha = 0.7) +
  geom_point(size = 2) +
  geom_point(aes(y = truth), shape = 4, size = 3, stroke = 1.2) +
  labs(
    title = "Intercepto por modelo: IC90% posterior (•) vs verdade (×)",
    x = "Modelo",
    y = "alpha_model"
  ) +
  theme_minimal()

p_region_recovery <- ggplot(tbl_region_recovery, aes(x = region, y = mean)) +
  geom_linerange(aes(ymin = q05, ymax = q95), linewidth = 2, alpha = 0.7) +
  geom_point(size = 2) +
  geom_point(aes(y = truth), shape = 4, size = 3, stroke = 1.2) +
  labs(
    title = "Efeito por região: IC90% posterior (•) vs verdade (×)",
    x = "Região",
    y = "u_region"
  ) +
  theme_minimal()

# ----------------------------
# 8) Métricas simples (PIT) + outputs
# ----------------------------
pit_bin    <- cut(pit, breaks = seq(0, 1, by = 0.05), include.lowest = TRUE)
pit_counts <- as.numeric(table(pit_bin))
pit_props  <- pit_counts / sum(pit_counts)
pit_rms    <- sqrt(mean((pit_props - mean(pit_props))^2))

# ----------------------------
# 9) Impressões (console) + (opcional) salvar
# ----------------------------
lines_out <- character(0)
add_line <- function(...) {
  lines_out <<- c(lines_out, paste0(...))
}

add_line("====================")
add_line("RESUMO FINAL — Hidrômetro (degradação / erro)")
add_line("====================\n")

add_line("1) Sampler / HMC")
add_line(capture.output(print(sampler_overall)))
add_line("\nPor chain:")
add_line(capture.output(print(diag_summary)))

add_line("\n\n2) Diagnósticos MCMC (foco)")
add_line(capture.output(print(diag_focus)))

add_line("\n\n3) Recuperação de parâmetros escalares (1 dataset)")
add_line(capture.output(print(posterior_summ %>% mutate(across(where(is.numeric), ~round(.x, 3))))))

add_line("\n\n4) Calibração preditiva (PIT)")
add_line(paste0("PIT-RMS (quanto menor, melhor; 0 ~ perfeito): ", round(pit_rms, 4)))

add_line("\n\n5) Cobertura 90% (escalares)")
add_line(capture.output(print(
  posterior_summ %>%
    summarise(
      covered_90_rate = mean(covered_90),
      mean_abs_z      = mean(abs(z))
    )
)))

add_line("\n\n6) Cobertura 90% (por modelo / por região)")
add_line(capture.output(print(tbl_model_recovery  %>% summarise(covered_90_rate = mean(covered_90)))))
add_line(capture.output(print(tbl_region_recovery %>% summarise(covered_90_rate = mean(covered_90)))))

# imprime no console
cat(paste(lines_out, collapse = "\n"), "\n")

# opcional: salva report
if (opt$save_report == 1L) {
  safe_dir_create(opt$report_path)
  writeLines(lines_out, con = opt$report_path)
  message("==> report salvo em: ", opt$report_path)
}

# opcional: salva plots
if (opt$save_plots == 1L) {
  safe_dir_create(opt$plots_path)

  grDevices::pdf(opt$plots_path, width = 10, height = 7)
  tryCatch(
    {
      print(p_ppc_dens)
      print(p_ppc_agebin)
      print(p_ppc_model)
      print(p_pit)
      print(p_param_recovery)
      print(p_res_age)
      print(p_res_logv)
      print(p_model_recovery)
      print(p_region_recovery)
    },
    finally = {
      grDevices::dev.off()
    }
  )
  message("==> plots salvos em: ", opt$plots_path)
} else if (interactive()) {
  # em modo interativo, é ok visualizar
  print(p_ppc_dens)
  print(p_ppc_agebin)
  print(p_ppc_model)
  print(p_pit)
  print(p_param_recovery)
  print(p_res_age)
  print(p_res_logv)
  print(p_model_recovery)
  print(p_region_recovery)
}

invisible(list(
  opt = opt,
  truth = true,
  sampler_overall = sampler_overall,
  sampler_chain_summary = diag_summary,
  mcmc_diagnostics_focus = diag_focus,
  posterior_summary_scalars = posterior_summ,
  recovery_by_model = tbl_model_recovery,
  recovery_by_region = tbl_region_recovery,
  ppc_by_agebin = ppc_by_bin,
  ppc_by_model = ppc_by_model,
  pit_rms = pit_rms,
  plots = list(
    p_ppc_dens = p_ppc_dens,
    p_ppc_agebin = p_ppc_agebin,
    p_ppc_model = p_ppc_model,
    p_pit = p_pit,
    p_param_recovery = p_param_recovery,
    p_res_age = p_res_age,
    p_res_logv = p_res_logv,
    p_model_recovery = p_model_recovery,
    p_region_recovery = p_region_recovery
  )
))
