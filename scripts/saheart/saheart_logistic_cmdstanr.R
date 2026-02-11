#!/usr/bin/env Rscript

############################################################
# SAheart — Regressão Logística Bayesiana (Stan via cmdstanr)
# Console-only (nada é gravado em arquivo)
# Stan inline via cmdstanr::write_stan_file
#
# Rode a partir do ROOT do repo:
#   Rscript scripts/saheart/saheart_logistic_cmdstanr.R
#
# Dados:
# - Por padrão, tenta ler: data/raw/SAheart.data
# - Se não existir, baixa do URL oficial do ESL (Hastie/Tibs):
#   https://hastie.su.domains/ElemStatLearn/datasets/SAheart.data
#
# Opções (formato --chave=valor):
#   --data=data/raw/SAheart.data          (arquivo local .data/.csv)
#   --data_url=https://.../SAheart.data   (override do URL)
#   --scale_predictors=0|1                (padrão 1)
#   --prior_only=0|1                      (padrão 0)
#
#   --alpha_scale=1.5                     (prior alpha ~ Normal(0, alpha_scale))
#   --beta_scale=1.0                      (prior beta  ~ Normal(0, beta_scale))
#
#   --seed=123
#   --chains=4
#   --iter_warmup=1000
#   --iter_sampling=1000
#   --adapt_delta=0.95
#   --max_treedepth=12
#   --refresh=0
############################################################

# ----------------------------
# 0) Helpers (args + checks)
# ----------------------------
parse_args <- function(args) {
  out <- list(
    data = file.path("data", "raw", "SAheart.data"),
    data_url = "https://hastie.su.domains/ElemStatLearn/datasets/SAheart.data",
    scale_predictors = 1L,
    prior_only = 0L,

    # priors
    alpha_scale = 1.5,
    beta_scale  = 1.0,

    # MCMC
    seed = 123L,
    chains = 4L,
    iter_warmup = 1000L,
    iter_sampling = 1000L,
    adapt_delta = 0.95,
    max_treedepth = 12L,
    refresh = 0L
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
  out$scale_predictors <- as.integer(out$scale_predictors)
  out$prior_only <- as.integer(out$prior_only)

  out$alpha_scale <- as.numeric(out$alpha_scale)
  out$beta_scale  <- as.numeric(out$beta_scale)

  out$seed <- as.integer(out$seed)
  out$chains <- as.integer(out$chains)
  out$iter_warmup <- as.integer(out$iter_warmup)
  out$iter_sampling <- as.integer(out$iter_sampling)
  out$adapt_delta <- as.numeric(out$adapt_delta)
  out$max_treedepth <- as.integer(out$max_treedepth)
  out$refresh <- as.integer(out$refresh)

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

read_saheart <- function(path_local, url_fallback) {
  # arquivo local primeiro
  if (file.exists(path_local)) {
    df <- utils::read.csv(
      path_local,
      header = TRUE, sep = ",",
      row.names = 1,
      stringsAsFactors = FALSE
    )
    return(list(df = tibble::as_tibble(df), source = path_local))
  }

  # fallback URL
  df <- utils::read.csv(
    url(url_fallback),
    header = TRUE, sep = ",",
    row.names = 1,
    stringsAsFactors = FALSE
  )
  list(df = tibble::as_tibble(df), source = url_fallback)
}

auc_rank <- function(scores, labels) {
  labels <- as.integer(labels)
  n1 <- sum(labels == 1)
  n0 <- sum(labels == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(scores, ties.method = "average")
  (sum(r[labels == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# ----------------------------
# 0.1) Bootstrap
# ----------------------------
args <- commandArgs(trailingOnly = TRUE)
opt <- parse_args(args)

pkgs <- c("cmdstanr", "posterior", "dplyr", "tibble")
require_pkgs(pkgs)

library(cmdstanr)
library(posterior)
library(dplyr)
library(tibble)

options(mc.cores = max(1L, parallel::detectCores()))
set.seed(opt$seed)

check_cmdstan()

# ----------------------------
# 1) Ler e preparar dados
# ----------------------------
dat <- read_saheart(opt$data, opt$data_url)
df <- dat$df

# Checagens mínimas esperadas do SAheart (ESL)
needed_cols <- c("sbp","tobacco","ldl","adiposity","famhist","typea","obesity","alcohol","age","chd")
missing_cols <- setdiff(needed_cols, names(df))
if (length(missing_cols) > 0) {
  stop(
    "Colunas esperadas não encontradas no dataset: ", paste(missing_cols, collapse = ", "),
    "\nFonte lida: ", dat$source,
    call. = FALSE
  )
}

y <- as.integer(df$chd)
if (any(!y %in% c(0L, 1L))) {
  stop("A coluna chd deve ser 0/1 (inteiro).", call. = FALSE)
}

X_raw <- df %>%
  transmute(
    sbp       = as.numeric(sbp),
    tobacco   = as.numeric(tobacco),
    ldl       = as.numeric(ldl),
    adiposity = as.numeric(adiposity),
    typea     = as.numeric(typea),
    obesity   = as.numeric(obesity),
    alcohol   = as.numeric(alcohol),
    age       = as.numeric(age),
    famhist   = ifelse(famhist == "Present", 1, 0)
  )

# NA check
if (anyNA(X_raw) || anyNA(y)) {
  stop("Há NA nos dados após coerção. Verifique o arquivo/fonte.", call. = FALSE)
}

# Padronizar predictors (recomendado)
if (opt$scale_predictors == 1L) {
  X_scaled <- X_raw %>% mutate(across(everything(), ~ as.numeric(scale(.x))))
} else {
  X_scaled <- X_raw
}

X <- as.matrix(X_scaled)
N <- nrow(X)
K <- ncol(X)
pred_names <- colnames(X)

cat("\n============================\n")
cat("SAheart — leitura/prep OK\n")
cat("============================\n")
cat("Fonte            :", dat$source, "\n")
cat("N                :", N, "\n")
cat("K                :", K, "\n")
cat("scale_predictors :", opt$scale_predictors, "\n")
cat("Prevalência CHD  :", sprintf("%.3f", mean(y)), "\n")

# ----------------------------
# 2) Stan model (inline)
# ----------------------------
stan_code <- "
data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N, K] X;
  array[N] int<lower=0, upper=1> y;
  int<lower=0, upper=1> prior_only;
  real<lower=0> alpha_scale;
  real<lower=0> beta_scale;
}
parameters {
  real alpha;
  vector[K] beta;
}
model {
  alpha ~ normal(0, alpha_scale);
  beta  ~ normal(0, beta_scale);

  if (prior_only == 0) {
    y ~ bernoulli_logit(alpha + X * beta);
  }
}
generated quantities {
  vector[N] p;
  array[N] int y_rep;

  for (n in 1:N) {
    real eta = alpha + dot_product(row(X, n), beta);
    p[n] = inv_logit(eta);
    y_rep[n] = bernoulli_rng(p[n]);
  }
}
"

stan_data <- list(
  N = N, K = K, X = X, y = y,
  prior_only = opt$prior_only,
  alpha_scale = opt$alpha_scale,
  beta_scale  = opt$beta_scale
)

stan_file <- cmdstanr::write_stan_file(stan_code)
mod <- cmdstanr::cmdstan_model(stan_file)

# ----------------------------
# 3) Amostragem
# ----------------------------
fit <- mod$sample(
  data = stan_data,
  seed = opt$seed,
  chains = opt$chains,
  iter_warmup = opt$iter_warmup,
  iter_sampling = opt$iter_sampling,
  adapt_delta = opt$adapt_delta,
  max_treedepth = opt$max_treedepth,
  refresh = opt$refresh
)

# ----------------------------
# 4) QC (padrão do repo)
# ----------------------------
sd <- fit$sampler_diagnostics(format = "draws_array")

divergent <- sum(sd[, , "divergent__"])
treedepth_hit <- sum(sd[, , "treedepth__"] >= opt$max_treedepth)

energy <- sd[, , "energy__"]
ebfmi_chain <- apply(energy, 2, function(e) mean(diff(e)^2) / stats::var(e))
min_bfmi <- suppressWarnings(min(ebfmi_chain, na.rm = TRUE))

sum_ab <- fit$summary(variables = c("alpha", "beta"))
max_rhat <- max(sum_ab$rhat, na.rm = TRUE)
min_ess_bulk <- min(sum_ab$ess_bulk, na.rm = TRUE)
min_ess_tail <- min(sum_ab$ess_tail, na.rm = TRUE)

flag_rhat <- (max_rhat > 1.01)
flag_ess  <- (min_ess_bulk < 400) || (min_ess_tail < 400)
flag_div  <- (divergent > 0)
flag_tree <- (treedepth_hit > 0)
flag_bfmi <- (!is.na(min_bfmi) && is.finite(min_bfmi) && min_bfmi < 0.30)

qc_flags <- tibble(
  check = c(
    "Rhat (max <= 1.01)",
    "ESS bulk/tail (min >= 400)",
    "Divergências (0)",
    "Treedepth hits (0)",
    "E-BFMI (min >= 0.30)"
  ),
  status = c(
    ifelse(flag_rhat, "ATENÇÃO", "OK"),
    ifelse(flag_ess,  "ATENÇÃO", "OK"),
    ifelse(flag_div,  "ATENÇÃO", "OK"),
    ifelse(flag_tree, "ATENÇÃO", "OK"),
    ifelse(flag_bfmi, "ATENÇÃO", "OK")
  ),
  value = c(
    sprintf("max Rhat = %.4f", max_rhat),
    sprintf("min bulk = %.1f | min tail = %.1f", min_ess_bulk, min_ess_tail),
    sprintf("divergences = %d", divergent),
    sprintf("hits = %d (max_treedepth=%d)", treedepth_hit, opt$max_treedepth),
    sprintf("min BFMI = %.3f", min_bfmi)
  )
)

# ----------------------------
# 5) Posterior draws + métricas (PPC / calib / AUC)
# ----------------------------
p_draws <- posterior::as_draws_matrix(fit$draws("p", format = "draws_matrix"))
yrep_draws <- posterior::as_draws_matrix(fit$draws("y_rep", format = "draws_matrix"))

obs_prev <- mean(y)
rep_prev <- rowMeans(yrep_draws)
rep_prev_ci <- stats::quantile(rep_prev, probs = c(0.05, 0.5, 0.95), names = FALSE)

# Brier score por draw
y_mat <- matrix(y, nrow = nrow(p_draws), ncol = length(y), byrow = TRUE)
brier_draw <- rowMeans((p_draws - y_mat)^2)
brier_ci <- stats::quantile(brier_draw, probs = c(0.05, 0.5, 0.95), names = FALSE)

# Predição pontual (posterior mean)
p_hat <- colMeans(p_draws)

# Calibração por bins 0.1 + ECE
bins <- cut(p_hat, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)
calib_tbl <- tibble(p_hat = p_hat, y = y, bin = bins) %>%
  group_by(bin) %>%
  summarise(
    n = dplyr::n(),
    p_mean = mean(p_hat),
    y_mean = mean(y),
    .groups = "drop"
  )
ece <- with(calib_tbl, sum((n / sum(n)) * abs(y_mean - p_mean)))

# AUC via ranks
auc <- as.numeric(auc_rank(p_hat, y))

# Sumários de parâmetros (alpha/beta)
alpha_vec <- as.numeric(posterior::as_draws_matrix(fit$draws("alpha", format = "draws_matrix"))[, 1])
beta_mat <- posterior::as_draws_matrix(fit$draws("beta", format = "draws_matrix"))

summ_alpha <- tibble(
  param = "alpha",
  mean = mean(alpha_vec),
  sd   = stats::sd(alpha_vec),
  q05  = unname(stats::quantile(alpha_vec, 0.05)),
  q50  = unname(stats::quantile(alpha_vec, 0.50)),
  q95  = unname(stats::quantile(alpha_vec, 0.95)),
  p_pos = mean(alpha_vec > 0),
  p_neg = mean(alpha_vec < 0)
)

summ_beta <- tibble(
  predictor = pred_names,
  mean = colMeans(beta_mat),
  sd   = apply(beta_mat, 2, stats::sd),
  q05  = apply(beta_mat, 2, stats::quantile, probs = 0.05),
  q50  = apply(beta_mat, 2, stats::quantile, probs = 0.50),
  q95  = apply(beta_mat, 2, stats::quantile, probs = 0.95),
  p_pos = colMeans(beta_mat > 0),
  p_neg = colMeans(beta_mat < 0)
) %>%
  mutate(abs_mean = abs(mean)) %>%
  arrange(desc(abs_mean)) %>%
  select(-abs_mean)

# ----------------------------
# 6) Impressão final (console-only)
# ----------------------------
cat("\n============================================================\n")
cat("SAheart | Regressão Logística Bayesiana (cmdstanr/Stan) — Relatório\n")
cat("============================================================\n")

cat("\n[1] Dados\n")
cat(sprintf("Fonte: %s\n", dat$source))
cat(sprintf("N = %d | K = %d | scale_predictors = %d\n", N, K, opt$scale_predictors))
cat(sprintf("Prevalência observada CHD = %.3f\n", obs_prev))

cat("\n[2] QC padrão-ouro (sem LOO/WAIC)\n")
print(qc_flags, n = Inf)

cat("\n[3] Parâmetros — Intercepto (alpha)\n")
print(summ_alpha)

cat("\n[4] Parâmetros — Betas (ordenado por |média|)\n")
print(summ_beta, n = Inf)

cat("\n[5] Posterior Predictive Checks (PPC)\n")
cat(sprintf("Prevalência replicada (5%%, 50%%, 95%%): %.3f | %.3f | %.3f\n",
            rep_prev_ci[1], rep_prev_ci[2], rep_prev_ci[3]))
cat(sprintf("Brier score (5%%, 50%%, 95%%): %.4f | %.4f | %.4f\n",
            brier_ci[1], brier_ci[2], brier_ci[3]))
cat(sprintf("AUC (ranking) = %.3f\n", auc))
cat(sprintf("ECE (bins 0.1) = %.4f\n", ece))

cat("\n[6] Calibração por bins (0.1)\n")
print(calib_tbl, n = Inf)

cat("\n============================================================\n")
cat("Fim.\n")
cat("============================================================\n")
