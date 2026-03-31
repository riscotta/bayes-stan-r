#!/usr/bin/env Rscript

############################################################
# SAT — seleção Bayesiana de variáveis
# R + Stan (rstan)
# Console-only (não grava dados, resultados ou modelos em disco)
#
# Rode a partir do ROOT do repo:
#   Rscript scripts/sat_model_selection/sat_stan_model_selection_rstan.R
#
# O script:
# - lê a base SAT diretamente da URL pública do Rdatasets
# - compara todos os 2^K modelos (aqui K = 4 => 16 modelos)
# - avalia evidência marginal (bridge sampling / Bayes factors)
# - avalia desempenho preditivo com PSIS-LOO
# - resume diagnósticos HMC e PPC
# - imprime tudo apenas no final no console
#
# Opções (formato --chave=valor):
#   --data_url=https://.../SAT.csv
#   --seed=42
#   --chains=4
#   --iter=4000
#   --warmup=2000
#   --adapt_delta=0.99
#   --max_treedepth=14
#   --prior_incl=0.50
############################################################

parse_args <- function(args) {
  out <- list(
    data_url = "https://vincentarelbundock.github.io/Rdatasets/csv/mosaicData/SAT.csv",
    seed = 42L,
    chains = 4L,
    iter = 4000L,
    warmup = 2000L,
    adapt_delta = 0.99,
    max_treedepth = 14L,
    prior_incl = 0.50
  )

  if (length(args) == 0L) return(out)

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
  out$iter <- as.integer(out$iter)
  out$warmup <- as.integer(out$warmup)
  out$adapt_delta <- as.numeric(out$adapt_delta)
  out$max_treedepth <- as.integer(out$max_treedepth)
  out$prior_incl <- as.numeric(out$prior_incl)

  out
}

require_pkgs <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    stop(
      "Pacotes faltando: ", paste(missing, collapse = ", "),
      "\nRode primeiro: Rscript scripts/_setup/install_deps.R --all",
      call. = FALSE
    )
  }
}

read_sat_data <- function(data_url) {
  sat_raw <- utils::read.csv(data_url, stringsAsFactors = FALSE)

  drop_cols <- intersect(c("rownames", "X"), names(sat_raw))
  if (length(drop_cols) > 0L) {
    sat_raw <- sat_raw[, setdiff(names(sat_raw), drop_cols), drop = FALSE]
  }

  required_cols <- c("state", "expend", "ratio", "salary", "frac", "sat")
  missing_cols <- setdiff(required_cols, names(sat_raw))
  if (length(missing_cols) > 0L) {
    stop(
      "A base SAT não contém as colunas esperadas: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  sat_raw[, required_cols, drop = FALSE]
}

add_line <- local({
  report_lines <- character(0)

  list(
    add = function(..., .sep = "") {
      report_lines <<- c(report_lines, paste(..., sep = .sep, collapse = ""))
    },
    blank = function() {
      report_lines <<- c(report_lines, "")
    },
    block = function(x) {
      report_lines <<- c(report_lines, x)
    },
    print = function() {
      cat(paste(report_lines, collapse = "\n"))
    }
  )
})

safe_exp <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x) & (x < 700)
  out[ok] <- exp(x[ok])
  out
}

softmax <- function(z) {
  z <- z - max(z)
  ez <- exp(z)
  ez / sum(ez)
}

model_label <- function(mask, vars) {
  if (sum(mask) == 0L) return("NULL")
  paste(vars[mask == 1L], collapse = " + ")
}

compute_bfmi <- function(sampler_params_list) {
  vapply(sampler_params_list, function(sp) {
    e <- sp[, "energy__"]
    num <- mean(diff(e)^2)
    den <- stats::var(e)
    if (!is.finite(num) || !is.finite(den) || den <= 0) return(NA_real_)
    num / den
  }, numeric(1))
}

summarize_ppc <- function(y_obs_orig, mu_draws_std, yrep_draws_std, y_center, y_scale) {
  mu_orig   <- y_center + y_scale * mu_draws_std
  yrep_orig <- y_center + y_scale * yrep_draws_std

  obs_mean <- mean(y_obs_orig)
  obs_sd   <- stats::sd(y_obs_orig)
  obs_min  <- min(y_obs_orig)
  obs_max  <- max(y_obs_orig)

  rep_mean <- rowMeans(yrep_orig)
  rep_sd   <- apply(yrep_orig, 1L, stats::sd)
  rep_min  <- apply(yrep_orig, 1L, min)
  rep_max  <- apply(yrep_orig, 1L, max)

  ppp_mean <- mean(rep_mean >= obs_mean)
  ppp_sd   <- mean(rep_sd   >= obs_sd)
  ppp_min  <- mean(rep_min  >= obs_min)
  ppp_max  <- mean(rep_max  >= obs_max)

  rmse_draws <- sqrt(rowMeans((mu_orig - matrix(
    y_obs_orig,
    nrow = nrow(mu_orig),
    ncol = ncol(mu_orig),
    byrow = TRUE
  ))^2))

  c(
    ppp_mean = ppp_mean,
    ppp_sd   = ppp_sd,
    ppp_min  = ppp_min,
    ppp_max  = ppp_max,
    rmse_med = stats::median(rmse_draws),
    rmse_p10 = stats::quantile(rmse_draws, 0.10),
    rmse_p90 = stats::quantile(rmse_draws, 0.90)
  )
}

extract_param_draw_summary <- function(fit) {
  draws <- posterior::as_draws_array(fit)
  draws <- posterior::subset_draws(
    draws,
    variable = c("^alpha$", "^beta\\[", "^sigma$"),
    regex = TRUE
  )
  s <- posterior::summarise_draws(draws, "mean", "sd", "rhat", "ess_bulk", "ess_tail")
  as.data.frame(s)
}

fit_one_model <- function(mask, model_id, sm_reg, sm_null, y_std, X_std, y_raw, x_names,
                          y_center, y_scale, x_center, x_scale, opt) {
  idx <- which(mask == 1L)
  N <- nrow(X_std)

  if (length(idx) == 0L) {
    fit <- rstan::sampling(
      object = sm_null,
      data = list(N = N, y = y_std),
      seed = opt$seed + model_id,
      chains = opt$chains,
      iter = opt$iter,
      warmup = opt$warmup,
      refresh = 0,
      control = list(adapt_delta = opt$adapt_delta, max_treedepth = opt$max_treedepth)
    )
  } else {
    fit <- rstan::sampling(
      object = sm_reg,
      data = list(
        N = N,
        K = length(idx),
        X = X_std[, idx, drop = FALSE],
        y = y_std
      ),
      seed = opt$seed + model_id,
      chains = opt$chains,
      iter = opt$iter,
      warmup = opt$warmup,
      refresh = 0,
      control = list(adapt_delta = opt$adapt_delta, max_treedepth = opt$max_treedepth)
    )
  }

  param_sum <- extract_param_draw_summary(fit)
  max_rhat <- max(param_sum$rhat, na.rm = TRUE)
  min_ess_bulk <- min(param_sum$ess_bulk, na.rm = TRUE)
  min_ess_tail <- min(param_sum$ess_tail, na.rm = TRUE)

  sampler_params <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
  divergences <- sum(vapply(sampler_params, function(sp) sum(sp[, "divergent__"]), numeric(1)))
  treedepth_hits <- sum(vapply(sampler_params, function(sp) sum(sp[, "treedepth__"] >= opt$max_treedepth), numeric(1)))
  bfmi_by_chain <- compute_bfmi(sampler_params)
  min_bfmi <- min(bfmi_by_chain, na.rm = TRUE)

  log_lik_array <- loo::extract_log_lik(fit, parameter_name = "log_lik", merge_chains = FALSE)
  r_eff <- loo::relative_eff(exp(log_lik_array))
  loo_obj <- suppressWarnings(loo::loo(log_lik_array, r_eff = r_eff))

  pareto_k <- loo_obj$diagnostics$pareto_k
  n_k_gt_05 <- sum(pareto_k > 0.5, na.rm = TRUE)
  n_k_gt_07 <- sum(pareto_k > 0.7, na.rm = TRUE)
  n_k_gt_10 <- sum(pareto_k > 1.0, na.rm = TRUE)

  bridge_obj <- suppressWarnings(
    bridgesampling::bridge_sampler(fit, silent = TRUE)
  )

  mu_draws_std <- rstan::extract(fit, pars = "mu")$mu
  yrep_draws_std <- rstan::extract(fit, pars = "y_rep")$y_rep
  ppc <- summarize_ppc(
    y_obs_orig = y_raw,
    mu_draws_std = mu_draws_std,
    yrep_draws_std = yrep_draws_std,
    y_center = y_center,
    y_scale = y_scale
  )

  alpha_draws <- rstan::extract(fit, pars = "alpha")$alpha
  alpha_mean_std <- mean(alpha_draws)

  beta_mean_std_full <- stats::setNames(rep(0, length(x_names)), x_names)
  if (length(idx) > 0L) {
    beta_draws <- rstan::extract(fit, pars = "beta")$beta
    beta_mean_std_full[idx] <- colMeans(beta_draws)
  }

  list(
    fit = fit,
    idx = idx,
    param_sum = param_sum,
    max_rhat = max_rhat,
    min_ess_bulk = min_ess_bulk,
    min_ess_tail = min_ess_tail,
    divergences = divergences,
    treedepth_hits = treedepth_hits,
    bfmi_by_chain = bfmi_by_chain,
    min_bfmi = min_bfmi,
    loo = loo_obj,
    elpd_loo = loo_obj$estimates["elpd_loo", "Estimate"],
    se_elpd_loo = loo_obj$estimates["elpd_loo", "SE"],
    p_loo = loo_obj$estimates["p_loo", "Estimate"],
    looic = loo_obj$estimates["looic", "Estimate"],
    pareto_k = pareto_k,
    n_k_gt_05 = n_k_gt_05,
    n_k_gt_07 = n_k_gt_07,
    n_k_gt_10 = n_k_gt_10,
    bridge = bridge_obj,
    logml = bridge_obj$logml,
    ppc = ppc,
    alpha_mean_std = alpha_mean_std,
    beta_mean_std_full = beta_mean_std_full,
    alpha_orig = y_center + y_scale * alpha_mean_std - sum((beta_mean_std_full * (y_scale / x_scale)) * x_center)
  )
}

args <- commandArgs(trailingOnly = TRUE)
opt <- parse_args(args)

require_pkgs(c("rstan", "loo", "bridgesampling", "posterior"))

suppressPackageStartupMessages({
  library(rstan)
  library(loo)
  library(bridgesampling)
  library(posterior)
})

rstan_options(auto_write = FALSE)
options(mc.cores = max(1L, parallel::detectCores(logical = TRUE) - 1L))

sat_raw <- read_sat_data(opt$data_url)
sat_raw <- sat_raw[stats::complete.cases(sat_raw), , drop = FALSE]

y_name <- "sat"
x_names <- c("expend", "ratio", "salary", "frac")

y_raw <- sat_raw[[y_name]]
X_raw <- as.matrix(sat_raw[, x_names, drop = FALSE])

if (nrow(sat_raw) < 20L) stop("Base analítica muito pequena após limpeza.", call. = FALSE)
if (any(apply(X_raw, 2L, stats::sd) == 0)) stop("Há preditor com desvio-padrão zero.", call. = FALSE)
if (stats::sd(y_raw) == 0) stop("A resposta tem desvio-padrão zero.", call. = FALSE)
if (opt$warmup >= opt$iter) stop("warmup deve ser menor que iter.", call. = FALSE)
if (opt$prior_incl <= 0 || opt$prior_incl >= 1) stop("prior_incl deve estar no intervalo aberto (0, 1).", call. = FALSE)

y_center <- mean(y_raw)
y_scale <- stats::sd(y_raw)
x_center <- colMeans(X_raw)
x_scale <- apply(X_raw, 2L, stats::sd)

y_std <- as.numeric(scale(y_raw))
X_std <- scale(X_raw)
N <- nrow(X_std)
K <- ncol(X_std)

model_space <- expand.grid(rep(list(c(0L, 1L)), K), KEEP.OUT.ATTRS = FALSE)
colnames(model_space) <- x_names
model_space$model_id <- seq_len(nrow(model_space))
model_space$k <- rowSums(model_space[, x_names, drop = FALSE])
model_space$label <- apply(
  model_space[, x_names, drop = FALSE],
  1L,
  function(z) model_label(as.integer(z), x_names)
)

log_prior_model <- function(mask, p_incl = opt$prior_incl) {
  sum(stats::dbinom(mask, size = 1L, prob = p_incl, log = TRUE))
}

stan_code_reg <- "
data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N, K] X;
  vector[N] y;
}
parameters {
  real alpha;
  vector[K] beta;
  real<lower=0> sigma;
}
model {
  alpha ~ normal(0, 1.5);
  beta  ~ normal(0, 1.0);
  sigma ~ exponential(1);

  y ~ normal(alpha + X * beta, sigma);
}
generated quantities {
  vector[N] mu;
  vector[N] log_lik;
  vector[N] y_rep;

  mu = alpha + X * beta;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(y[n] | mu[n], sigma);
    y_rep[n]   = normal_rng(mu[n], sigma);
  }
}
"

stan_code_null <- "
data {
  int<lower=1> N;
  vector[N] y;
}
parameters {
  real alpha;
  real<lower=0> sigma;
}
model {
  alpha ~ normal(0, 1.5);
  sigma ~ exponential(1);

  y ~ normal(alpha, sigma);
}
generated quantities {
  vector[N] mu;
  vector[N] log_lik;
  vector[N] y_rep;

  for (n in 1:N) {
    mu[n]      = alpha;
    log_lik[n] = normal_lpdf(y[n] | mu[n], sigma);
    y_rep[n]   = normal_rng(mu[n], sigma);
  }
}
"

sm_reg <- rstan::stan_model(model_code = stan_code_reg)
sm_null <- rstan::stan_model(model_code = stan_code_null)

results <- vector("list", nrow(model_space))
for (m in seq_len(nrow(model_space))) {
  mask_m <- as.integer(model_space[m, x_names, drop = TRUE])
  results[[m]] <- fit_one_model(
    mask = mask_m,
    model_id = model_space$model_id[m],
    sm_reg = sm_reg,
    sm_null = sm_null,
    y_std = y_std,
    X_std = X_std,
    y_raw = y_raw,
    x_names = x_names,
    y_center = y_center,
    y_scale = y_scale,
    x_center = x_center,
    x_scale = x_scale,
    opt = opt
  )
}

res_tbl <- model_space
res_tbl$log_prior <- apply(
  res_tbl[, x_names, drop = FALSE],
  1L,
  function(z) log_prior_model(as.integer(z), p_incl = opt$prior_incl)
)
res_tbl$logml <- vapply(results, function(z) z$logml, numeric(1))
res_tbl$log_post_unnorm <- res_tbl$logml + res_tbl$log_prior
res_tbl$post_prob <- softmax(res_tbl$log_post_unnorm)
res_tbl$elpd_loo <- vapply(results, function(z) z$elpd_loo, numeric(1))
res_tbl$se_elpd_loo <- vapply(results, function(z) z$se_elpd_loo, numeric(1))
res_tbl$p_loo <- vapply(results, function(z) z$p_loo, numeric(1))
res_tbl$looic <- vapply(results, function(z) z$looic, numeric(1))
res_tbl$max_rhat <- vapply(results, function(z) z$max_rhat, numeric(1))
res_tbl$min_ess_bulk <- vapply(results, function(z) z$min_ess_bulk, numeric(1))
res_tbl$min_ess_tail <- vapply(results, function(z) z$min_ess_tail, numeric(1))
res_tbl$divergences <- vapply(results, function(z) z$divergences, numeric(1))
res_tbl$treedepth_hits <- vapply(results, function(z) z$treedepth_hits, numeric(1))
res_tbl$min_bfmi <- vapply(results, function(z) z$min_bfmi, numeric(1))
res_tbl$n_k_gt_05 <- vapply(results, function(z) z$n_k_gt_05, numeric(1))
res_tbl$n_k_gt_07 <- vapply(results, function(z) z$n_k_gt_07, numeric(1))
res_tbl$n_k_gt_10 <- vapply(results, function(z) z$n_k_gt_10, numeric(1))
res_tbl$ppp_mean <- vapply(results, function(z) z$ppc["ppp_mean"], numeric(1))
res_tbl$ppp_sd <- vapply(results, function(z) z$ppc["ppp_sd"], numeric(1))
res_tbl$ppp_min <- vapply(results, function(z) z$ppc["ppp_min"], numeric(1))
res_tbl$ppp_max <- vapply(results, function(z) z$ppc["ppp_max"], numeric(1))
res_tbl$rmse_med <- vapply(results, function(z) z$ppc["rmse_med"], numeric(1))
res_tbl$rmse_p10 <- vapply(results, function(z) z$ppc["rmse_p10"], numeric(1))
res_tbl$rmse_p90 <- vapply(results, function(z) z$ppc["rmse_p90"], numeric(1))

loo_list <- lapply(results, function(z) z$loo)
names(loo_list) <- res_tbl$label
loo_cmp <- loo::loo_compare(loo_list)
loo_cmp_df <- data.frame(
  label = rownames(loo_cmp),
  elpd_diff = loo_cmp[, "elpd_diff"],
  se_diff = loo_cmp[, "se_diff"],
  row.names = NULL,
  check.names = FALSE
)

best_label_loo <- loo_cmp_df$label[1]
best_model_loo <- res_tbl$model_id[match(best_label_loo, res_tbl$label)]

null_row <- which(res_tbl$label == "NULL")
if (length(null_row) != 1L) {
  stop("Não foi possível identificar unicamente o modelo nulo.", call. = FALSE)
}

res_tbl$log_BF_vs_null <- res_tbl$logml - res_tbl$logml[null_row]
res_tbl$BF_vs_null <- safe_exp(res_tbl$log_BF_vs_null)

res_by_post <- res_tbl[order(res_tbl$post_prob, decreasing = TRUE), ]
rownames(res_by_post) <- NULL
best_model_post <- res_by_post$model_id[1]

incl_prob <- sapply(x_names, function(v) {
  sum(res_tbl$post_prob[res_tbl[[v]] == 1L])
})
incl_prob <- sort(incl_prob, decreasing = TRUE)

alpha_std_by_model <- vapply(results, function(z) z$alpha_mean_std, numeric(1))
beta_std_by_model <- do.call(rbind, lapply(results, function(z) z$beta_mean_std_full))
colnames(beta_std_by_model) <- x_names

alpha_std_bma <- sum(alpha_std_by_model * res_tbl$post_prob)
beta_std_bma <- colSums(beta_std_by_model * res_tbl$post_prob)

beta_orig_bma <- beta_std_bma * (y_scale / x_scale)
alpha_orig_bma <- y_center + y_scale * alpha_std_bma - sum(beta_orig_bma * x_center)

best_post_row <- res_tbl[res_tbl$model_id == best_model_post, , drop = FALSE]
best_loo_row <- res_tbl[res_tbl$model_id == best_model_loo, , drop = FALSE]

add_line$add("============================================================")
add_line$add("RELATÓRIO FINAL - SAT / R + STAN / SELEÇÃO BAYESIANA")
add_line$add("============================================================")
add_line$blank()

add_line$add("[1] BASE ANALÍTICA")
add_line$add("Fonte lida diretamente da URL em memória: ", opt$data_url)
add_line$add("Observações: ", N)
add_line$add("Resposta: ", y_name)
add_line$add("Preditores candidatos: ", paste(x_names, collapse = ", "))
add_line$add("Número total de modelos: ", nrow(res_tbl))
add_line$add("Estados na base: ", paste(sat_raw$state, collapse = ", "))
add_line$blank()

base_stats <- data.frame(
  variavel = c(y_name, x_names),
  media = c(mean(y_raw), colMeans(X_raw)),
  dp = c(stats::sd(y_raw), apply(X_raw, 2L, stats::sd)),
  min = c(min(y_raw), apply(X_raw, 2L, min)),
  max = c(max(y_raw), apply(X_raw, 2L, max))
)
add_line$add("Resumo descritivo das variáveis usadas:")
add_line$block(utils::capture.output(print(base_stats, row.names = FALSE)))
add_line$blank()

add_line$add("[2] VERIFICAÇÃO DO AJUSTE MCMC/HMC POR MODELO")
diag_tbl <- res_tbl[, c(
  "model_id", "label", "k", "post_prob", x_names,
  "max_rhat", "min_ess_bulk", "min_ess_tail",
  "divergences", "treedepth_hits", "min_bfmi"
)]
diag_tbl$post_prob <- round(diag_tbl$post_prob, 4)
diag_tbl$max_rhat <- round(diag_tbl$max_rhat, 4)
diag_tbl$min_ess_bulk <- round(diag_tbl$min_ess_bulk, 1)
diag_tbl$min_ess_tail <- round(diag_tbl$min_ess_tail, 1)
diag_tbl$min_bfmi <- round(diag_tbl$min_bfmi, 3)
diag_tbl <- diag_tbl[order(diag_tbl$post_prob, decreasing = TRUE), ]
add_line$block(utils::capture.output(print(diag_tbl, row.names = FALSE)))
add_line$blank()
add_line$add("Critérios práticos de aceitação:")
add_line$add("- max_rhat ideal <= 1.01")
add_line$add("- min_ess_bulk e min_ess_tail suficientemente altos")
add_line$add("- divergences = 0")
add_line$add("- treedepth_hits = 0")
add_line$add("- min_bfmi > 0.30")
add_line$blank()

add_line$add("[3] CHECAGENS PREDITIVAS A POSTERIORI (PPC) POR MODELO")
ppc_tbl <- res_tbl[, c(
  "model_id", "label", "post_prob",
  "ppp_mean", "ppp_sd", "ppp_min", "ppp_max",
  "rmse_med", "rmse_p10", "rmse_p90"
)]
ppc_tbl$post_prob <- round(ppc_tbl$post_prob, 4)
ppc_tbl$ppp_mean <- round(ppc_tbl$ppp_mean, 3)
ppc_tbl$ppp_sd <- round(ppc_tbl$ppp_sd, 3)
ppc_tbl$ppp_min <- round(ppc_tbl$ppp_min, 3)
ppc_tbl$ppp_max <- round(ppc_tbl$ppp_max, 3)
ppc_tbl$rmse_med <- round(ppc_tbl$rmse_med, 2)
ppc_tbl$rmse_p10 <- round(ppc_tbl$rmse_p10, 2)
ppc_tbl$rmse_p90 <- round(ppc_tbl$rmse_p90, 2)
ppc_tbl <- ppc_tbl[order(ppc_tbl$post_prob, decreasing = TRUE), ]
add_line$block(utils::capture.output(print(ppc_tbl, row.names = FALSE)))
add_line$blank()
add_line$add("Leitura sugerida dos PPC:")
add_line$add("- p-values próximos de 0.5 indicam reprodução razoável da estatística observada")
add_line$add("- valores muito próximos de 0 ou 1 sugerem falha do modelo naquela dimensão")
add_line$blank()

add_line$add("[4] COMPARAÇÃO ENTRE MODELOS - EVIDÊNCIA MARGINAL")
post_tbl <- res_by_post[, c("model_id", "label", "k", "post_prob", "logml", "log_BF_vs_null", "BF_vs_null")]
post_tbl$post_prob <- round(post_tbl$post_prob, 4)
post_tbl$logml <- round(post_tbl$logml, 2)
post_tbl$log_BF_vs_null <- round(post_tbl$log_BF_vs_null, 2)
post_tbl$BF_vs_null <- ifelse(
  is.na(post_tbl$BF_vs_null),
  NA_character_,
  formatC(post_tbl$BF_vs_null, digits = 3, format = "fg")
)
add_line$block(utils::capture.output(print(post_tbl, row.names = FALSE)))
add_line$blank()

add_line$add("[5] COMPARAÇÃO ENTRE MODELOS - PSIS-LOO")
loo_tbl <- res_tbl[, c("model_id", "label", "elpd_loo", "se_elpd_loo", "p_loo", "looic", "n_k_gt_05", "n_k_gt_07", "n_k_gt_10")]
loo_tbl$elpd_loo <- round(loo_tbl$elpd_loo, 2)
loo_tbl$se_elpd_loo <- round(loo_tbl$se_elpd_loo, 2)
loo_tbl$p_loo <- round(loo_tbl$p_loo, 2)
loo_tbl$looic <- round(loo_tbl$looic, 2)
loo_tbl <- loo_tbl[order(loo_tbl$elpd_loo, decreasing = TRUE), ]
add_line$block(utils::capture.output(print(loo_tbl, row.names = FALSE)))
add_line$blank()
add_line$add("Diferenças de ELPD em relação ao melhor modelo por LOO:")
add_line$block(utils::capture.output(print(loo_cmp_df, row.names = FALSE)))
add_line$blank()
add_line$add("Leitura sugerida do PSIS-LOO:")
add_line$add("- maior elpd_loo = melhor desempenho preditivo fora da amostra")
add_line$add("- muitos Pareto k > 0.7 enfraquecem a confiabilidade do LOO")
add_line$blank()

add_line$add("[6] PROBABILIDADES POSTERIORES DE INCLUSÃO DOS PREDITORES")
incl_tbl <- data.frame(
  preditor = names(incl_prob),
  prob_inclusao = round(as.numeric(incl_prob), 4),
  row.names = NULL
)
add_line$block(utils::capture.output(print(incl_tbl, row.names = FALSE)))
add_line$blank()

add_line$add("[7] BAYESIAN MODEL AVERAGING (BMA) - COEFICIENTES NA ESCALA ORIGINAL")
bma_tbl <- data.frame(
  parametro = c("(Intercept)", x_names),
  estimativa = round(c(alpha_orig_bma, beta_orig_bma), 4),
  row.names = NULL
)
add_line$block(utils::capture.output(print(bma_tbl, row.names = FALSE)))
add_line$blank()

add_line$add("[8] MELHOR MODELO POR EVIDÊNCIA MARGINAL")
add_line$block(utils::capture.output(
  print(best_post_row[, c(
    "model_id", "label", "post_prob", "logml", "elpd_loo", "looic",
    "max_rhat", "divergences", "treedepth_hits", "min_bfmi"
  )], row.names = FALSE)
))
add_line$blank()

add_line$add("[9] MELHOR MODELO POR PSIS-LOO")
add_line$block(utils::capture.output(
  print(best_loo_row[, c(
    "model_id", "label", "post_prob", "logml", "elpd_loo", "looic",
    "max_rhat", "divergences", "treedepth_hits", "min_bfmi"
  )], row.names = FALSE)
))
add_line$blank()

add_line$add("[10] CONCLUSÃO TÉCNICA AUTOMÁTICA")
if (all(res_tbl$divergences == 0) &&
    all(res_tbl$treedepth_hits == 0) &&
    all(res_tbl$max_rhat <= 1.01) &&
    all(res_tbl$min_bfmi > 0.30)) {
  add_line$add("Todos os modelos passaram nos critérios principais de amostragem HMC.")
} else {
  add_line$add("Há pelo menos um modelo com alerta de amostragem HMC; ver tabela de diagnósticos.")
}

if (sum(res_tbl$n_k_gt_07) == 0) {
  add_line$add("O PSIS-LOO não apresentou observações problemáticas com Pareto k > 0.7.")
} else {
  add_line$add("O PSIS-LOO apresentou observações problemáticas em pelo menos um modelo; interpretar com cautela.")
}

add_line$add("Melhor modelo por evidência marginal: ", best_post_row$label)
add_line$add("Melhor modelo por desempenho preditivo (LOO): ", best_loo_row$label)
add_line$add(
  "Preditor(es) com maior probabilidade posterior de inclusão: ",
  paste(
    incl_tbl$preditor[order(incl_tbl$prob_inclusao, decreasing = TRUE)][1:min(3, nrow(incl_tbl))],
    collapse = ", "
  )
)
add_line$blank()

add_line$print()
