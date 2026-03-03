#!/usr/bin/env Rscript

###############################################################################
# INHALER (Ordinal crossover) — variante "mais Bayesiana" (sem hard-bounds)
#
# - Cutpoints via c1 + gaps positivos com softplus (log1p_exp): estabilidade numérica
# - Priors informativos fracos (regularização) ao invés de truncamentos duros
# - Random intercept por sujeito (centrado)
# - Split holdout que garante todas as categorias no treino
#
# Rode a partir do ROOT do repo (sem setwd()):
#   Rscript scripts/inhaler_ordinal/inhaler_ordinal_softplus_cmdstanr.R
#
# Opções (formato --chave=valor):
#   --seed=20260302
#   --chains=4
#   --iter_warmup=1000
#   --iter_sampling=1000
#   --adapt_delta=0.995
#   --max_treedepth=15
#   --refresh=0
#
#   --prob_draws=1000
#
#   --save_plots=1|0
#   --plots_path=outputs/figures/inhaler_ordinal_plots.pdf
#   --save_report=1|0
#   --report_path=outputs/tables/inhaler_ordinal_report.txt
#
#   --data_path=data/raw/inhaler.csv
###############################################################################

# ----------------------------
# 0) Helpers (args + checks)
# ----------------------------
parse_args <- function(args) {
  out <- list(
    seed = 20260302L,

    chains = 4L,
    iter_warmup = 1000L,
    iter_sampling = 1000L,
    adapt_delta = 0.995,
    max_treedepth = 15L,
    refresh = 0L,

    # subamostra de draws para calcular p_hat no teste (mais rápido)
    prob_draws = 1000L,

    # outputs
    save_plots = 1L,
    plots_path = file.path("outputs", "figures", "inhaler_ordinal_plots.pdf"),
    save_report = 1L,
    report_path = file.path("outputs", "tables", "inhaler_ordinal_report.txt"),

    # data (opcional)
    data_path = file.path("data", "raw", "inhaler.csv")
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
  out$prob_draws <- as.integer(out$prob_draws)
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

safe_dir_create <- function(path) {
  dirp <- dirname(path)
  if (!dir.exists(dirp)) dir.create(dirp, recursive = TRUE, showWarnings = FALSE)
}

# ----------------------------
# 0.1) Bootstrap
# ----------------------------
args <- commandArgs(trailingOnly = TRUE)
opt <- parse_args(args)

pkgs <- c("cmdstanr", "posterior", "dplyr", "ggplot2", "tibble")
require_pkgs(pkgs)

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(dplyr)
  library(ggplot2)
  library(tibble)
})

options(stringsAsFactors = FALSE)
options(mc.cores = max(1L, parallel::detectCores()))
set.seed(opt$seed)

check_cmdstan()

# -----------------------------
# 1) Dados (Inhaler)
# -----------------------------
load_inhaler_data <- function(data_path) {
  # 1) Se existir CSV local, usa ele
  if (file.exists(data_path)) {
    # evita depender de readr
    df <- tryCatch(utils::read.csv(data_path, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(df)) stop("Falha ao ler CSV em: ", data_path, call. = FALSE)

    needed <- c("subject", "rating", "treat", "period", "carry")
    miss <- setdiff(needed, names(df))
    if (length(miss) > 0) {
      stop(
        "CSV em ", data_path, " não tem as colunas esperadas: ", paste(miss, collapse = ", "),
        call. = FALSE
      )
    }
    return(df)
  }

  # 2) Caso contrário, tenta brms::inhaler
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop(
      "Dataset 'inhaler' não encontrado em ", data_path, " e o pacote 'brms' não está instalado.\n",
      "Opções:\n",
      "  1) Instale: install.packages('brms')\n",
      "  2) Salve um CSV em data/raw/inhaler.csv e rode com --data_path=...",
      call. = FALSE
    )
  }

  data("inhaler", package = "brms")
  get("inhaler", envir = asNamespace("brms"))
}

dat_raw <- load_inhaler_data(opt$data_path)

dat <- dat_raw %>%
  mutate(
    rating  = ordered(rating),
    y       = as.integer(rating),   # 1..K
    subject = factor(subject),
    treat   = factor(treat),
    period  = factor(period),
    carry   = factor(carry)
  )

K <- nlevels(dat$rating)
if (K < 2) stop("rating precisa ter pelo menos 2 categorias.", call. = FALSE)

# Design matrix SEM intercepto (o nível é absorvido pelos cutpoints)
X_all <- stats::model.matrix(~ treat + period + carry, data = dat)
X_all <- X_all[, -1, drop = FALSE]
P <- ncol(X_all)

subj_id <- as.integer(dat$subject)
J <- nlevels(dat$subject)

# -------------------
# 2) Split treino/teste (holdout)
# -------------------
idx_test <- integer(0)
by_subj <- split(seq_len(nrow(dat)), dat$subject)

for (s in names(by_subj)) {
  rows <- by_subj[[s]]
  if (length(rows) >= 2) idx_test <- c(idx_test, sample(rows, 1))
}
idx_test <- sort(unique(idx_test))
idx_train <- setdiff(seq_len(nrow(dat)), idx_test)

if (length(idx_test) == 0) {
  idx_test <- sample(seq_len(nrow(dat)), size = max(1, floor(0.2 * nrow(dat))))
  idx_train <- setdiff(seq_len(nrow(dat)), idx_test)
}

ensure_all_levels_in_train <- function(idx_train, idx_test, y, K) {
  repeat {
    counts_tr <- tabulate(y[idx_train], nbins = K)
    missing <- which(counts_tr == 0)
    if (length(missing) == 0) break

    moved_any <- FALSE
    for (k in missing) {
      cand <- idx_test[y[idx_test] == k]
      if (length(cand) > 0) {
        move <- cand[1]
        idx_test  <- setdiff(idx_test, move)
        idx_train <- c(idx_train, move)
        moved_any <- TRUE
      }
    }
    if (!moved_any) break
  }

  if (length(idx_test) == 0) {
    counts_tr <- tabulate(y[idx_train], nbins = K)
    y_tr_now <- y[idx_train]
    movable <- idx_train[counts_tr[y_tr_now] > 1]
    if (length(movable) > 0) {
      move <- sample(movable, 1)
      idx_test  <- move
      idx_train <- setdiff(idx_train, move)
    } else {
      idx_test <- integer(0)
    }
  }

  list(train = sort(idx_train), test = sort(idx_test))
}

adj <- ensure_all_levels_in_train(idx_train, idx_test, y = dat$y, K = K)
idx_train <- adj$train
idx_test  <- adj$test

y_tr <- as.integer(dat$y[idx_train])
y_te <- as.integer(dat$y[idx_test])

X_tr <- X_all[idx_train, , drop = FALSE]
X_te <- X_all[idx_test,  , drop = FALSE]

subj_tr <- as.integer(subj_id[idx_train])
subj_te <- as.integer(subj_id[idx_test])

N_tr <- length(idx_train)
N_te <- length(idx_test)

if (any(tabulate(y_tr, nbins = K) == 0)) {
  stop("Ainda ficou alguma categoria vazia no treino. Revise o split.", call. = FALSE)
}
if (N_te == 0) {
  stop("Teste ficou vazio após o ajuste. Aumente N_total ou mude o split.", call. = FALSE)
}

cat("\nContagens por categoria (TREINO):\n"); print(table(y_tr))
cat("\nContagens por categoria (TESTE):\n"); print(table(y_te))

# ------------------------
# 3) Priors "mais Bayesianos" p/ cutpoints
# ------------------------
# Alvo: quantis da logística padrão
c0 <- stats::qlogis((1:(K - 1)) / K)              # alvo de localização (K-1)
d0 <- if (K > 2) mean(diff(c0)) else 1.0         # gap típico

min_gap <- 0.05
# Softplus(0) = log(2). Aproxima E[gap] em torno de d0.
delta_scale <- max(0.10, (d0 - min_gap) / log(2))

# desvios permitidos
c_sd <- 1.0
delta_sd <- 0.8

# -----------------------------
# 4) Stan code
# -----------------------------
stan_code <- "
functions {
  real softplus(real x) {
    return log1p_exp(x);
  }
}
data {
  int<lower=1> N_tr;
  int<lower=1> N_te;
  int<lower=2> K;

  array[N_tr] int<lower=1, upper=K> y_tr;
  array[N_te] int<lower=1, upper=K> y_te;

  int<lower=1> P;
  matrix[N_tr, P] X_tr;
  matrix[N_te, P] X_te;

  int<lower=1> J;
  array[N_tr] int<lower=1, upper=J> subj_tr;
  array[N_te] int<lower=1, upper=J> subj_te;

  int<lower=0, upper=1> prior_only;

  vector[K-1] c0;
  real<lower=0> c_sd;
  real<lower=0> min_gap;
  real<lower=0> delta_scale;
  real<lower=0> delta_sd;
}
parameters {
  vector[P] beta;

  vector[J] z_u;
  real<lower=0> sigma_u;

  real c1;
  vector[K-2] delta_raw;
}
transformed parameters {
  vector[J] u;
  ordered[K-1] c;

  u = sigma_u * (z_u - mean(z_u));

  c[1] = c1;
  if (K > 2) {
    for (k in 2:(K-1)) {
      c[k] = c[k-1] + min_gap + delta_scale * softplus(delta_raw[k-1]);
    }
  }

  vector[N_tr] eta_tr;
  vector[N_te] eta_te;

  for (n in 1:N_tr)
    eta_tr[n] = X_tr[n] * beta + u[subj_tr[n]];

  for (n in 1:N_te)
    eta_te[n] = X_te[n] * beta + u[subj_te[n]];
}
model {
  beta    ~ normal(0, 1.5);

  z_u     ~ normal(0, 1);
  sigma_u ~ student_t(3, 0, 0.7);

  c1 ~ normal(c0[1], c_sd);
  if (K > 2)
    delta_raw ~ normal(0, delta_sd);

  if (prior_only == 0) {
    y_tr ~ ordered_logistic(eta_tr, c);
  }
}
generated quantities {
  array[N_tr] int<lower=1, upper=K> yrep_tr;
  array[N_te] int<lower=1, upper=K> yrep_te;

  vector[N_te] log_lik_te;

  vector[N_te] eta_te_out = eta_te;
  vector[K-1]  c_out      = c;

  for (n in 1:N_tr)
    yrep_tr[n] = ordered_logistic_rng(eta_tr[n], c);

  for (n in 1:N_te) {
    yrep_te[n] = ordered_logistic_rng(eta_te[n], c);
    log_lik_te[n] = ordered_logistic_lpmf(y_te[n] | eta_te[n], c);
  }
}
"

# ------------------------
# 5) Compilar e amostrar
# ------------------------
tmp_dir <- file.path(tempdir(), paste0("inhaler_stan_", as.integer(Sys.time())))
dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)

on.exit({
  try(unlink(tmp_dir, recursive = TRUE, force = TRUE), silent = TRUE)
}, add = TRUE)

stan_file <- file.path(tmp_dir, "inhaler_ordinal_softplus.stan")
writeLines(stan_code, stan_file)

mod <- cmdstanr::cmdstan_model(stan_file, quiet = TRUE)

stan_data_post <- list(
  N_tr = N_tr, N_te = N_te, K = K,
  y_tr = y_tr, y_te = y_te,
  P = P, X_tr = X_tr, X_te = X_te,
  J = J, subj_tr = subj_tr, subj_te = subj_te,
  prior_only = 0,
  c0 = c0, c_sd = c_sd,
  min_gap = min_gap, delta_scale = delta_scale, delta_sd = delta_sd
)

stan_data_prior <- stan_data_post
stan_data_prior$prior_only <- 1

parallel_chains <- min(opt$chains, parallel::detectCores())

init_fun <- function() {
  list(
    beta = rep(0, P),
    z_u = rnorm(J, 0, 0.1),
    sigma_u = 0.3,
    c1 = c0[1],
    delta_raw = if (K > 2) rep(0, K - 2) else numeric(0)
  )
}

fit_post <- mod$sample(
  data = stan_data_post,
  seed = opt$seed,
  chains = opt$chains,
  parallel_chains = parallel_chains,
  iter_warmup = opt$iter_warmup,
  iter_sampling = opt$iter_sampling,
  refresh = opt$refresh,
  adapt_delta = opt$adapt_delta,
  max_treedepth = opt$max_treedepth,
  init = init_fun,
  show_messages = FALSE
)

fit_prior <- mod$sample(
  data = stan_data_prior,
  seed = opt$seed,
  chains = 2,
  parallel_chains = min(2, parallel_chains),
  iter_warmup = 0,
  iter_sampling = 1500,
  refresh = 0,
  fixed_param = TRUE,
  show_messages = FALSE
)

# -----------------------
# 6) Funções de checagem
# -----------------------
ordered_logistic_probs <- function(eta, c) {
  N <- length(eta)
  K <- length(c) + 1
  Fk <- sapply(seq_len(K - 1), function(k) stats::plogis(c[k] - eta))
  p <- matrix(NA_real_, nrow = N, ncol = K)
  p[, 1] <- Fk[, 1]
  if (K > 2) {
    for (k in 2:(K - 1)) p[, k] <- Fk[, k] - Fk[, k - 1]
  }
  p[, K] <- 1 - Fk[, K - 1]
  p
}

randomized_pit <- function(p_mat, y_int) {
  N <- nrow(p_mat); K <- ncol(p_mat)
  cdf <- t(apply(p_mat, 1, cumsum))
  u <- numeric(N)
  for (i in 1:N) {
    yi <- y_int[i]
    lo <- if (yi == 1) 0 else cdf[i, yi - 1]
    hi <- cdf[i, yi]
    u[i] <- stats::runif(1, lo, hi)
  }
  u
}

rps_ordinal <- function(p_mat, y_int) {
  N <- nrow(p_mat); K <- ncol(p_mat)
  cdf <- t(apply(p_mat, 1, cumsum))
  y_cdf <- matrix(0, nrow = N, ncol = K)
  for (i in 1:N) y_cdf[i, y_int[i]:K] <- 1
  rowSums((cdf[, 1:(K-1), drop = FALSE] - y_cdf[, 1:(K-1), drop = FALSE])^2)
}

brier_multiclass <- function(p_mat, y_int) {
  N <- nrow(p_mat); K <- ncol(p_mat)
  Y <- matrix(0, nrow = N, ncol = K)
  Y[cbind(seq_len(N), y_int)] <- 1
  rowSums((p_mat - Y)^2)
}

ece_thresholds <- function(p_mat, y_int, bins = 10) {
  N <- nrow(p_mat); K <- ncol(p_mat)
  cdf <- t(apply(p_mat, 1, cumsum))
  out <- vector("list", K - 1)
  for (k in 1:(K - 1)) {
    q <- cdf[, k]
    z <- as.integer(y_int <= k)
    brks <- stats::quantile(q, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE)
    brks <- unique(brks)
    if (length(brks) < 3) { out[[k]] <- list(k = k, ece = NA_real_, df = NULL); next }
    g <- cut(q, breaks = brks, include.lowest = TRUE)
    df <- data.frame(q = q, z = z, g = g) %>%
      dplyr::group_by(g) %>%
      dplyr::summarise(n = dplyr::n(), q_bar = mean(q), z_bar = mean(z), .groups = "drop")
    ece <- sum(df$n / N * abs(df$z_bar - df$q_bar))
    out[[k]] <- list(k = k, ece = ece, df = df)
  }
  out
}

compute_bfmi <- function(energy_vec) {
  if (length(energy_vec) < 3) return(NA_real_)
  dE <- diff(energy_vec)
  stats::mean(dE^2) / stats::var(energy_vec)
}

extract_array_draws_mat <- function(da, var_prefix) {
  idx <- grep(paste0("^", var_prefix, "\\["), dimnames(da)$variable)
  m <- posterior::as_draws_matrix(da[, , idx, drop = FALSE])
  vn <- colnames(m)
  ord <- order(as.integer(gsub(".*\\[|\\].*", "", vn)))
  m[, ord, drop = FALSE]
}

freq_rep_summary <- function(yrep_mat, K) {
  S <- nrow(yrep_mat)
  freqs <- matrix(0, nrow = S, ncol = K)
  for (s in 1:S) freqs[s, ] <- tabulate(yrep_mat[s, ], nbins = K)
  tibble::tibble(
    k = 1:K,
    med = apply(freqs, 2, stats::median),
    lo  = apply(freqs, 2, stats::quantile, probs = 0.05),
    hi  = apply(freqs, 2, stats::quantile, probs = 0.95)
  )
}

plot_ppc_bars <- function(df, title) {
  ggplot(df, aes(x = factor(k))) +
    geom_col(aes(y = obs), alpha = 0.35) +
    geom_point(aes(y = med), size = 2) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15) +
    labs(x = "Categoria (rating)", y = "Frequência", title = title,
         subtitle = "Barras: observado | Ponto+IC90%: replicado") +
    theme_minimal(base_size = 12)
}

# -----------------------
# 7) Extrair draws e checks
# -----------------------
sum_df <- fit_post$summary()

sdx <- fit_post$sampler_diagnostics()
param_names <- dimnames(sdx)$variable

get_diag_param <- function(name) {
  idx <- which(param_names == name)
  if (length(idx) != 1) return(NULL)
  sdx[, , idx, drop = TRUE]
}

div_mat <- get_diag_param("divergent__")
treedepth_mat <- get_diag_param("treedepth__")
energy_mat <- get_diag_param("energy__")

n_div <- if (!is.null(div_mat)) sum(div_mat) else NA_integer_
max_td <- if (!is.null(treedepth_mat)) max(treedepth_mat) else NA_integer_
bfmi_by_chain <- if (!is.null(energy_mat)) apply(energy_mat, 2, compute_bfmi) else rep(NA_real_, opt$chains)

# draws
post_vars <- c("beta", "sigma_u", "c_out", "yrep_tr", "yrep_te", "log_lik_te", "eta_te_out")
prior_vars <- c("yrep_tr", "yrep_te")

draws_post <- fit_post$draws(variables = post_vars)
draws_prior <- fit_prior$draws(variables = prior_vars)

da_post <- posterior::as_draws_array(draws_post)
da_prior <- posterior::as_draws_array(draws_prior)

yrep_tr_post  <- extract_array_draws_mat(da_post,  "yrep_tr")
yrep_te_post  <- extract_array_draws_mat(da_post,  "yrep_te")
yrep_tr_prior <- extract_array_draws_mat(da_prior, "yrep_tr")
yrep_te_prior <- extract_array_draws_mat(da_prior, "yrep_te")

eta_te_mat <- extract_array_draws_mat(da_post, "eta_te_out")
c_mat      <- extract_array_draws_mat(da_post, "c_out")

S_all <- nrow(eta_te_mat)
S_use <- min(S_all, max(50L, opt$prob_draws))
set.seed(opt$seed)
idx_s <- if (S_use == S_all) seq_len(S_all) else sample.int(S_all, S_use)

p_accum <- matrix(0, nrow = N_te, ncol = K)
for (s in idx_s) {
  p_s <- ordered_logistic_probs(eta_te_mat[s, ], c_mat[s, ])
  p_accum <- p_accum + p_s
}

p_hat <- p_accum / length(idx_s)

pred_class <- max.col(p_hat, ties.method = "first")
exp_rating <- as.numeric(p_hat %*% seq_len(K))

acc <- mean(pred_class == y_te)
mae_class <- mean(abs(pred_class - y_te))
mae_exp <- mean(abs(exp_rating - y_te))

log_score <- mean(log(p_hat[cbind(seq_len(N_te), y_te)] + 1e-12))
brier <- mean(brier_multiclass(p_hat, y_te))
rps <- mean(rps_ordinal(p_hat, y_te))

set.seed(opt$seed)
u_pit <- randomized_pit(p_hat, y_te)
ds_resid <- stats::qnorm(u_pit)

cal_list <- ece_thresholds(p_hat, y_te, bins = 10)
ece_vals <- vapply(cal_list, function(x) x$ece, numeric(1))
ece_mean <- mean(ece_vals, na.rm = TRUE)

# PPC marginais
freq_obs_tr <- tabulate(y_tr, nbins = K)
freq_obs_te <- tabulate(y_te, nbins = K)

ppc_tr_post  <- freq_rep_summary(yrep_tr_post,  K) %>% mutate(obs = freq_obs_tr)
ppc_te_post  <- freq_rep_summary(yrep_te_post,  K) %>% mutate(obs = freq_obs_te)
ppc_tr_prior <- freq_rep_summary(yrep_tr_prior, K) %>% mutate(obs = freq_obs_tr)
ppc_te_prior <- freq_rep_summary(yrep_te_prior, K) %>% mutate(obs = freq_obs_te)

g_ppc_tr_post  <- plot_ppc_bars(ppc_tr_post,  "PPC (Posterior) — Treino: distribuição marginal")
g_ppc_te_post  <- plot_ppc_bars(ppc_te_post,  "PPC (Posterior) — Teste: distribuição marginal")
g_ppc_tr_prior <- plot_ppc_bars(ppc_tr_prior, "PPC (Prior) — Treino: distribuição marginal")
g_ppc_te_prior <- plot_ppc_bars(ppc_te_prior, "PPC (Prior) — Teste: distribuição marginal")

g_pit <- ggplot(data.frame(u = u_pit), aes(x = u)) +
  geom_histogram(bins = 20) +
  labs(title = "Calibração (Teste) — Randomized PIT",
       subtitle = "Se calibrado: histograma ~ uniforme",
       x = "u", y = "contagem") +
  theme_minimal(base_size = 12)

g_ds_hist <- ggplot(data.frame(r = ds_resid), aes(x = r)) +
  geom_histogram(bins = 25) +
  labs(title = "Resíduos Dunn–Smyth (Teste)",
       subtitle = "Se calibrado: ~ Normal(0,1)",
       x = "resíduo", y = "contagem") +
  theme_minimal(base_size = 12)

g_ds_qq <- ggplot(data.frame(r = ds_resid), aes(sample = r)) +
  stat_qq() + stat_qq_line() +
  labs(title = "QQ-plot Dunn–Smyth (Teste)") +
  theme_minimal(base_size = 12)

cal_plots <- list()
for (k in 1:(K - 1)) {
  obj <- cal_list[[k]]
  if (is.null(obj$df)) next
  d <- obj$df
  cal_plots[[k]] <- ggplot(d, aes(x = q_bar, y = z_bar, size = n)) +
    geom_point(alpha = 0.8) +
    geom_abline(slope = 1, intercept = 0) +
    coord_equal(xlim = c(0,1), ylim = c(0,1)) +
    labs(
      title = paste0("Calibração (Teste) — Threshold k=", k, " (evento: y <= ", k, ")"),
      subtitle = paste0("ECE_k = ", signif(obj$ece, 3)),
      x = "Prob prevista (média no bin)",
      y = "Freq observada (média no bin)"
    ) +
    theme_minimal(base_size = 12) +
    guides(size = "none")
}

conf_mat <- table(
  Observado = factor(y_te, levels = 1:K),
  Predito   = factor(pred_class, levels = 1:K)
)

# --------------------------
# 8) Relatório final (console + opcional arquivos)
# --------------------------
write_report <- function(con = NULL) {
  if (!is.null(con)) {
    sink(con, type = "output")
    on.exit(sink(type = "output"), add = TRUE)
  }

  cat("======================== NUTS DIAGNOSTICS =======================\n")
  cat("Divergences:", n_div, "\n")
  cat("Max treedepth observado:", max_td, "\n")
  cat("BFMI por chain:\n")
  print(bfmi_by_chain)

  cat("\n======================= PARAMETER SUMMARY =======================\n")
  keep_vars <- c(
    grep("^beta\\[", sum_df$variable, value = TRUE),
    "sigma_u",
    grep("^c_out\\[", sum_df$variable, value = TRUE)
  )
  df_par <- sum_df %>% filter(variable %in% keep_vars)

  # compatibilidade cmdstanr: 'median' vs 'q50'
  if (!("q50" %in% names(df_par)) && ("median" %in% names(df_par))) df_par$q50 <- df_par$median

  cols_show <- c("variable", "mean", "sd", "q5", "q50", "q95", "rhat", "ess_bulk", "ess_tail")
  cols_show <- intersect(cols_show, names(df_par))
  print(df_par %>% select(all_of(cols_show)))

  cat("\n==================== TEST (HOLDOUT) METRICS =====================\n")
  print(tibble::tibble(
    metric = c(
      "Accuracy (classe MAP)",
      "MAE (classe MAP)",
      "MAE (E[y])",
      "Log score médio",
      "Brier (multi-classe) médio",
      "RPS (ordinal) médio",
      "ECE médio (thresholds)",
      "Draws usados p/ p_hat"
    ),
    value  = c(acc, mae_class, mae_exp, log_score, brier, rps, ece_mean, length(idx_s))
  ))

  cat("\n======================== CONFUSION MATRIX =======================\n")
  print(conf_mat)

  cat("\n============================= NOTAS =============================\n")
  cat("- Se aparecer divergência/treedepth: aumente adapt_delta e/ou max_treedepth.\n")
  cat("- Para estabilizar métricas, aumente --prob_draws (p_hat usa subamostra de draws).\n")

  invisible(NULL)
}

# console sempre
write_report()

# report em arquivo
if (opt == 1L) {
  safe_dir_create(opt)
  con <- file(opt, open = "wt")
  write_report(con)
  close(con)
  message("\n[OK] Report salvo em: ", opt)
}

# plots
plot_all <- function() {
  print(g_ppc_tr_prior)
  print(g_ppc_te_prior)
  print(g_ppc_tr_post)
  print(g_ppc_te_post)
  print(g_pit)
  print(g_ds_hist)
  print(g_ds_qq)
  if (length(cal_plots) > 0) {
    for (k in seq_along(cal_plots)) if (!is.null(cal_plots[[k]])) print(cal_plots[[k]])
  }
  invisible(NULL)
}

if (opt$save_plots == 1L) {
  safe_dir_create(opt$plots_path)
  grDevices::pdf(opt$plots_path, width = 9, height = 6)
  on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
  plot_all()
  grDevices::dev.off()
  message("[OK] Plots salvos em: ", opt$plots_path)
} else {
  # interativo/console-only
  plot_all()
}

###############################################################################
