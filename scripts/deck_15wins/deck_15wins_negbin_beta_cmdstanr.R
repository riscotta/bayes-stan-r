#!/usr/bin/env Rscript

############################################################
# Estudo (Desenho B): "Quantas partidas pra chegar em r vitórias?"
# - Dados FAKE (simulados)
# - Modelo Bayesiano (conjugado) + Stan via cmdstanr
#     por deck: p[d] ~ Beta(a0, b0)
#     perdas antes de r vitórias: losses ~ NegBin(r, p)
#       (número de falhas antes de r sucessos)
# - Validação "padrão-ouro" (sem LOO/WAIC):
#     (1) Recuperação de parâmetros (p_true dentro do IC crível)
#     (2) Checagem analítica (posterior Beta conjugado) vs Stan
#     (3) Diagnósticos HMC: divergências, treedepth, Rhat, ESS, E-BFMI
#     (4) Posterior predictive check (PPC) numérico
# - Saída: console-only.
#
# Rode a partir do ROOT do repo:
#   Rscript scripts/deck_15wins/deck_15wins_negbin_beta_cmdstanr.R
#
# Opções (formato --chave=valor):
#   --D=4
#   --p_true=0.48,0.52,0.55,0.60
#   --deck_names=Deck_A,Deck_B,Deck_C,Deck_D
#   --r=15
#   --S_per_deck=12
#   --a0=2
#   --b0=2
#   --M_prior=20000
#   --prior_only=0|1
#   --chains=4
#   --iter_warmup=1000
#   --iter_sampling=1000
#   --seed=20260211
#   --adapt_delta=0.95
#   --max_treedepth=12
#   --refresh=0
############################################################

# ----------------------------
# 0) Helpers (args + checks)
# ----------------------------
parse_args <- function(args) {
  out <- list(
    D = 4L,
    p_true = "0.48,0.52,0.55,0.60",
    deck_names = "Deck_A,Deck_B,Deck_C,Deck_D",
    r = 15L,
    S_per_deck = 12L,
    a0 = 2,
    b0 = 2,
    M_prior = 20000L,
    prior_only = 0L,
    chains = 4L,
    iter_warmup = 1000L,
    iter_sampling = 1000L,
    seed = 20260211L,
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
  out$D <- as.integer(out$D)
  out$r <- as.integer(out$r)
  out$S_per_deck <- as.integer(out$S_per_deck)
  out$a0 <- as.numeric(out$a0)
  out$b0 <- as.numeric(out$b0)
  out$M_prior <- as.integer(out$M_prior)
  out$prior_only <- as.integer(out$prior_only)
  out$chains <- as.integer(out$chains)
  out$iter_warmup <- as.integer(out$iter_warmup)
  out$iter_sampling <- as.integer(out$iter_sampling)
  out$seed <- as.integer(out$seed)
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

parse_csv_num <- function(x) {
  # aceita "0.48,0.52" ou "0.48;0.52" ou "0.48 0.52"
  x <- gsub(";", ",", x, fixed = TRUE)
  x <- gsub(" +", ",", x)
  x <- gsub("\t+", ",", x)
  x <- trimws(x)
  if (nchar(x) == 0) return(numeric(0))
  as.numeric(strsplit(x, ",", fixed = TRUE)[[1]])
}

parse_csv_chr <- function(x) {
  x <- gsub(";", ",", x, fixed = TRUE)
  x <- gsub(" +", ",", x)
  x <- gsub("\t+", ",", x)
  x <- trimws(x)
  if (nchar(x) == 0) return(character(0))
  trimws(strsplit(x, ",", fixed = TRUE)[[1]])
}

args <- commandArgs(trailingOnly = TRUE)
opt <- parse_args(args)

pkgs <- c("cmdstanr", "posterior")
require_pkgs(pkgs)

library(cmdstanr)
library(posterior)

options(mc.cores = parallel::detectCores())
set.seed(opt$seed)

check_cmdstan()

# ----------------------------
# 1) Parâmetros do estudo + dados FAKE
# ----------------------------
stopifnot(opt$D >= 1L, opt$r >= 1L, opt$S_per_deck >= 1L)
stopifnot(opt$a0 > 0, opt$b0 > 0)
stopifnot(opt$chains >= 1L, opt$iter_warmup >= 0L, opt$iter_sampling >= 1L)
stopifnot(opt$adapt_delta > 0 && opt$adapt_delta < 1)
stopifnot(opt$max_treedepth >= 1L)
stopifnot(opt$prior_only %in% c(0L, 1L))

p_true <- parse_csv_num(opt$p_true)
deck_names <- parse_csv_chr(opt$deck_names)

if (length(p_true) == 0) stop("--p_true vazio.", call. = FALSE)
if (any(!is.finite(p_true)) || any(p_true <= 0 | p_true >= 1)) {
  stop("--p_true deve estar em (0,1) e numérico.", call. = FALSE)
}

# Ajuste robusto D/p_true/nomes
if (length(p_true) < opt$D) {
  stop(
    "Você pediu D=", opt$D, " decks, mas forneceu apenas ", length(p_true), " valores em --p_true.",
    call. = FALSE
  )
}
if (length(deck_names) < opt$D) {
  # completa com nomes genéricos
  deck_names <- c(deck_names, paste0("Deck_", seq_len(opt$D)))
}

p_true <- p_true[seq_len(opt$D)]
deck_names <- deck_names[seq_len(opt$D)]

D <- opt$D
r_target <- opt$r
S_per_deck <- opt$S_per_deck
S <- D * S_per_deck

deck_id <- rep(seq_len(D), each = S_per_deck)

# losses[s] = derrotas antes de completar r vitórias
losses <- integer(S)
N_games <- integer(S)

for (s in seq_len(S)) {
  d <- deck_id[s]
  losses[s] <- stats::rnbinom(1, size = r_target, prob = p_true[d])
  N_games[s] <- r_target + losses[s]
}

deck_tbl <- data.frame(
  deck_id = seq_len(D),
  deck = deck_names,
  p_true = p_true,
  sessions = as.integer(tabulate(deck_id, nbins = D)),
  total_losses = as.integer(tapply(losses, deck_id, sum)),
  total_games = as.integer(tapply(N_games, deck_id, sum)),
  mean_games_per_rwins = as.numeric(tapply(N_games, deck_id, mean)),
  sd_games_per_rwins = as.numeric(tapply(N_games, deck_id, stats::sd))
)

# ----------------------------
# 2) Prior + prior predictive (sanidade)
# ----------------------------
a0 <- opt$a0
b0 <- opt$b0

M_prior <- opt$M_prior
if (!is.finite(M_prior) || M_prior < 1000L) M_prior <- 1000L

p_prior <- stats::rbeta(M_prior, a0, b0)
losses_prior <- stats::rnbinom(M_prior, size = r_target, prob = p_prior)
N_prior <- r_target + losses_prior
prior_pred_quant <- stats::quantile(N_prior, probs = c(0.50, 0.80, 0.90, 0.95, 0.99), names = TRUE)

# ----------------------------
# 3) Stan (cmdstanr) — NegBin parametrizada como no Stan
# ----------------------------
stan_code <- "
data {
  int<lower=1> D;
  int<lower=1> S;
  array[S] int<lower=1, upper=D> deck_id;
  int<lower=1> r;
  array[S] int<lower=0> losses;
  real<lower=0> a0;
  real<lower=0> b0;
  int<lower=0, upper=1> prior_only;
}
parameters {
  vector<lower=0, upper=1>[D] p;
}
model {
  p ~ beta(a0, b0);

  if (prior_only == 0) {
    for (s in 1:S) {
      // Stan neg_binomial_lpmf(n | alpha, beta) tem pmf:
      //   C(n+alpha-1,n) * (beta/(beta+1))^alpha * (1/(beta+1))^n
      // Queremos: C(l+r-1,l) * p^r * (1-p)^l
      // => beta/(beta+1) = p  => beta = p/(1-p)
      real beta_nb = p[deck_id[s]] / (1 - p[deck_id[s]]);
      target += neg_binomial_lpmf(losses[s] | r, beta_nb);
    }
  }
}
generated quantities {
  vector[D] expected_N;
  for (d in 1:D) expected_N[d] = r / p[d];
}
"

stan_data <- list(
  D = D,
  S = S,
  deck_id = deck_id,
  r = r_target,
  losses = losses,
  a0 = a0,
  b0 = b0,
  prior_only = opt$prior_only
)

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
# 4) Diagnósticos HMC (divergências, treedepth, E-BFMI)
# ----------------------------
diag <- posterior::as_draws_array(fit$sampler_diagnostics())

# dims típicos: iterations x chains x vars
# vars incluem divergent__, treedepth__, energy__ ...
get_var <- function(arr, name) {
  vn <- dimnames(arr)[[3]]
  if (is.null(vn) || !(name %in% vn)) return(NULL)
  arr[, , which(vn == name), drop = TRUE]
}

div <- get_var(diag, "divergent__")
td  <- get_var(diag, "treedepth__")
eng <- get_var(diag, "energy__")

n_divergent <- if (is.null(div)) NA_integer_ else sum(div)
n_treedepth <- if (is.null(td)) NA_integer_ else sum(td >= opt$max_treedepth)

e_bfmi <- rep(NA_real_, opt$chains)
if (!is.null(eng)) {
  for (ch in seq_len(opt$chains)) {
    e <- eng[, ch]
    e_bfmi[ch] <- mean(diff(e)^2) / stats::var(e)
  }
}

# ----------------------------
# 5) Posterior draws + sumários (Rhat/ESS)
# ----------------------------
# draws para p e expected_N
p_draws <- posterior::as_draws_matrix(fit$draws(variables = "p"))
EN_draws <- posterior::as_draws_matrix(fit$draws(variables = "expected_N"))

# Matriz n_draws x D
p_mat <- as.matrix(p_draws)
EN_mat <- as.matrix(EN_draws)

# Ordenar colunas (p[1]..p[D]) garantindo estabilidade
p_cols <- paste0("p[", seq_len(D), "]")
EN_cols <- paste0("expected_N[", seq_len(D), "]")

p_mat <- p_mat[, p_cols, drop = FALSE]
EN_mat <- EN_mat[, EN_cols, drop = FALSE]

# Diagnósticos summary (posterior)
sum_draws <- posterior::summarise_draws(
  fit$draws(variables = c("p", "expected_N")),
  "mean", "sd", "q5", "q50", "q95", "rhat", "ess_bulk", "ess_tail"
)

max_rhat <- max(sum_draws$rhat, na.rm = TRUE)
min_ess  <- min(sum_draws$ess_bulk, sum_draws$ess_tail, na.rm = TRUE)

# Quantis 2.5/50/97.5
q_mat <- function(mat, probs = c(0.025, 0.50, 0.975)) {
  t(apply(mat, 2, stats::quantile, probs = probs))
}

p_ci <- q_mat(p_mat)
EN_ci <- q_mat(EN_mat)

p_mean <- colMeans(p_mat)
EN_mean <- colMeans(EN_mat)

best_idx <- apply(p_mat, 1, which.max)
prob_best <- tabulate(best_idx, nbins = D) / length(best_idx)

# ----------------------------
# 6) Validação conjugada (Analítico) vs Stan
# ----------------------------
S_by_deck <- as.integer(tabulate(deck_id, nbins = D))
L_by_deck <- as.integer(tapply(losses, deck_id, sum))

a_post <- a0 + r_target * S_by_deck
b_post <- b0 + L_by_deck

p_ana_mean <- a_post / (a_post + b_post)
p_ana_ci <- cbind(
  q025 = stats::qbeta(0.025, a_post, b_post),
  q50  = stats::qbeta(0.50,  a_post, b_post),
  q975 = stats::qbeta(0.975, a_post, b_post)
)

delta_mean <- p_mean - p_ana_mean
delta_q50  <- p_ci[, 2] - p_ana_ci[, 2]

covered_95 <- (p_true >= p_ci[, 1]) & (p_true <= p_ci[, 3])

# ----------------------------
# 7) PPC numérico: média de N por deck
# ----------------------------
# PPC usando draws de p (mais barato que re-amostrar Stan)
set.seed(opt$seed + 1L)
M_ppc <- min(2000L, nrow(p_mat))
ppc_idx <- sample.int(nrow(p_mat), M_ppc)

ppc_meanN <- matrix(NA_real_, nrow = M_ppc, ncol = D)

for (i in seq_len(M_ppc)) {
  p_i <- p_mat[ppc_idx[i], ]
  for (d in seq_len(D)) {
    losses_rep <- stats::rnbinom(S_per_deck, size = r_target, prob = p_i[d])
    N_rep <- r_target + losses_rep
    ppc_meanN[i, d] <- mean(N_rep)
  }
}

obs_meanN <- as.numeric(tapply(N_games, deck_id, mean))

ppc_summary <- data.frame(
  deck = deck_names,
  obs_meanN = obs_meanN,
  ppc_mean = colMeans(ppc_meanN),
  ppc_q025 = apply(ppc_meanN, 2, stats::quantile, probs = 0.025),
  ppc_q50  = apply(ppc_meanN, 2, stats::quantile, probs = 0.50),
  ppc_q975 = apply(ppc_meanN, 2, stats::quantile, probs = 0.975),
  bayes_p  = vapply(seq_len(D), function(d) mean(ppc_meanN[, d] >= obs_meanN[d]), numeric(1))
)

# ----------------------------
# 8) Tabela final consolidada
# ----------------------------
posterior_tbl <- data.frame(
  deck = deck_names,
  p_true = p_true,

  p_mean_stan = p_mean,
  p_q025_stan = p_ci[, 1],
  p_q50_stan  = p_ci[, 2],
  p_q975_stan = p_ci[, 3],

  p_mean_ana = p_ana_mean,
  p_q025_ana = p_ana_ci[, 1],
  p_q50_ana  = p_ana_ci[, 2],
  p_q975_ana = p_ana_ci[, 3],

  delta_mean = delta_mean,
  delta_q50  = delta_q50,

  EN_mean_stan = EN_mean,
  EN_q025_stan = EN_ci[, 1],
  EN_q50_stan  = EN_ci[, 2],
  EN_q975_stan = EN_ci[, 3],

  prob_best = prob_best,
  covered_95 = covered_95
)

# ----------------------------
# 9) Impressão (console)
# ----------------------------
cat("\n============================================================\n")
cat("ESTUDO (DESENHO B) — 'Quantas partidas para chegar em r vitórias?'\n")
cat("Dados FAKE + Modelo Bayesiano (Beta + NegBin) em Stan via cmdstanr\n")
cat("============================================================\n\n")

cat("Config:\n")
cat(sprintf("- D=%d decks | r=%d vitórias | S_per_deck=%d sessões\n", D, r_target, S_per_deck))
cat(sprintf("- Prior: Beta(a0=%g, b0=%g)\n", a0, b0))
cat(sprintf("- prior_only=%d | chains=%d | warmup=%d | sampling=%d\n", opt$prior_only, opt$chains, opt$iter_warmup, opt$iter_sampling))
cat(sprintf("- adapt_delta=%.2f | max_treedepth=%d\n", opt$adapt_delta, opt$max_treedepth))

cat("\n1) Dados simulados (observado por deck)\n")
print(deck_tbl, row.names = FALSE)

cat("\n2) Prior predictive de N (uma sessão até r vitórias)\n")
cat("Quantis de N (prior predictive):\n")
print(prior_pred_quant)

cat("\n3) Diagnósticos HMC / Qualidade\n")
cat(sprintf("- Divergências: %s\n", ifelse(is.na(n_divergent), "NA", as.character(n_divergent))))
cat(sprintf("- Hits em max_treedepth (%d): %s\n", opt$max_treedepth, ifelse(is.na(n_treedepth), "NA", as.character(n_treedepth))))
cat(sprintf("- E-BFMI por cadeia: %s\n", paste(ifelse(is.na(e_bfmi), "NA", sprintf("%.3f", e_bfmi)), collapse = ", ")))
cat(sprintf("- max Rhat: %.4f\n", max_rhat))
cat(sprintf("- min ESS (bulk/tail): %.1f\n", min_ess))

cat("\n4) Posterior (Stan) + Validação conjugada (Analítico)\n")
cat("   * 'delta_*' = Stan - Analítico (deve ficar ~ 0)\n")
cat("   * EN = r/p (partidas esperadas para r vitórias)\n\n")
print(posterior_tbl, row.names = FALSE)

cat("\n5) PPC — média de N por deck\n")
cat("   * bayes_p ~ 0.5 é bom sinal; muito perto de 0 ou 1 sugere misfit\n\n")
print(ppc_summary, row.names = FALSE)

cat("\n6) Checklist de interpretação\n")
cat("- Se divergências > 0: aumente adapt_delta (ex.: 0.99).\n")
cat("- Se hits em treedepth: aumente max_treedepth (ex.: 15).\n")
cat("- Se max Rhat > 1.01 ou ESS baixo: aumente iter_sampling/iter_warmup.\n")
cat("- Se delta_* não ~ 0: tem bug (ou parametrização errada) no Stan.\n")
cat("- covered_95: TRUE indica p_true no IC 95% (recuperação ok).\n")
cat("\n============================================================\n\n")
