#!/usr/bin/env Rscript

###############################################################################
# Kidney (survival com censura) — R + Stan via cmdstanr
# Modelo: Lognormal survival com censura à direita
# Efeitos fixos: age * sex + disease
# Grupo (paciente): (1 + age | patient) com correlação
# Qualidade: prior predictive + diagnósticos HMC + PPC numérico
# Observação: CmdStan escreve arquivos temporários para compilar/amostrar;
#             este script usa tempdir() e remove tudo no final (por padrão).
###############################################################################

options(stringsAsFactors = FALSE)
options(mc.cores = max(1, parallel::detectCores(logical = TRUE)))

args <- commandArgs(trailingOnly = TRUE)
KEEP_TMP <- "--keep-tmp" %in% args

# pequeno helper pra concatenar strings
`%+%` <- function(a, b) paste0(a, b)

#-----------------------------#
# Bootstrap: dependências     #
#-----------------------------#
ensure_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "Pacote '", pkg, "' não está instalado.\n\n",
      "Como resolver (recomendado):\n",
      "  Rscript scripts/_setup/install_deps.R\n\n",
      "Ou instale manualmente no R:\n",
      "  install.packages('\", pkg, "')\n",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

invisible(lapply(c("cmdstanr", "posterior", "survival"), ensure_pkg))

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(survival)
})

#-----------------------------#
# Utils: checagens & helpers  #
#-----------------------------#
stopf <- function(...) stop(sprintf(...), call. = FALSE)

assert_has_cols <- function(df, cols) {
  miss <- setdiff(cols, names(df))
  if (length(miss) > 0) stopf("Faltam colunas no dataset: %s", paste(miss, collapse = ", "))
}

summ_quant <- function(x) {
  qs <- stats::quantile(x, probs = c(0.01, 0.05, 0.5, 0.95, 0.99), na.rm = TRUE)
  c(mean = mean(x, na.rm = TRUE), sd = stats::sd(x, na.rm = TRUE), qs)
}

pp_pvalue <- function(stat_rep, stat_obs) {
  # p-valor PPC “clássico”: P(T_rep >= T_obs)
  mean(stat_rep >= stat_obs)
}

safe_unlink <- function(path) {
  ok <- TRUE
  if (file.exists(path)) {
    tryCatch({
      unlink(path, recursive = TRUE, force = TRUE)
    }, error = function(e) ok <<- FALSE)
  }
  ok
}

#-----------------------------#
# 0) Ambiente CmdStan         #
#-----------------------------#
cmd_ok <- TRUE
cmd_path <- NULL
cmd_ver  <- NULL
tryCatch({
  cmd_path <- cmdstanr::cmdstan_path()
  cmd_ver  <- cmdstanr::cmdstan_version()
}, error = function(e) {
  cmd_ok <<- FALSE
})

if (!cmd_ok) {
  stopf(
    "CmdStan não parece instalado/configurado.\n\n" %+%
      "Faça:\n" %+%
      "  Rscript scripts/_setup/install_deps.R\n" %+%
      "  Rscript scripts/_setup/install_cmdstan.R\n\n" %+%
      "Depois rode novamente."
  )
}

#-----------------------------#
# 1) Carrega e valida dados   #
#-----------------------------#
# Dataset do pacote survival (McGilchrist & Aisbett, 1991)
# Esperado: patient, time, status (1=evento, 0=censurado), age, sex (1/2), disease (0..3)

kidney <- NULL
tryCatch({
  data("kidney", package = "survival", envir = environment())
  kidney <- get("kidney", envir = environment())
}, error = function(e) {
  stop(
    "Não consegui carregar o dataset 'kidney' do pacote survival.\n",
    "Tente:\n  install.packages('survival')\n",
    call. = FALSE
  )
})

req_cols <- c("time", "status", "age", "sex", "disease", "patient")
assert_has_cols(kidney, req_cols)

kidney <- within(kidney, {
  time    <- as.numeric(time)
  age     <- as.numeric(age)
  patient <- factor(patient)

  # survival::kidney usa sex = 1 (male), 2 (female)
  sex <- factor(sex, levels = c(1, 2), labels = c("male", "female"))

  # survival::kidney usa disease = 0 (GN), 1 (AN), 2 (PKD), 3 (Other)
  disease <- factor(disease, levels = c(0, 1, 2, 3), labels = c("GN", "AN", "PKD", "other"))
})

if (anyNA(kidney$time) || any(kidney$time <= 0)) stopf("Problema em 'time': NA ou valores <= 0.")
if (anyNA(kidney$age)) stopf("Problema em 'age': contém NA.")
if (nlevels(kidney$patient) < 2) stopf("Poucos níveis em 'patient'.")

# Converte status (1=evento, 0=censurado) -> censura à direita (1=censurado)
kidney$cens_right <- as.integer(kidney$status == 0)
if (!all(kidney$cens_right %in% c(0L, 1L))) stopf("'cens_right' não ficou binário 0/1.")

# Centraliza idade (boa prática para HMC, especialmente com slope aleatório)
kidney$age_c <- as.numeric(scale(kidney$age, center = TRUE, scale = FALSE))

# Matriz de desenho dos efeitos fixos (inclui intercepto)
X <- model.matrix(~ age_c * sex + disease, data = kidney)

# Índices
N <- nrow(kidney)
J <- nlevels(kidney$patient)
pid <- as.integer(kidney$patient)

# Standata (inclui switch para prior predictive)
stan_data_base <- list(
  N     = N,
  K     = ncol(X),
  X     = X,
  time  = as.vector(kidney$time),
  cens  = as.integer(kidney$cens_right),
  J     = J,
  pid   = pid,
  age_c = as.vector(kidney$age_c),
  prior_only = 0L
)

#-----------------------------#
# 2) Stan model (lognormal +  #
#    censura + RE correl)     #
#-----------------------------#
stan_code <- "
data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N, K] X;

  vector<lower=0>[N] time;
  array[N] int<lower=0, upper=1> cens;  // 1 = right-censored

  int<lower=1> J;
  array[N] int<lower=1, upper=J> pid;

  vector[N] age_c;

  int<lower=0, upper=1> prior_only; // 1: ignora likelihood (prior predictive)
}

parameters {
  vector[K] beta;
  real<lower=0> sigma;

  // random effects por paciente: (intercept, slope_age)
  vector<lower=0>[2] tau;
  cholesky_factor_corr[2] L_Omega;
  matrix[2, J] z_u; // N(0,1) não-centrado
}

transformed parameters {
  matrix[2, J] u;
  u = diag_pre_multiply(tau, L_Omega) * z_u;
}

model {
  // Priors (padrão estável; ajuste fino se necessário)
  beta  ~ normal(0, 2.5);
  sigma ~ exponential(1);

  tau      ~ exponential(1);
  L_Omega  ~ lkj_corr_cholesky(2);
  to_vector(z_u) ~ normal(0, 1);

  if (prior_only == 0) {
    for (i in 1:N) {
      real mu = X[i] * beta
              + u[1, pid[i]]
              + u[2, pid[i]] * age_c[i];

      if (cens[i] == 0)
        target += lognormal_lpdf(time[i] | mu, sigma);
      else
        target += lognormal_lccdf(time[i] | mu, sigma);
    }
  }
}

generated quantities {
  corr_matrix[2] Omega = multiply_lower_tri_self_transpose(L_Omega);

  vector[N] log_lik;
  vector[N] time_rep;
  array[N] int cens_rep;

  for (i in 1:N) {
    real mu = X[i] * beta
            + u[1, pid[i]]
            + u[2, pid[i]] * age_c[i];

    // log-lik por observação (útil pra auditoria)
    if (prior_only == 0) {
      if (cens[i] == 0)
        log_lik[i] = lognormal_lpdf(time[i] | mu, sigma);
      else
        log_lik[i] = lognormal_lccdf(time[i] | mu, sigma);
    } else {
      log_lik[i] = 0;
    }

    // Posterior/ prior predictive: tempo de evento
    time_rep[i] = lognormal_rng(mu, sigma);

    // Para censuradas: checa se o evento simulado cai acima do limiar censurado
    if (cens[i] == 1) cens_rep[i] = (time_rep[i] > time[i]);
    else cens_rep[i] = -1;
  }
}
"

#-----------------------------#
# 3) Compila e roda           #
#-----------------------------#
# Tudo em diretórios temporários (e apagamos no final)
tmp_root   <- file.path(tempdir(), paste0("kidney_cmdstanr_", Sys.getpid()))
tmp_build  <- file.path(tmp_root, "build")
tmp_output <- file.path(tmp_root, "output")

dir.create(tmp_build,  recursive = TRUE, showWarnings = FALSE)
dir.create(tmp_output, recursive = TRUE, showWarnings = FALSE)

# Grava Stan em arquivo temporário (necessário para compilar)
stan_file <- cmdstanr::write_stan_file(stan_code, dir = tmp_build)

mod <- cmdstanr::cmdstan_model(stan_file)

#-----------------------------#
# 3a) Prior predictive        #
#-----------------------------#
prior_data <- stan_data_base
prior_data$prior_only <- 1L

fit_prior <- mod$sample(
  data = prior_data,
  chains = 4, parallel_chains = 4,
  iter_warmup = 0, iter_sampling = 500,
  fixed_param = TRUE,
  seed = 123,
  output_dir = tmp_output,
  refresh = 0
)

prior_time_rep <- as.matrix(fit_prior$draws("time_rep", format = "matrix"))

#-----------------------------#
# 3b) Posterior fit           #
#-----------------------------#
post_data <- stan_data_base
post_data$prior_only <- 0L

fit <- mod$sample(
  data = post_data,
  chains = 4, parallel_chains = 4,
  iter_warmup = 1000, iter_sampling = 1000,
  adapt_delta = 0.97, max_treedepth = 12,
  seed = 20260220,
  output_dir = tmp_output,
  refresh = 200
)

#-----------------------------#
# 4) Diagnósticos (qualidade) #
#-----------------------------#
# 4a) Diagnose CmdStan (inclui E-BFMI e alertas gerais)
diagnose_txt <- capture.output(fit$cmdstan_diagnose())

# 4b) Resumo de amostragem: divergências e treedepth
# diagnostic_summary() tem colunas como: num_divergent, num_max_treedepth, etc.
diag_sum <- fit$diagnostic_summary()
num_div <- sum(diag_sum$num_divergent)
num_td  <- sum(diag_sum$num_max_treedepth)

# 4c) Rhat/ESS para parâmetros “centrais”
# (beta, sigma, tau, Omega)
sum_core <- fit$summary(c("beta", "sigma", "tau", "Omega"))
max_rhat <- suppressWarnings(max(sum_core$rhat, na.rm = TRUE))
min_essb <- suppressWarnings(min(sum_core$ess_bulk, na.rm = TRUE))
min_esst <- suppressWarnings(min(sum_core$ess_tail, na.rm = TRUE))

bad_rhat <- sum_core[!is.na(sum_core$rhat) & sum_core$rhat > 1.01, , drop = FALSE]

#-----------------------------#
# 5) PPC numérico (posterior) #
#-----------------------------#
idx_obs  <- which(kidney$cens_right == 0L)
idx_cens <- which(kidney$cens_right == 1L)

log_time_obs <- log(kidney$time[idx_obs])

obs_stats <- c(
  mean_log_time = mean(log_time_obs),
  sd_log_time   = sd(log_time_obs),
  q05_log_time  = unname(quantile(log_time_obs, 0.05)),
  q50_log_time  = unname(quantile(log_time_obs, 0.50)),
  q95_log_time  = unname(quantile(log_time_obs, 0.95))
)

# time_rep: draws x N
# (não precisa carregar tudo; mas é leve aqui)
time_rep_mat <- as.matrix(fit$draws("time_rep", format = "matrix"))

# Para não ficar pesado, amostra no máximo 800 draws para PPC
nd <- nrow(time_rep_mat)
set.seed(1)
keep <- if (nd > 800) sample.int(nd, 800) else seq_len(nd)
time_rep_mat <- time_rep_mat[keep, , drop = FALSE]

log_time_rep <- log(time_rep_mat[, idx_obs, drop = FALSE])

rep_mean <- rowMeans(log_time_rep)
rep_sd   <- apply(log_time_rep, 1, sd)

rep_q05 <- apply(log_time_rep, 1, quantile, probs = 0.05)
rep_q50 <- apply(log_time_rep, 1, quantile, probs = 0.50)
rep_q95 <- apply(log_time_rep, 1, quantile, probs = 0.95)

ppc_pvals <- c(
  p_mean_log_time = pp_pvalue(rep_mean, obs_stats["mean_log_time"]),
  p_sd_log_time   = pp_pvalue(rep_sd,   obs_stats["sd_log_time"]),
  p_q05_log_time  = pp_pvalue(rep_q05,  obs_stats["q05_log_time"]),
  p_q50_log_time  = pp_pvalue(rep_q50,  obs_stats["q50_log_time"]),
  p_q95_log_time  = pp_pvalue(rep_q95,  obs_stats["q95_log_time"])
)

# Para censurados: prob(evento > tempo_censura) por obs censurada
cens_prob_by_i <- NULL
if (length(idx_cens) > 0) {
  cens_rep_mat <- as.matrix(fit$draws("cens_rep", format = "matrix"))
  cens_rep_mat <- cens_rep_mat[keep, , drop = FALSE]
  cens_prob_by_i <- colMeans(cens_rep_mat[, idx_cens, drop = FALSE] == 1)
}

#-----------------------------#
# 6) Prior predictive checks  #
#-----------------------------#
prior_time_rep_log <- log(prior_time_rep)
prior_stats <- summ_quant(as.vector(prior_time_rep_log))

#-----------------------------#
# 7) Resultados (impressão)   #
#-----------------------------#
cat("\n",
    "==================== RELATÓRIO — KIDNEY (cmdstanr + Stan) ====================\n",
    sep = ""
)

cat("\n[Ambiente]\n")
cat("CmdStan path: ", cmd_path, "\n", sep = "")
cat("CmdStan ver.: ", cmd_ver,  "\n", sep = "")

cat("\n[Dados]\n")
cat("N (linhas): ", N, "\n", sep = "")
cat("J (pacientes): ", J, "\n", sep = "")
cat("Censurados (direita): ", sum(kidney$cens_right == 1L),
    " (", round(mean(kidney$cens_right == 1L) * 100, 1), "%)\n", sep = "")
cat("Tempo (observado) — resumo (não-censurados):\n")
print(summ_quant(kidney$time[idx_obs]))

cat("\n[Prior predictive — sanity check da escala]\n")
cat("Resumo do log(time_rep) gerado do PRIOR (global):\n")
print(prior_stats)
cat("Leitura rápida: se isso estiver absurdamente fora da escala do problema,\n",
    "ajuste priors de beta/sigma/tau antes de confiar no posterior.\n", sep = "")

cat("\n[Amostragem HMC/NUTS — qualidade]\n")
cat("Divergências (total): ", num_div, "\n", sep = "")
cat("Max treedepth hits (total): ", num_td, "\n", sep = "")
cat("Max R-hat (core params): ", round(max_rhat, 4), "\n", sep = "")
cat("Min ESS bulk (core params): ", round(min_essb, 1), "\n", sep = "")
cat("Min ESS tail (core params): ", round(min_esst, 1), "\n", sep = "")

if (nrow(bad_rhat) > 0) {
  cat("\nParâmetros com R-hat > 1.01 (core params):\n")
  print(bad_rhat[, c("variable", "rhat", "ess_bulk", "ess_tail"), drop = FALSE])
} else {
  cat("R-hat OK (<= 1.01) para core params.\n")
}

cat("\n[Diagnose do CmdStan (inclui E-BFMI)]\n")
cat(paste(diagnose_txt, collapse = "\n"), "\n")

cat("\n[Sumário posterior — parâmetros principais]\n")
sum_main <- fit$summary(c("beta", "sigma", "tau", "Omega"))
print(sum_main[, c("variable", "mean", "sd", "q5", "median", "q95", "rhat", "ess_bulk", "ess_tail")])

cat("\n[PPC numérico (sem LOO/WAIC)]\n")
cat("Estatísticas observadas (log-time, não-censurados):\n")
print(obs_stats)

cat("\nP-valores PPC (P(T_rep >= T_obs)) — regra prática: extremos (~0 ou ~1) sinalizam misfit:\n")
print(ppc_pvals)

if (!is.null(cens_prob_by_i)) {
  cat("\nCensurados: prob(evento > tempo_censura) por observação censurada (resumo):\n")
  print(summ_quant(cens_prob_by_i))
  cat("Interpretação: como a obs é censurada, esperamos essas probabilidades altas.\n")
}

cat("\n[Notas práticas]\n")
cat("- Se tiver divergências: suba adapt_delta (0.99) e/ou reescale covariáveis.\n")
cat("- Se estourar treedepth: aumente max_treedepth (13–15) e reveja priors.\n")
cat("- Se E-BFMI baixo: reparametrize (não-centrado já está), padronize e revise priors.\n")

cat("\n============================== FIM DO RELATÓRIO ==============================\n")

#-----------------------------#
# 8) Limpeza (sem saídas)     #
#-----------------------------#
if (KEEP_TMP) {
  cat("\n[KEEP_TMP] Diretório temporário mantido para inspeção:\n", tmp_root, "\n", sep = "")
} else {
  clean_ok <- safe_unlink(tmp_root)
  if (!clean_ok) {
    cat("\n[AVISO] Não consegui apagar totalmente o diretório temporário:\n", tmp_root, "\n",
        "Isso pode acontecer no Windows por lock em executável.\n", sep = "")
  }
}
