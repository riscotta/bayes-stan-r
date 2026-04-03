#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

############################################################
# PMS / Serviços em janeiro de 2026
# Reproduz a manchete a partir da base analítica já preparada.
#
# Padrão do repositório:
# - rodar a partir do root do repositório
# - sem setwd()
# - entrada padrão em data/raw/pms_servicos/
# - relatório final no console
# - sem gravação de saídas analíticas em disco
#
# Como rodar:
#   Rscript scripts/pms_servicos/pms_servicos_rstan.R
#
# Exemplos:
#   Rscript scripts/pms_servicos/pms_servicos_rstan.R
#   Rscript scripts/pms_servicos/pms_servicos_rstan.R --refresh=50
#   Rscript scripts/pms_servicos/pms_servicos_rstan.R \
#     --input_dir=data/raw/pms_servicos \
#     --input_csv=pms_base_analitica_stan.csv
############################################################

args <- commandArgs(trailingOnly = TRUE)
AUTO_INSTALL_PKGS <- "--auto-install" %in% args

parse_args <- function(args) {
  out <- list(
    input_dir = file.path("data", "raw", "pms_servicos"),
    input_csv = "pms_base_analitica_stan.csv",
    seed = 20260403L,
    iter_warmup = 1500L,
    iter_sampling = 1500L,
    chains = 4L,
    adapt_delta = 0.995,
    max_treedepth = 13L,
    cores = max(1L, parallel::detectCores(logical = TRUE) - 1L),
    refresh = 100L,
    tol_recorde_pct = 0.05
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

  out$seed <- as.integer(out$seed)
  out$iter_warmup <- as.integer(out$iter_warmup)
  out$iter_sampling <- as.integer(out$iter_sampling)
  out$chains <- as.integer(out$chains)
  out$adapt_delta <- as.numeric(out$adapt_delta)
  out$max_treedepth <- as.integer(out$max_treedepth)
  out$cores <- as.integer(out$cores)
  out$refresh <- as.integer(out$refresh)
  out$tol_recorde_pct <- as.numeric(out$tol_recorde_pct)

  if (!is.finite(out$iter_warmup) || out$iter_warmup <= 0) {
    stop("'iter_warmup' deve ser inteiro positivo.", call. = FALSE)
  }
  if (!is.finite(out$iter_sampling) || out$iter_sampling <= 0) {
    stop("'iter_sampling' deve ser inteiro positivo.", call. = FALSE)
  }
  if (!is.finite(out$chains) || out$chains <= 0) {
    stop("'chains' deve ser inteiro positivo.", call. = FALSE)
  }
  if (!is.finite(out$cores) || out$cores <= 0) {
    stop("'cores' deve ser inteiro positivo.", call. = FALSE)
  }
  if (!is.finite(out$refresh) || out$refresh < 0) {
    stop("'refresh' deve ser inteiro >= 0.", call. = FALSE)
  }
  if (!is.finite(out$tol_recorde_pct) || out$tol_recorde_pct < 0) {
    stop("'tol_recorde_pct' deve ser numérico >= 0.", call. = FALSE)
  }

  out
}

ensure_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (!AUTO_INSTALL_PKGS) {
      stop(
        "Pacote '", pkg, "' não está instalado.\n\n",
        "Como resolver (recomendado):\n",
        "  Rscript scripts/_setup/install_deps.R --all\n\n",
        "Alternativa (manual, dentro do R):\n",
        "  install.packages('", pkg, "')\n\n",
        "Se você quiser auto-instalar neste script:\n",
        "  Rscript scripts/pms_servicos/pms_servicos_rstan.R --auto-install\n",
        call. = FALSE
      )
    }
    message("Instalando pacote ausente: ", pkg)
    install.packages(pkg, dependencies = TRUE)
  }
  invisible(TRUE)
}

pkgs <- c("data.table", "rstan")
invisible(lapply(pkgs, ensure_pkg))

suppressPackageStartupMessages({
  library(data.table)
  library(rstan)
})

opts <- parse_args(args)

options(mc.cores = opts$cores)
rstan_options(auto_write = FALSE)

# -----------------------------
# Funções auxiliares
# -----------------------------
fail <- function(...) stop(sprintf(...), call. = FALSE)

sec <- function(titulo) {
  cat("\n", strrep("=", 88), "\n", titulo, "\n", strrep("=", 88), "\n", sep = "")
}

fmt_num <- function(x, digits = 3) {
  formatC(x, digits = digits, format = "f", decimal.mark = ",")
}

fmt_pct <- function(x, digits = 2) {
  paste0(fmt_num(x, digits = digits), "%")
}

qvec <- function(x, probs = c(0.05, 0.50, 0.95)) {
  as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE))
}

bfmi_chain <- function(energy) {
  if (length(energy) < 3L || stats::var(energy) == 0) return(NA_real_)
  mean(diff(energy)^2) / stats::var(energy)
}

safe_sd <- function(x, fallback = 1e-3) {
  out <- stats::sd(x)
  if (!is.finite(out) || out <= 0) fallback else out
}

# -----------------------------
# Configuração efetiva
# -----------------------------
ARQUIVO_CSV <- file.path(opts$input_dir, opts$input_csv)
SEED <- opts$seed
ITER_WARMUP <- opts$iter_warmup
ITER_SAMPLING <- opts$iter_sampling
CHAINS <- opts$chains
ADAPT_DELTA <- opts$adapt_delta
MAX_TREEDEPTH <- opts$max_treedepth
TOL_RECORDE_PCT <- opts$tol_recorde_pct

cat("\n============================================================\n")
cat("PMS / Serviços em janeiro de 2026 (rstan)\n")
cat("============================================================\n")
cat("Input dir      :", opts$input_dir, "\n")
cat("Input csv      :", opts$input_csv, "\n")
cat("Arquivo alvo   :", ARQUIVO_CSV, "\n")
cat("Seed           :", SEED, "\n")
cat("Warmup         :", ITER_WARMUP, "\n")
cat("Sampling       :", ITER_SAMPLING, "\n")
cat("Chains         :", CHAINS, "\n")
cat("Adapt delta    :", ADAPT_DELTA, "\n")
cat("Max treedepth  :", MAX_TREEDEPTH, "\n")
cat("Cores          :", opts$cores, "\n")
cat("Refresh        :", opts$refresh, "\n")
cat("Tol. recorde % :", TOL_RECORDE_PCT, "\n")
cat("============================================================\n")

# -----------------------------
# Leitura e validação estrutural
# -----------------------------
sec("1) LEITURA E VALIDAÇÃO DA BASE")

if (!file.exists(ARQUIVO_CSV)) {
  fail("Arquivo não encontrado: %s", ARQUIVO_CSV)
}

base <- fread(
  input = ARQUIVO_CSV,
  sep = ";",
  encoding = "UTF-8",
  na.strings = c("", "NA", "NaN", "NULL")
)

colunas_obrigatorias <- c(
  "serie_id", "serie_chave", "t", "n_obs_serie", "fonte_tabela",
  "abertura_familia", "nivel_territorial", "codigo_territorio", "territorio",
  "categoria_nivel", "categoria_codigo", "categoria", "periodo_yyyymm",
  "ano", "mes", "data_ref", "tipo_indice", "tipo_indice_grupo",
  "indice", "indice_sa", "variacao_mm1_sa", "variacao_mm12",
  "variacao_acum_ano", "variacao_acum_12m", "n_metricas_preenchidas",
  "flag_tem_indice_sa_e_mm1"
)

faltantes <- setdiff(colunas_obrigatorias, names(base))
if (length(faltantes) > 0L) {
  fail("Base inválida: faltam colunas obrigatórias: %s", paste(faltantes, collapse = ", "))
}

cols_num <- c(
  "serie_id", "t", "n_obs_serie", "codigo_territorio", "periodo_yyyymm",
  "ano", "mes", "indice", "indice_sa", "variacao_mm1_sa",
  "variacao_mm12", "variacao_acum_ano", "variacao_acum_12m",
  "n_metricas_preenchidas", "flag_tem_indice_sa_e_mm1"
)

for (nm in intersect(cols_num, names(base))) {
  suppressWarnings(set(base, j = nm, value = as.numeric(base[[nm]])))
}

base[, data_ref := as.Date(data_ref)]
if (anyNA(base$data_ref)) fail("Há datas inválidas em data_ref.")

cat("Linhas na base:", nrow(base), "\n")
cat("Séries únicas:", uniqueN(base$serie_id), "\n")
cat("Período:", min(base$data_ref), "até", max(base$data_ref), "\n")

# -----------------------------
# Seleção da série-alvo
# -----------------------------
sec("2) SELEÇÃO DA SÉRIE-ALVO")

pms_total <- base[
  nivel_territorial == "Brasil" &
    categoria_nivel == "total" &
    categoria_codigo == "TOTAL" &
    fonte_tabela == "pms_5906_brasil" &
    flag_tem_indice_sa_e_mm1 == 1
][order(data_ref)]

if (nrow(pms_total) == 0L) {
  fail("Não encontrei a série Brasil/Total da PMS com índice sazonalmente ajustado.")
}

if (uniqueN(pms_total$serie_id) != 1L) {
  fail("Esperava exatamente 1 série para Brasil/Total; encontrei %d.", uniqueN(pms_total$serie_id))
}

if (anyDuplicated(pms_total$data_ref)) {
  fail("A série Brasil/Total possui datas duplicadas.")
}

sequencia_esperada <- seq(from = min(pms_total$data_ref), to = max(pms_total$data_ref), by = "month")
if (!identical(as.Date(pms_total$data_ref), as.Date(sequencia_esperada))) {
  fail("A série Brasil/Total não está mensalmente contígua. Verifique lacunas no calendário.")
}

if (anyNA(pms_total$indice_sa) || anyNA(pms_total$variacao_mm1_sa)) {
  fail("A série Brasil/Total possui NA em indice_sa ou variacao_mm1_sa.")
}

if (any(!is.finite(pms_total$indice_sa)) || any(pms_total$indice_sa <= 0)) {
  fail("A série Brasil/Total possui indice_sa não positivo ou não finito, inviabilizando o log.")
}

cat("Série selecionada:", unique(pms_total$serie_chave), "\n")
cat("Observações na série:", nrow(pms_total), "\n")
cat("Início:", min(pms_total$data_ref), "| Fim:", max(pms_total$data_ref), "\n")

# -----------------------------
# Checagens aritméticas da manchete
# -----------------------------
sec("3) VERIFICAÇÃO ARITMÉTICA DA MANCHETE")

ref_jan <- as.Date("2026-01-01")
ref_dec <- as.Date("2025-12-01")
ref_fev20 <- as.Date("2020-02-01")

datas_obrigatorias <- as.Date(c("2026-01-01", "2025-12-01", "2020-02-01"))
faltam_datas <- setdiff(datas_obrigatorias, unique(as.Date(pms_total$data_ref)))
if (length(faltam_datas) > 0L) {
  fail(
    "Data obrigatória ausente na série: %s",
    paste(format(as.Date(faltam_datas), "%Y-%m-%d"), collapse = ", ")
  )
}

idx_jan <- which(pms_total$data_ref == ref_jan)
idx_dec <- which(pms_total$data_ref == ref_dec)
idx_fev20 <- which(pms_total$data_ref == ref_fev20)

if (idx_jan <= 1L) fail("Jan/2026 não pode ser o primeiro ponto da série.")
if (idx_dec != (idx_jan - 1L)) fail("Dez/2025 não está imediatamente antes de Jan/2026.")

obs_jan_mm1 <- pms_total[data_ref == ref_jan, variacao_mm1_sa]
obs_jan_idx <- pms_total[data_ref == ref_jan, indice_sa]
obs_dec_idx <- pms_total[data_ref == ref_dec, indice_sa]
obs_fev20_idx <- pms_total[data_ref == ref_fev20, indice_sa]

pre_jan <- pms_total[data_ref < ref_jan]
obs_prev_max_idx <- pre_jan[, max(indice_sa)]
obs_prev_max_date <- pre_jan[which.max(indice_sa), data_ref]
obs_gap_recorde_pct <- 100 * (obs_jan_idx / obs_prev_max_idx - 1)
obs_acima_fev20_pct <- 100 * (obs_jan_idx / obs_fev20_idx - 1)
obs_mom_via_indice <- 100 * (obs_jan_idx / obs_dec_idx - 1)

nat_jan <- base[
  nivel_territorial == "Brasil" &
    data_ref == ref_jan &
    flag_tem_indice_sa_e_mm1 == 1
]

breadth_n <- nrow(nat_jan)
breadth_pos <- nat_jan[variacao_mm1_sa > 0, .N]
breadth_zero <- nat_jan[variacao_mm1_sa == 0, .N]
breadth_neg <- nat_jan[variacao_mm1_sa < 0, .N]

cat("Jan/2026 - variação m/m SA (coluna oficial):", fmt_pct(obs_jan_mm1, 1), "\n")
cat("Jan/2026 - variação m/m SA (recalculada pelo índice):", fmt_pct(obs_mom_via_indice, 3), "\n")
cat("Jan/2026 - índice SA:", fmt_num(obs_jan_idx, 5), "\n")
cat("Pico anterior exato do índice SA:", fmt_num(obs_prev_max_idx, 5), "em", as.character(obs_prev_max_date), "\n")
cat("Diferença de Jan/2026 para o pico anterior:", fmt_pct(obs_gap_recorde_pct, 4), "\n")
cat("Jan/2026 acima de Fev/2020:", fmt_pct(obs_acima_fev20_pct, 3), "\n")
cat("Difusão nacional em Jan/2026 (séries Brasil com SA disponível):\n")
cat("  Positivas:", breadth_pos, "| Estáveis:", breadth_zero, "| Negativas:", breadth_neg, "| Total:", breadth_n, "\n")

# -----------------------------
# Preparação para Stan
# -----------------------------
sec("4) PREPARAÇÃO PARA O MODELO BAYESIANO")

# Estratégia desta versão:
# - trabalhar no log do índice sazonalmente ajustado
# - padronizar para melhorar a geometria numérica
# - modelar o nível diretamente, ancorado nas observações
# - fazer a taxa latente mensal seguir AR(1), com reversão moderada
#
# Isso reduz a superdispersão do nível reconstruído por acumulação livre,
# mas preserva a melhoria do sampler trazida pela parametrização não centrada.

y_log <- log(pms_total$indice_sa)
y_mean <- mean(y_log)
y_sd <- safe_sd(y_log, fallback = 1e-4)
y_std <- (y_log - y_mean) / y_sd

T_obs <- length(y_std)
if (T_obs < 3L) fail("Série curta demais para o modelo proposto.")

dy_obs <- diff(y_std)
dy_scale <- safe_sd(dy_obs, fallback = 0.05)
dy_mean <- mean(dy_obs)

prior_level1_sd <- max(0.05, 0.35 * dy_scale)
prior_g1_sd <- max(0.03, 0.60 * dy_scale)
prior_mu_g_sd <- max(0.015, 0.35 * dy_scale)
prior_sigma_obs <- max(0.02, 0.45 * dy_scale)
prior_sigma_g <- max(0.003, 0.12 * dy_scale)

cat("Transformação aplicada: log(indice_sa) e padronização z-score.\n")
cat("Observações no nível:", T_obs, "| Diferenças mensais:", length(dy_obs), "\n")
cat("Escala empírica das diferenças padronizadas:", fmt_num(dy_scale, 4), "\n")
cat("Média empírica das diferenças padronizadas:", fmt_num(dy_mean, 4), "\n")
cat("Priors de estabilidade:\n")
cat("  sd prior level1:", fmt_num(prior_level1_sd, 4), "\n")
cat("  sd prior g1:", fmt_num(prior_g1_sd, 4), "\n")
cat("  sd prior mu_g:", fmt_num(prior_mu_g_sd, 4), "\n")
cat("  sd prior sigma_obs:", fmt_num(prior_sigma_obs, 4), "\n")
cat("  sd prior sigma_g:", fmt_num(prior_sigma_g, 4), "\n")

stan_data <- list(
  T = T_obs,
  y = as.vector(y_std),
  y_mean = as.numeric(y_mean),
  y_sd = as.numeric(y_sd),
  dy0 = as.numeric(dy_obs[1]),
  dy_mean = as.numeric(dy_mean),
  prior_level1_sd = as.numeric(prior_level1_sd),
  prior_g1_sd = as.numeric(prior_g1_sd),
  prior_mu_g_sd = as.numeric(prior_mu_g_sd),
  prior_sigma_obs = as.numeric(prior_sigma_obs),
  prior_sigma_g = as.numeric(prior_sigma_g)
)

stan_code <- "
data {
  int<lower=3> T;
  vector[T] y;
  real y_mean;
  real y_sd;
  real dy0;
  real dy_mean;
  real<lower=1e-8> prior_level1_sd;
  real<lower=1e-8> prior_g1_sd;
  real<lower=1e-8> prior_mu_g_sd;
  real<lower=1e-8> prior_sigma_obs;
  real<lower=1e-8> prior_sigma_g;
}
parameters {
  vector[T-1] z_g;
  real level1;
  real g1;
  real mu_g;
  real<lower=0, upper=0.98> phi;
  real<lower=0> sigma_obs;
  real<lower=0> sigma_g;
}
transformed parameters {
  vector[T] g;
  vector[T] level_std;

  g[1] = g1;
  level_std[1] = level1;

  for (t in 2:T) {
    g[t] = mu_g + phi * (g[t-1] - mu_g) + sigma_g * z_g[t-1];
    level_std[t] = level_std[t-1] + g[t-1];
  }
}
model {
  z_g ~ std_normal();

  level1 ~ normal(y[1], prior_level1_sd);
  g1 ~ normal(dy0, prior_g1_sd);
  mu_g ~ normal(dy_mean, prior_mu_g_sd);
  phi ~ beta(3, 2);
  sigma_obs ~ normal(0, prior_sigma_obs);
  sigma_g ~ normal(0, prior_sigma_g);

  y ~ normal(level_std, sigma_obs);
}
generated quantities {
  vector[T] y_rep_std;
  vector[T] level_log;
  vector[T] level_idx;
  vector[T] y_rep_idx;

  for (t in 1:T) {
    y_rep_std[t] = normal_rng(level_std[t], sigma_obs);
    level_log[t] = y_mean + y_sd * level_std[t];
    level_idx[t] = exp(level_log[t]);
    y_rep_idx[t] = exp(y_mean + y_sd * y_rep_std[t]);
  }
}
"

# -----------------------------
# Ajuste do modelo
# -----------------------------
sec("5) AJUSTE DO MODELO")

mod <- rstan::stan_model(
  model_code = stan_code,
  model_name = "pms_servicos_nivel_ancorado_ar1"
)

init_fun <- function() {
  list(
    level1 = as.numeric(y_std[1]),
    g1 = as.numeric(dy_obs[1]),
    mu_g = as.numeric(dy_mean),
    phi = 0.65,
    sigma_obs = max(0.5 * prior_sigma_obs, 0.015),
    sigma_g = max(0.5 * prior_sigma_g, 0.003),
    z_g = rep(0, T_obs - 1L)
  )
}

fit <- rstan::sampling(
  object = mod,
  data = stan_data,
  seed = SEED,
  chains = CHAINS,
  iter = ITER_WARMUP + ITER_SAMPLING,
  warmup = ITER_WARMUP,
  init = init_fun,
  refresh = opts$refresh,
  control = list(
    adapt_delta = ADAPT_DELTA,
    max_treedepth = MAX_TREEDEPTH
  )
)

# -----------------------------
# Extração de draws e métricas substantivas
# -----------------------------
sec("6) EXTRAÇÃO DE RESULTADOS")

post <- rstan::extract(
  fit,
  pars = c("sigma_obs", "sigma_g", "mu_g", "phi", "g", "level_idx", "y_rep_idx"),
  permuted = TRUE
)

level_idx_draws <- post$level_idx
yrep_idx_draws <- post$y_rep_idx
g_draws <- post$g

if (!is.matrix(level_idx_draws) || ncol(level_idx_draws) != T_obs) {
  fail("Extração de level_idx falhou ou retornou dimensão inesperada.")
}

if (!is.matrix(g_draws) || ncol(g_draws) != T_obs) {
  fail("Extração de g falhou ou retornou dimensão inesperada.")
}

jan_draw <- level_idx_draws[, idx_jan]
dec_draw <- level_idx_draws[, idx_dec]
fev20_draw <- level_idx_draws[, idx_fev20]
prevmax_draw <- apply(level_idx_draws[, 1:(idx_jan - 1), drop = FALSE], 1, max)

mom_draw <- 100 * (jan_draw / dec_draw - 1)
acima_fev20_draw <- 100 * (jan_draw / fev20_draw - 1)
gap_recorde_draw <- 100 * (jan_draw / prevmax_draw - 1)
latent_growth_jan_draw <- 100 * (exp(y_sd * g_draws[, idx_dec]) - 1)

# Probabilidades posteriores de interesse
p_mom_pos <- mean(mom_draw > 0)
p_mom_ge_03 <- mean(mom_draw >= 0.30)
p_recorde_estrito <- mean(jan_draw >= prevmax_draw)
p_patamar_recorde <- mean(gap_recorde_draw >= -TOL_RECORDE_PCT)
p_acima_20_1 <- mean(acima_fev20_draw >= 20.1)

# Posterior predictive checks
ppc_mean <- colMeans(yrep_idx_draws)
ppc_q05 <- apply(yrep_idx_draws, 2, stats::quantile, probs = 0.05)
ppc_q95 <- apply(yrep_idx_draws, 2, stats::quantile, probs = 0.95)

ppc_rmse <- sqrt(mean((pms_total$indice_sa - ppc_mean)^2))
ppc_mae <- mean(abs(pms_total$indice_sa - ppc_mean))
ppc_cov90 <- mean(pms_total$indice_sa >= ppc_q05 & pms_total$indice_sa <= ppc_q95)

# -----------------------------
# Diagnósticos do sampler
# -----------------------------
sec("7) DIAGNÓSTICOS")

sampler_params <- rstan::get_sampler_params(fit, inc_warmup = FALSE)

divergencias <- sum(vapply(sampler_params, function(x) sum(x[, "divergent__"]), numeric(1)))
max_treedepth_hits <- sum(vapply(sampler_params, function(x) sum(x[, "treedepth__"] >= MAX_TREEDEPTH), numeric(1)))
bfmi_vals <- vapply(sampler_params, function(x) bfmi_chain(x[, "energy__"]), numeric(1))

sum_core <- summary(fit, pars = c("sigma_obs", "sigma_g", "mu_g", "phi", "g1"))$summary
sum_all <- summary(fit)$summary

max_rhat <- max(sum_all[, "Rhat"], na.rm = TRUE)
min_neff <- min(sum_all[, "n_eff"], na.rm = TRUE)

diagnostico_ok <- (
  divergencias == 0L &&
    max_treedepth_hits == 0L &&
    is.finite(max_rhat) && max_rhat < 1.01 &&
    is.finite(min_neff) && min_neff >= 400 &&
    all(is.na(bfmi_vals) | bfmi_vals > 0.30)
)

# -----------------------------
# Relatório final
# -----------------------------
sec("RELATÓRIO FINAL — PMS / SERVIÇOS / JANEIRO 2026")

cat("Hipótese operacional da manchete:\n")
cat("  (i) a variação m/m sazonalmente ajustada de jan/2026 foi +0,3%;\n")
cat("  (ii) o nível da série ficou no mesmo patamar do recorde histórico;\n")
cat("  (iii) o nível ficou ~20,1% acima de fev/2020.\n\n")

cat("Base analítica usada:\n")
cat("  Arquivo:", ARQUIVO_CSV, "\n")
cat("  Linhas totais:", nrow(base), "\n")
cat("  Série da manchete:", unique(pms_total$serie_chave), "\n")
cat("  Janela temporal da série:", as.character(min(pms_total$data_ref)), "a", as.character(max(pms_total$data_ref)), "\n\n")

cat("Resultado aritmético direto dos dados:\n")
cat("  Variação m/m SA oficial em jan/2026:", fmt_pct(obs_jan_mm1, 1), "\n")
cat("  Variação m/m SA recalculada pelo índice:", fmt_pct(obs_mom_via_indice, 3), "\n")
cat("  Índice SA em jan/2026:", fmt_num(obs_jan_idx, 5), "\n")
cat("  Pico anterior exato do índice SA:", fmt_num(obs_prev_max_idx, 5), "em", as.character(obs_prev_max_date), "\n")
cat("  Distância exata para o pico anterior:", fmt_pct(obs_gap_recorde_pct, 4), "\n")
cat("  Distância para fev/2020:", fmt_pct(obs_acima_fev20_pct, 3), "\n\n")

cat("Resumo substantivo do modelo Bayesiano (nível ancorado + tendência AR(1)):\n")
q_mom <- qvec(mom_draw)
q_gap <- qvec(gap_recorde_draw)
q_fev20 <- qvec(acima_fev20_draw)
q_jan_idx <- qvec(jan_draw)
q_growth <- qvec(latent_growth_jan_draw)

cat("  Nível latente do índice SA em jan/2026 [5%, 50%, 95%]: ",
    fmt_num(q_jan_idx[1], 3), " | ", fmt_num(q_jan_idx[2], 3), " | ", fmt_num(q_jan_idx[3], 3), "\n", sep = "")
cat("  Variação latente m/m em jan/2026 [5%, 50%, 95%]: ",
    fmt_pct(q_mom[1], 2), " | ", fmt_pct(q_mom[2], 2), " | ", fmt_pct(q_mom[3], 2), "\n", sep = "")
cat("  Taxa latente mensal do crescimento subjacente [5%, 50%, 95%]: ",
    fmt_pct(q_growth[1], 2), " | ", fmt_pct(q_growth[2], 2), " | ", fmt_pct(q_growth[3], 2), "\n", sep = "")
cat("  Gap latente vs pico anterior [5%, 50%, 95%]: ",
    fmt_pct(q_gap[1], 3), " | ", fmt_pct(q_gap[2], 3), " | ", fmt_pct(q_gap[3], 3), "\n", sep = "")
cat("  Distância latente vs fev/2020 [5%, 50%, 95%]: ",
    fmt_pct(q_fev20[1], 2), " | ", fmt_pct(q_fev20[2], 2), " | ", fmt_pct(q_fev20[3], 2), "\n", sep = "")
cat("  P(variação m/m > 0):", fmt_pct(100 * p_mom_pos, 1), "\n")
cat("  P(variação m/m >= 0,3%):", fmt_pct(100 * p_mom_ge_03, 1), "\n")
cat("  P(novo recorde estrito):", fmt_pct(100 * p_recorde_estrito, 1), "\n")
cat("  P(estar no mesmo patamar do recorde; tolerância = ", fmt_pct(TOL_RECORDE_PCT, 2), "):", fmt_pct(100 * p_patamar_recorde, 1), "\n", sep = "")
cat("  P(estar >= 20,1% acima de fev/2020):", fmt_pct(100 * p_acima_20_1, 1), "\n\n")

cat("Posterior predictive checks:\n")
cat("  RMSE (índice SA):", fmt_num(ppc_rmse, 4), "\n")
cat("  MAE  (índice SA):", fmt_num(ppc_mae, 4), "\n")
cat("  Cobertura do IC preditivo de 90%:", fmt_pct(100 * ppc_cov90, 1), "\n\n")

cat("Diagnósticos do sampler:\n")
cat("  Divergências:", divergencias, "\n")
cat("  Saturações de tree depth:", max_treedepth_hits, "\n")
cat("  Max R-hat:", fmt_num(max_rhat, 4), "\n")
cat("  Min n_eff:", fmt_num(min_neff, 0), "\n")
cat("  E-BFMI por chain:", paste(fmt_num(bfmi_vals, 3), collapse = " | "), "\n\n")

cat("Parâmetros principais do modelo:\n")
print(round(sum_core[, c("mean", "sd", "2.5%", "50%", "97.5%", "n_eff", "Rhat")], 4))
cat("\n")

cat("Conclusão prática:\n")
if (diagnostico_ok) {
  cat("  Os diagnósticos indicam ajuste confiável para inferência.\n")
} else {
  cat("  Atenção: ainda há sinal de diagnóstico imperfeito. Se persistir, estreite mais o prior de sigma_g ou aumente warmup/iterações.\n")
}

if (obs_jan_mm1 >= 0.3 && obs_gap_recorde_pct > -0.01) {
  cat("  A manchete fica bem sustentada pelos dados observados: houve alta m/m de 0,3% em jan/2026 e o nível ajustado ficou virtualmente empatado com o máximo anterior.\n")
} else {
  cat("  A manchete não fica integralmente sustentada na leitura literal dos dados; a parte do patamar recorde deve ser interpretada com cautela.\n")
}

cat("  Nesta versão, o nível latente é ancorado diretamente nas observações e a taxa de crescimento mensal tem persistência controlada por AR(1).\n")
cat("  Não foi usado LOO/WAIC porque o objetivo aqui é verificação de uma única especificação, não comparação entre modelos.\n")

invisible(list(
  dados = pms_total,
  fit = fit,
  resumo_parametros = sum_core,
  metricas = list(
    obs_jan_mm1 = obs_jan_mm1,
    obs_gap_recorde_pct = obs_gap_recorde_pct,
    obs_acima_fev20_pct = obs_acima_fev20_pct,
    p_mom_pos = p_mom_pos,
    p_mom_ge_03 = p_mom_ge_03,
    p_recorde_estrito = p_recorde_estrito,
    p_patamar_recorde = p_patamar_recorde,
    p_acima_20_1 = p_acima_20_1,
    ppc_rmse = ppc_rmse,
    ppc_mae = ppc_mae,
    ppc_cov90 = ppc_cov90,
    divergencias = divergencias,
    max_treedepth_hits = max_treedepth_hits,
    max_rhat = max_rhat,
    min_neff = min_neff,
    bfmi_vals = bfmi_vals
  )
))
