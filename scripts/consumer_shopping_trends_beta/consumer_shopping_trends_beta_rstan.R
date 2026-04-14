#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

############################################################
# Consumer Shopping Trends — modelo Beta Bayesiano
# Variável-alvo padrão: avg_online_spend
#
# Padrão do repositório:
# - rodar a partir do root do repositório
# - sem setwd()
# - entrada padrão em data/raw/consumer_shopping_trends/
# - saídas em outputs/{figures,tables,models}/consumer_shopping_trends_beta/
# - o modelo Stan é compilado em memória; nenhum arquivo .stan é gravado em disco
#
# Como rodar:
#   Rscript scripts/consumer_shopping_trends_beta/consumer_shopping_trends_beta_rstan.R
#
# Exemplos:
#   Rscript scripts/consumer_shopping_trends_beta/consumer_shopping_trends_beta_rstan.R
#   Rscript scripts/consumer_shopping_trends_beta/consumer_shopping_trends_beta_rstan.R \
#     --target_var=avg_store_spend
#   Rscript scripts/consumer_shopping_trends_beta/consumer_shopping_trends_beta_rstan.R \
#     --input_csv=data/raw/consumer_shopping_trends/Consumer_Shopping_Trends_2026\ \(6\).csv \
#     --iter=1000 --warmup=500 --chains=2
############################################################

args <- commandArgs(trailingOnly = TRUE)
AUTO_INSTALL_PKGS <- "--auto-install" %in% args

parse_args <- function(args) {
  out <- list(
    input_dir = file.path("data", "raw", "consumer_shopping_trends"),
    input_csv = NA_character_,
    preferred_csv = "Consumer_Shopping_Trends_2026 (6).csv",
    target_var = "avg_online_spend",
    lower_bound = 0,
    upper_bound = 150000,
    seed = 20260410L,
    chains = 4L,
    iter = 2000L,
    warmup = 1000L,
    adapt_delta = 0.95,
    max_treedepth = 12L,
    cores = max(1L, parallel::detectCores(logical = TRUE) - 1L),
    refresh = 100L,
    n_ppc_stats_draws = 200L,
    n_ppc_density_draws = 40L,
    ppc_density_n = 3000L,
    n_prior_density = 40L,
    save_tables = 1L,
    save_plots = 1L,
    save_report = 1L,
    save_fit_object = 1L
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

  int_fields <- c(
    "seed", "chains", "iter", "warmup", "max_treedepth", "cores", "refresh",
    "n_ppc_stats_draws", "n_ppc_density_draws", "ppc_density_n", "n_prior_density",
    "save_tables", "save_plots", "save_report", "save_fit_object"
  )
  num_fields <- c("lower_bound", "upper_bound", "adapt_delta")

  for (nm in int_fields) out[[nm]] <- as.integer(out[[nm]])
  for (nm in num_fields) out[[nm]] <- as.numeric(out[[nm]])

  if (!is.na(out$input_csv) && nzchar(out$input_csv)) {
    out$input_csv <- gsub("\\\\", "/", out$input_csv)
  } else {
    out$input_csv <- NA_character_
  }

  if (!is.finite(out$lower_bound) || !is.finite(out$upper_bound) || out$upper_bound <= out$lower_bound) {
    stop("'lower_bound' e 'upper_bound' devem ser finitos e satisfazer upper_bound > lower_bound.", call. = FALSE)
  }
  if (!is.finite(out$iter) || out$iter <= 0) stop("'iter' deve ser inteiro positivo.", call. = FALSE)
  if (!is.finite(out$warmup) || out$warmup <= 0 || out$warmup >= out$iter) {
    stop("'warmup' deve ser inteiro positivo e estritamente menor que 'iter'.", call. = FALSE)
  }
  if (!is.finite(out$chains) || out$chains <= 0) stop("'chains' deve ser inteiro positivo.", call. = FALSE)
  if (!is.finite(out$cores) || out$cores <= 0) stop("'cores' deve ser inteiro positivo.", call. = FALSE)
  if (!is.finite(out$adapt_delta) || out$adapt_delta <= 0 || out$adapt_delta >= 1) {
    stop("'adapt_delta' deve estar no intervalo aberto (0, 1).", call. = FALSE)
  }
  if (!is.finite(out$max_treedepth) || out$max_treedepth <= 0) stop("'max_treedepth' deve ser inteiro positivo.", call. = FALSE)
  if (!is.finite(out$refresh) || out$refresh < 0) stop("'refresh' deve ser inteiro >= 0.", call. = FALSE)
  if (!out$target_var %in% c("avg_online_spend", "avg_store_spend")) {
    stop("'target_var' deve ser 'avg_online_spend' ou 'avg_store_spend'.", call. = FALSE)
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
        "  Rscript scripts/consumer_shopping_trends_beta/consumer_shopping_trends_beta_rstan.R --auto-install\n",
        call. = FALSE
      )
    }
    message("Instalando pacote ausente: ", pkg)
    install.packages(pkg, dependencies = TRUE)
  }
  invisible(TRUE)
}

pkgs <- c("data.table", "ggplot2", "rstan")
invisible(lapply(pkgs, ensure_pkg))

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
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

fmt_num <- function(x, digits = 4) {
  formatC(x, digits = digits, format = "f", decimal.mark = ",")
}

compute_skewness <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3L) return(NA_real_)
  m <- mean(x)
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) return(0)
  mean(((x - m) / s)^3)
}

resolve_input_csv <- function(input_dir, explicit_input_csv, preferred_csv) {
  if (!is.na(explicit_input_csv) && nzchar(explicit_input_csv)) {
    return(explicit_input_csv)
  }

  candidate <- file.path(input_dir, preferred_csv)
  if (file.exists(candidate)) return(candidate)

  csv_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)

  if (length(csv_files) == 1L) return(csv_files[1])
  if (length(csv_files) == 0L) {
    fail(
      "Nenhum arquivo CSV foi encontrado em %s. Baixe a base do Kaggle e salve em %s.",
      input_dir, candidate
    )
  }

  fail(
    paste0(
      "Mais de um CSV encontrado em ", input_dir, ". Informe --input_csv=...\nArquivos encontrados:\n",
      paste(basename(csv_files), collapse = "\n")
    )
  )
}

scaled_beta_density_on_y <- function(y_grid, alpha, beta, lower_bound, upper_bound) {
  z_grid <- (y_grid - lower_bound) / (upper_bound - lower_bound)
  dens_z <- stats::dbeta(z_grid, shape1 = alpha, shape2 = beta)
  dens_y <- dens_z / (upper_bound - lower_bound)
  dens_y[!is.finite(dens_y)] <- 0
  dens_y
}

save_plot <- function(plot_obj, filepath, width = 10, height = 6, dpi = 300) {
  ggplot2::ggsave(
    filename = filepath,
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi
  )
}

safe_density <- function(x, from, to, n = 512) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) fail("Não há observações suficientes para estimar densidade.")
  stats::density(x, from = from, to = to, n = n)
}

# -----------------------------
# Caminhos e configuração efetiva
# -----------------------------
INPUT_DIR <- opts$input_dir
TABLES_DIR <- file.path("outputs", "tables", "consumer_shopping_trends_beta")
FIGURES_DIR <- file.path("outputs", "figures", "consumer_shopping_trends_beta")
MODELS_DIR <- file.path("outputs", "models", "consumer_shopping_trends_beta")

if (!dir.exists(INPUT_DIR)) {
  fail("Diretório de entrada não encontrado: %s", INPUT_DIR)
}

dir.create(TABLES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MODELS_DIR, recursive = TRUE, showWarnings = FALSE)

INPUT_CSV <- resolve_input_csv(INPUT_DIR, opts$input_csv, opts$preferred_csv)
TIMESTAMP_TAG <- format(Sys.time(), "%Y%m%d_%H%M%S")
LOG_FILE <- file.path(TABLES_DIR, paste0("consumer_shopping_trends_beta_log_", TIMESTAMP_TAG, ".txt"))

log_message <- function(..., append = TRUE) {
  msg <- paste0(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), paste(..., collapse = " "))
  cat(msg, "\n")
  cat(msg, "\n", file = LOG_FILE, append = append)
}

cat("\n============================================================\n")
cat("Consumer Shopping Trends — modelo Beta Bayesiano (rstan)\n")
cat("============================================================\n")
cat("Input dir      :", INPUT_DIR, "\n")
cat("Input csv      :", INPUT_CSV, "\n")
cat("Target var     :", opts$target_var, "\n")
cat("Bounds         : [", opts$lower_bound, ", ", opts$upper_bound, "]\n", sep = "")
cat("Seed           :", opts$seed, "\n")
cat("Iter           :", opts$iter, "\n")
cat("Warmup         :", opts$warmup, "\n")
cat("Chains         :", opts$chains, "\n")
cat("Adapt delta    :", opts$adapt_delta, "\n")
cat("Max treedepth  :", opts$max_treedepth, "\n")
cat("Cores          :", opts$cores, "\n")
cat("Refresh        :", opts$refresh, "\n")
cat("Save tables    :", opts$save_tables, "\n")
cat("Save plots     :", opts$save_plots, "\n")
cat("Save report    :", opts$save_report, "\n")
cat("Save fit object:", opts$save_fit_object, "\n")
cat("============================================================\n")

log_message("Arquivo CSV selecionado:", INPUT_CSV, append = FALSE)
log_message("Diretório de tabelas:", TABLES_DIR)
log_message("Diretório de figuras:", FIGURES_DIR)
log_message("Diretório de modelos:", MODELS_DIR)

# -----------------------------
# Leitura e validação dos dados
# -----------------------------
sec("1) LEITURA E VALIDAÇÃO DA BASE")

df <- data.table::fread(
  input = INPUT_CSV,
  encoding = "UTF-8",
  na.strings = c("", "NA", "NaN", "NULL")
)

cat("Linhas na base:", nrow(df), "\n")
cat("Colunas na base:", ncol(df), "\n")

if (!(opts$target_var %in% names(df))) {
  fail("A variável-alvo '%s' não existe no CSV.", opts$target_var)
}

cols_numeric_relevantes <- c("avg_online_spend", "avg_store_spend")
for (nm in intersect(cols_numeric_relevantes, names(df))) {
  suppressWarnings(data.table::set(df, j = nm, value = as.numeric(df[[nm]])))
}

y_raw <- df[[opts$target_var]]
if (!is.numeric(y_raw)) {
  fail("A variável-alvo '%s' não é numérica após leitura/coerção.", opts$target_var)
}

keep <- is.finite(y_raw)
if (!all(keep)) {
  cat("Linhas removidas por NA/NaN/Inf no alvo:", sum(!keep), "\n")
  log_message("Foram removidas", sum(!keep), "linhas com NA/NaN/Inf no alvo.")
}

df <- df[keep]
y <- df[[opts$target_var]]
N <- length(y)

if (N < 10L) fail("Poucas observações válidas (%d) para ajuste do modelo.", N)
if (any(y < opts$lower_bound | y > opts$upper_bound)) {
  fail(
    "Há valores fora dos limites [%s, %s] na variável '%s'.",
    opts$lower_bound, opts$upper_bound, opts$target_var
  )
}

z_raw <- (y - opts$lower_bound) / (opts$upper_bound - opts$lower_bound)
eps <- 1e-8
z <- pmin(pmax(z_raw, eps), 1 - eps)

cat("Observações válidas:", N, "\n")
cat("Min alvo       :", min(y), "\n")
cat("Max alvo       :", max(y), "\n")
cat("Min escalado   :", min(z), "\n")
cat("Max escalado   :", max(z), "\n")

# -----------------------------
# Resumo observado e frequências
# -----------------------------
sec("2) RESUMO DESCRITIVO")

obs_summary <- data.frame(
  variable = opts$target_var,
  n = N,
  min = min(y),
  q05 = unname(stats::quantile(y, 0.05)),
  q25 = unname(stats::quantile(y, 0.25)),
  median = stats::median(y),
  mean = mean(y),
  q75 = unname(stats::quantile(y, 0.75)),
  q95 = unname(stats::quantile(y, 0.95)),
  max = max(y),
  sd = stats::sd(y),
  skew = compute_skewness(y),
  min_scaled = min(z),
  max_scaled = max(z),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

print(obs_summary)

if (isTRUE(opts$save_tables == 1L)) {
  utils::write.csv(
    obs_summary,
    file = file.path(TABLES_DIR, paste0("resumo_observado_", opts$target_var, "_", TIMESTAMP_TAG, ".csv")),
    row.names = FALSE
  )
}

if ("shopping_preference" %in% names(df) && isTRUE(opts$save_tables == 1L)) {
  tab_pref <- as.data.frame(table(df$shopping_preference), stringsAsFactors = FALSE)
  names(tab_pref) <- c("shopping_preference", "n")
  tab_pref$prop <- tab_pref$n / sum(tab_pref$n)
  utils::write.csv(
    tab_pref,
    file = file.path(TABLES_DIR, paste0("frequencia_shopping_preference_", TIMESTAMP_TAG, ".csv")),
    row.names = FALSE
  )
}

# -----------------------------
# Gráficos exploratórios observados
# -----------------------------
sec("3) GRÁFICOS EXPLORATÓRIOS")

plot_df <- data.frame(value = y)

p_hist <- ggplot(plot_df, aes(x = value)) +
  geom_histogram(bins = 50) +
  labs(
    title = paste("Histograma de", opts$target_var),
    x = opts$target_var,
    y = "Frequência"
  ) +
  theme_minimal(base_size = 12)

p_density <- ggplot(plot_df, aes(x = value)) +
  geom_density(adjust = 1.1) +
  labs(
    title = paste("Densidade observada de", opts$target_var),
    x = opts$target_var,
    y = "Densidade"
  ) +
  theme_minimal(base_size = 12)

if (isTRUE(opts$save_plots == 1L)) {
  save_plot(p_hist, file.path(FIGURES_DIR, paste0("histograma_", opts$target_var, "_", TIMESTAMP_TAG, ".png")))
  save_plot(p_density, file.path(FIGURES_DIR, paste0("densidade_observada_", opts$target_var, "_", TIMESTAMP_TAG, ".png")))

  png(file.path(FIGURES_DIR, paste0("qqplot_normal_", opts$target_var, "_", TIMESTAMP_TAG, ".png")), width = 1200, height = 900, res = 150)
  qqnorm(y, main = paste("QQ-plot Normal -", opts$target_var))
  qqline(y)
  dev.off()
}

# -----------------------------
# Checagem preditiva a priori
# -----------------------------
sec("4) CHECAGEM PREDITIVA A PRIORI")

set.seed(opts$seed)
prior_alpha <- stats::rlnorm(opts$n_prior_density, meanlog = 0, sdlog = 1)
prior_beta <- stats::rlnorm(opts$n_prior_density, meanlog = 0, sdlog = 1)

n_density_sample <- min(opts$ppc_density_n, N)
y_density_sample <- sample(y, size = n_density_sample)
obs_density <- safe_density(y_density_sample, from = opts$lower_bound, to = opts$upper_bound, n = 512)

prior_density_df <- data.frame(
  x = obs_density$x,
  y = obs_density$y,
  draw_id = 0L,
  source = "Observado",
  stringsAsFactors = FALSE
)

for (i in seq_len(opts$n_prior_density)) {
  y_prior <- stats::rbeta(n_density_sample, prior_alpha[i], prior_beta[i]) * (opts$upper_bound - opts$lower_bound) + opts$lower_bound
  d_prior <- safe_density(y_prior, from = opts$lower_bound, to = opts$upper_bound, n = 512)
  prior_density_df <- rbind(
    prior_density_df,
    data.frame(x = d_prior$x, y = d_prior$y, draw_id = i, source = "Preditivo a priori", stringsAsFactors = FALSE)
  )
}

p_prior <- ggplot(prior_density_df, aes(x = x, y = y, group = interaction(source, draw_id), linetype = source)) +
  geom_line(data = subset(prior_density_df, source == "Preditivo a priori"), alpha = 0.25) +
  geom_line(data = subset(prior_density_df, source == "Observado"), linewidth = 1) +
  labs(
    title = paste("Checagem preditiva a priori -", opts$target_var),
    x = opts$target_var,
    y = "Densidade"
  ) +
  theme_minimal(base_size = 12)

if (isTRUE(opts$save_plots == 1L)) {
  save_plot(p_prior, file.path(FIGURES_DIR, paste0("ppc_prior_density_", opts$target_var, "_", TIMESTAMP_TAG, ".png")))
}

# -----------------------------
# Modelo Stan
# -----------------------------
sec("5) AJUSTE DO MODELO BETA EM STAN")

stan_code <- "
data {
  int<lower=1> N;
  vector<lower=0, upper=1>[N] z;
}

parameters {
  real<lower=0> alpha;
  real<lower=0> beta;
}

model {
  alpha ~ lognormal(0, 1);
  beta  ~ lognormal(0, 1);
  z ~ beta(alpha, beta);
}

generated quantities {
  real mean_z;
  real kappa;
  mean_z = alpha / (alpha + beta);
  kappa = alpha + beta;
}
"

stan_data <- list(N = N, z = z)
log_message("Compilando modelo Stan em memória...")
sm <- rstan::stan_model(model_code = stan_code)

log_message("Iniciando amostragem MCMC...")
fit <- rstan::sampling(
  object = sm,
  data = stan_data,
  seed = opts$seed,
  chains = opts$chains,
  iter = opts$iter,
  warmup = opts$warmup,
  refresh = opts$refresh,
  control = list(adapt_delta = opts$adapt_delta, max_treedepth = opts$max_treedepth)
)
log_message("Amostragem concluída.")

# -----------------------------
# Salvamento e resumo posterior
# -----------------------------
sec("6) RESULTADOS POSTERIORES")

posterior_summary <- summary(
  fit,
  pars = c("alpha", "beta", "mean_z", "kappa"),
  probs = c(0.05, 0.25, 0.50, 0.75, 0.95)
)$summary

posterior_summary_df <- data.frame(
  parameter = rownames(posterior_summary),
  posterior_summary,
  row.names = NULL,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

posterior_draws <- as.data.frame(fit, pars = c("alpha", "beta", "mean_z", "kappa"))
posterior_draws$mean_y <- posterior_draws$mean_z * (opts$upper_bound - opts$lower_bound) + opts$lower_bound

mean_y_summary <- data.frame(
  parameter = "mean_y",
  mean = mean(posterior_draws$mean_y),
  sd = stats::sd(posterior_draws$mean_y),
  `5%` = unname(stats::quantile(posterior_draws$mean_y, 0.05)),
  `25%` = unname(stats::quantile(posterior_draws$mean_y, 0.25)),
  `50%` = unname(stats::quantile(posterior_draws$mean_y, 0.50)),
  `75%` = unname(stats::quantile(posterior_draws$mean_y, 0.75)),
  `95%` = unname(stats::quantile(posterior_draws$mean_y, 0.95)),
  n_eff = NA_real_,
  Rhat = NA_real_,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

posterior_summary_combined <- rbind(
  posterior_summary_df[, c("parameter", "mean", "sd", "5%", "25%", "50%", "75%", "95%", "n_eff", "Rhat")],
  mean_y_summary[, c("parameter", "mean", "sd", "5%", "25%", "50%", "75%", "95%", "n_eff", "Rhat")]
)

print(posterior_summary_combined)

if (isTRUE(opts$save_tables == 1L)) {
  utils::write.csv(
    posterior_summary_combined,
    file = file.path(TABLES_DIR, paste0("posterior_summary_", opts$target_var, "_", TIMESTAMP_TAG, ".csv")),
    row.names = FALSE
  )
  utils::write.csv(
    posterior_draws,
    file = file.path(TABLES_DIR, paste0("posterior_draws_", opts$target_var, "_", TIMESTAMP_TAG, ".csv")),
    row.names = FALSE
  )

  capture.output(
    print(fit, pars = c("alpha", "beta", "mean_z", "kappa")),
    file = file.path(TABLES_DIR, paste0("print_fit_", opts$target_var, "_", TIMESTAMP_TAG, ".txt"))
  )
}

if (isTRUE(opts$save_fit_object == 1L)) {
  saveRDS(
    fit,
    file = file.path(MODELS_DIR, paste0("fit_rstan_beta_", opts$target_var, "_", TIMESTAMP_TAG, ".rds"))
  )
}

# -----------------------------
# Diagnósticos MCMC
# -----------------------------
sec("7) DIAGNÓSTICOS MCMC")

sampler_params <- rstan::get_sampler_params(fit, inc_warmup = FALSE)

divergences <- sum(vapply(sampler_params, function(x) sum(x[, "divergent__"]), numeric(1)))
treedepth_hits <- sum(vapply(sampler_params, function(x) sum(x[, "treedepth__"] >= opts$max_treedepth), numeric(1)))
mean_accept_stat <- mean(unlist(lapply(sampler_params, function(x) x[, "accept_stat__"])))
mean_stepsize <- mean(unlist(lapply(sampler_params, function(x) x[, "stepsize__"])))

diagnostics_df <- data.frame(
  metric = c(
    "n_observations",
    "chains",
    "iter",
    "warmup",
    "post_warmup_draws_total",
    "divergences",
    "treedepth_hits",
    "mean_accept_stat",
    "mean_stepsize"
  ),
  value = c(
    N,
    opts$chains,
    opts$iter,
    opts$warmup,
    nrow(posterior_draws),
    divergences,
    treedepth_hits,
    mean_accept_stat,
    mean_stepsize
  ),
  stringsAsFactors = FALSE
)

print(diagnostics_df)

if (isTRUE(opts$save_tables == 1L)) {
  utils::write.csv(
    diagnostics_df,
    file = file.path(TABLES_DIR, paste0("diagnosticos_mcmc_", opts$target_var, "_", TIMESTAMP_TAG, ".csv")),
    row.names = FALSE
  )
}

# -----------------------------
# Distribuições posteriores
# -----------------------------
sec("8) DISTRIBUIÇÕES POSTERIORES")

plot_param_density <- function(draws_df, var_name, title_txt, xlab_txt, filepath) {
  p <- ggplot(draws_df, aes(x = .data[[var_name]])) +
    geom_density(adjust = 1.1) +
    labs(title = title_txt, x = xlab_txt, y = "Densidade") +
    theme_minimal(base_size = 12)
  save_plot(p, filepath)
}

if (isTRUE(opts$save_plots == 1L)) {
  plot_param_density(posterior_draws, "alpha", paste("Posterior de alpha -", opts$target_var), "alpha", file.path(FIGURES_DIR, paste0("posterior_alpha_", opts$target_var, "_", TIMESTAMP_TAG, ".png")))
  plot_param_density(posterior_draws, "beta", paste("Posterior de beta -", opts$target_var), "beta", file.path(FIGURES_DIR, paste0("posterior_beta_", opts$target_var, "_", TIMESTAMP_TAG, ".png")))
  plot_param_density(posterior_draws, "kappa", paste("Posterior de kappa -", opts$target_var), "kappa", file.path(FIGURES_DIR, paste0("posterior_kappa_", opts$target_var, "_", TIMESTAMP_TAG, ".png")))
  plot_param_density(posterior_draws, "mean_y", paste("Posterior da média em escala original -", opts$target_var), opts$target_var, file.path(FIGURES_DIR, paste0("posterior_mean_y_", opts$target_var, "_", TIMESTAMP_TAG, ".png")))
}

# -----------------------------
# Observado vs Beta ajustado vs Normal baseline
# -----------------------------
sec("9) COMPARAÇÃO DE DENSIDADES")

y_grid <- seq(opts$lower_bound + 1, opts$upper_bound - 1, length.out = 1000)
obs_dens_full <- safe_density(y, from = opts$lower_bound, to = opts$upper_bound, n = 1000)

alpha_hat <- mean(posterior_draws$alpha)
beta_hat <- mean(posterior_draws$beta)
mu_hat <- mean(y)
sd_hat <- stats::sd(y)

comparison_df <- rbind(
  data.frame(x = obs_dens_full$x, y = obs_dens_full$y, source = "Observado", stringsAsFactors = FALSE),
  data.frame(x = y_grid, y = scaled_beta_density_on_y(y_grid, alpha_hat, beta_hat, opts$lower_bound, opts$upper_bound), source = "Beta ajustado (média posterior)", stringsAsFactors = FALSE),
  data.frame(x = y_grid, y = stats::dnorm(y_grid, mean = mu_hat, sd = sd_hat), source = "Normal baseline", stringsAsFactors = FALSE)
)

p_compare <- ggplot(comparison_df, aes(x = x, y = y, linetype = source)) +
  geom_line(linewidth = 0.9) +
  labs(
    title = paste("Comparação de densidades -", opts$target_var),
    x = opts$target_var,
    y = "Densidade"
  ) +
  theme_minimal(base_size = 12)

if (isTRUE(opts$save_plots == 1L)) {
  save_plot(p_compare, file.path(FIGURES_DIR, paste0("comparacao_densidades_beta_vs_normal_", opts$target_var, "_", TIMESTAMP_TAG, ".png")))
}

# -----------------------------
# PPC a posteriori - estatísticas resumo
# -----------------------------
sec("10) PPC A POSTERIORI — ESTATÍSTICAS RESUMO")

set.seed(opts$seed + 1L)
ndraws_available <- nrow(posterior_draws)
stat_draw_ids <- sample(seq_len(ndraws_available), size = min(opts$n_ppc_stats_draws, ndraws_available))

obs_stats <- c(
  mean = mean(y),
  sd = stats::sd(y),
  q05 = unname(stats::quantile(y, 0.05)),
  median = stats::median(y),
  q95 = unname(stats::quantile(y, 0.95))
)

rep_stats <- data.frame(
  draw_id = integer(0),
  mean = numeric(0),
  sd = numeric(0),
  q05 = numeric(0),
  median = numeric(0),
  q95 = numeric(0),
  stringsAsFactors = FALSE
)

for (idx in stat_draw_ids) {
  y_rep <- stats::rbeta(N, posterior_draws$alpha[idx], posterior_draws$beta[idx]) * (opts$upper_bound - opts$lower_bound) + opts$lower_bound
  rep_stats <- rbind(
    rep_stats,
    data.frame(
      draw_id = idx,
      mean = mean(y_rep),
      sd = stats::sd(y_rep),
      q05 = unname(stats::quantile(y_rep, 0.05)),
      median = stats::median(y_rep),
      q95 = unname(stats::quantile(y_rep, 0.95)),
      stringsAsFactors = FALSE
    )
  )
}

if (isTRUE(opts$save_tables == 1L)) {
  utils::write.csv(
    rep_stats,
    file = file.path(TABLES_DIR, paste0("ppc_summary_stats_rep_", opts$target_var, "_", TIMESTAMP_TAG, ".csv")),
    row.names = FALSE
  )
}

ppc_long <- rbind(
  data.frame(stat = "mean", value = rep_stats$mean, source = "Replicado", stringsAsFactors = FALSE),
  data.frame(stat = "sd", value = rep_stats$sd, source = "Replicado", stringsAsFactors = FALSE),
  data.frame(stat = "q05", value = rep_stats$q05, source = "Replicado", stringsAsFactors = FALSE),
  data.frame(stat = "median", value = rep_stats$median, source = "Replicado", stringsAsFactors = FALSE),
  data.frame(stat = "q95", value = rep_stats$q95, source = "Replicado", stringsAsFactors = FALSE)
)

obs_long <- data.frame(
  stat = names(obs_stats),
  value = as.numeric(obs_stats),
  source = "Observado",
  stringsAsFactors = FALSE
)

p_ppc_stats <- ggplot(ppc_long, aes(x = value)) +
  geom_histogram(bins = 30) +
  geom_vline(data = obs_long, aes(xintercept = value), linetype = "dashed", linewidth = 0.8) +
  facet_wrap(~ stat, scales = "free", ncol = 2) +
  labs(
    title = paste("PPC a posteriori - estatísticas resumo -", opts$target_var),
    x = "Valor da estatística",
    y = "Frequência"
  ) +
  theme_minimal(base_size = 12)

if (isTRUE(opts$save_plots == 1L)) {
  save_plot(p_ppc_stats, file.path(FIGURES_DIR, paste0("ppc_posterior_summary_stats_", opts$target_var, "_", TIMESTAMP_TAG, ".png")), width = 11, height = 8)
}

# -----------------------------
# PPC a posteriori - sobreposição de densidades
# -----------------------------
sec("11) PPC A POSTERIORI — SOBREPOSIÇÃO DE DENSIDADES")

set.seed(opts$seed + 2L)
density_draw_ids <- sample(seq_len(ndraws_available), size = min(opts$n_ppc_density_draws, ndraws_available))

y_obs_sub <- sample(y, size = min(opts$ppc_density_n, N))
d_obs_sub <- safe_density(y_obs_sub, from = opts$lower_bound, to = opts$upper_bound, n = 512)

ppc_density_df <- data.frame(
  x = d_obs_sub$x,
  y = d_obs_sub$y,
  draw_id = 0L,
  source = "Observado",
  stringsAsFactors = FALSE
)

for (idx in density_draw_ids) {
  y_rep <- stats::rbeta(length(y_obs_sub), posterior_draws$alpha[idx], posterior_draws$beta[idx]) * (opts$upper_bound - opts$lower_bound) + opts$lower_bound
  d_rep <- safe_density(y_rep, from = opts$lower_bound, to = opts$upper_bound, n = 512)
  ppc_density_df <- rbind(
    ppc_density_df,
    data.frame(x = d_rep$x, y = d_rep$y, draw_id = idx, source = "Replicado", stringsAsFactors = FALSE)
  )
}

p_ppc_density <- ggplot(ppc_density_df, aes(x = x, y = y, group = interaction(source, draw_id), linetype = source)) +
  geom_line(data = subset(ppc_density_df, source == "Replicado"), alpha = 0.25) +
  geom_line(data = subset(ppc_density_df, source == "Observado"), linewidth = 1) +
  labs(
    title = paste("PPC a posteriori - sobreposição de densidades -", opts$target_var),
    x = opts$target_var,
    y = "Densidade"
  ) +
  theme_minimal(base_size = 12)

if (isTRUE(opts$save_plots == 1L)) {
  save_plot(p_ppc_density, file.path(FIGURES_DIR, paste0("ppc_posterior_density_overlay_", opts$target_var, "_", TIMESTAMP_TAG, ".png")))
}

# -----------------------------
# Relatório executivo
# -----------------------------
sec("12) RELATÓRIO EXECUTIVO")

normal_mass_below_zero <- stats::pnorm(opts$lower_bound, mean = mean(y), sd = stats::sd(y))

report_lines <- c(
  "CONSUMER SHOPPING TRENDS — RELATÓRIO EXECUTIVO",
  "============================================================",
  paste("Arquivo lido:", INPUT_CSV),
  paste("Variável-alvo:", opts$target_var),
  paste("N:", N),
  "",
  "Resumo observado:",
  paste("  Mínimo:", round(min(y), 4)),
  paste("  Q05:", round(unname(stats::quantile(y, 0.05)), 4)),
  paste("  Mediana:", round(stats::median(y), 4)),
  paste("  Média:", round(mean(y), 4)),
  paste("  Q95:", round(unname(stats::quantile(y, 0.95)), 4)),
  paste("  Máximo:", round(max(y), 4)),
  paste("  Desvio-padrão:", round(stats::sd(y), 4)),
  paste("  Assimetria aproximada:", round(compute_skewness(y), 6)),
  "",
  "Decisão de modelagem:",
  paste0("  - O alvo foi reescalonado de [", opts$lower_bound, ", ", opts$upper_bound, "] para (0,1)."),
  "  - O modelo principal é Beta(alpha, beta) com priors lognormal(0,1) para alpha e beta.",
  "  - O modelo Normal é mantido apenas como baseline visual de comparação.",
  paste("  - Massa de probabilidade abaixo de zero sob baseline Normal:", round(normal_mass_below_zero, 6)),
  "",
  "Posterior (médias):",
  paste("  alpha:", round(mean(posterior_draws$alpha), 6)),
  paste("  beta:", round(mean(posterior_draws$beta), 6)),
  paste("  kappa:", round(mean(posterior_draws$kappa), 6)),
  paste("  mean_z:", round(mean(posterior_draws$mean_z), 6)),
  paste("  mean_y:", round(mean(posterior_draws$mean_y), 6)),
  "",
  "Diagnósticos MCMC:",
  paste("  Divergências:", divergences),
  paste("  Hits de treedepth máximo:", treedepth_hits),
  paste("  Accept stat médio:", round(mean_accept_stat, 6)),
  paste("  Stepsize médio:", round(mean_stepsize, 6)),
  "",
  "Saídas principais:",
  paste("  - tables:", file.path(TABLES_DIR, paste0("posterior_summary_", opts$target_var, "_", TIMESTAMP_TAG, ".csv"))),
  paste("  - figures:", file.path(FIGURES_DIR, paste0("comparacao_densidades_beta_vs_normal_", opts$target_var, "_", TIMESTAMP_TAG, ".png"))),
  paste("  - figures:", file.path(FIGURES_DIR, paste0("ppc_posterior_summary_stats_", opts$target_var, "_", TIMESTAMP_TAG, ".png"))),
  paste("  - figures:", file.path(FIGURES_DIR, paste0("ppc_posterior_density_overlay_", opts$target_var, "_", TIMESTAMP_TAG, ".png")))
)

cat(paste(report_lines, collapse = "\n"), "\n")

if (isTRUE(opts$save_report == 1L)) {
  writeLines(report_lines, con = file.path(TABLES_DIR, paste0("relatorio_executivo_consumer_shopping_beta_", opts$target_var, "_", TIMESTAMP_TAG, ".txt")))
  writeLines(capture.output(sessionInfo()), con = file.path(TABLES_DIR, paste0("session_info_", TIMESTAMP_TAG, ".txt")))
  metadata_list <- list(
    timestamp = TIMESTAMP_TAG,
    input_csv = INPUT_CSV,
    target_var = opts$target_var,
    lower_bound = opts$lower_bound,
    upper_bound = opts$upper_bound,
    seed = opts$seed,
    chains = opts$chains,
    iter = opts$iter,
    warmup = opts$warmup,
    adapt_delta = opts$adapt_delta,
    max_treedepth = opts$max_treedepth,
    save_tables = opts$save_tables,
    save_plots = opts$save_plots,
    save_report = opts$save_report,
    save_fit_object = opts$save_fit_object
  )
  saveRDS(metadata_list, file = file.path(MODELS_DIR, paste0("metadata_consumer_shopping_beta_", TIMESTAMP_TAG, ".rds")))
}

log_message("Execução finalizada com sucesso.")
