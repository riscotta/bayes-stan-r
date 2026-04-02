#!/usr/bin/env Rscript

###############################################################################
# FruitFlies — replicacao Bayesiana do estudo com R + Stan via cmdstanr
# Modelo principal: AFT log-normal (normal em log(y))
# Padrão do repositório: sem setwd(), execução a partir do root, console-only
# por padrão; saídas opcionais em outputs/.
#
# Como rodar:
#   Rscript scripts/fruitflies_aft/fruitflies_aft_lognormal_cmdstanr.R
#
# Flags úteis (formato --chave=valor):
#   --seed=42
#   --chains=4
#   --parallel_chains=4
#   --iter_warmup=1000
#   --iter_sampling=2000
#   --adapt_delta=0.99
#   --max_treedepth=12
#   --refresh=250
#   --save_plots=0|1
#   --save_report=0|1
#
# Dependências:
#   Rscript scripts/_setup/install_deps.R --all
#   Rscript scripts/_setup/install_cmdstan.R
###############################################################################

options(stringsAsFactors = FALSE)
options(mc.cores = max(1L, parallel::detectCores(logical = TRUE)))

parse_args <- function(args) {
  out <- list(
    seed = 42L,
    chains = 4L,
    parallel_chains = 4L,
    iter_warmup = 1000L,
    iter_sampling = 2000L,
    adapt_delta = 0.99,
    max_treedepth = 12L,
    refresh = 250L,
    save_plots = 0L,
    save_report = 0L
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
  out$chains <- as.integer(out$chains)
  out$parallel_chains <- as.integer(out$parallel_chains)
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
      "\nRode primeiro: Rscript scripts/_setup/install_deps.R --all",
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

summarise_posterior <- function(x) {
  c(
    media = mean(x),
    sd = stats::sd(x),
    q05 = unname(stats::quantile(x, 0.05)),
    q50 = unname(stats::quantile(x, 0.50)),
    q95 = unname(stats::quantile(x, 0.95))
  )
}

contrast_summary <- function(a, b, label_a, label_b) {
  diff <- a - b
  ratio <- a / b
  data.frame(
    contraste = paste0(label_a, " vs ", label_b),
    prob_maior = mean(diff > 0),
    diff_media = mean(diff),
    diff_q05 = unname(stats::quantile(diff, 0.05)),
    diff_q50 = unname(stats::quantile(diff, 0.50)),
    diff_q95 = unname(stats::quantile(diff, 0.95)),
    razao_media = mean(ratio),
    razao_q05 = unname(stats::quantile(ratio, 0.05)),
    razao_q50 = unname(stats::quantile(ratio, 0.50)),
    razao_q95 = unname(stats::quantile(ratio, 0.95))
  )
}

args <- commandArgs(trailingOnly = TRUE)
opt <- parse_args(args)

pkgs <- c("cmdstanr", "posterior", "bayesplot", "Stat2Data", "dplyr", "ggplot2", "tibble")
require_pkgs(pkgs)

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(bayesplot)
  library(Stat2Data)
  library(dplyr)
  library(ggplot2)
  library(tibble)
})

set.seed(opt$seed)
check_cmdstan()

cat("\n================ LEITURA E PREPARACAO ================\n")

# -----------------------------
# 1) Dados
# -----------------------------
data("FruitFlies", package = "Stat2Data", envir = environment())
fruitflies <- get("FruitFlies", envir = environment())

df <- fruitflies %>%
  transmute(
    id = ID,
    treatment = factor(
      Treatment,
      levels = c("none", "1 pregnant", "1 virgin", "8 pregnant", "8 virgin")
    ),
    longevity = as.numeric(Longevity),
    thorax = as.numeric(Thorax),
    sleep = as.numeric(Sleep)
  ) %>%
  mutate(
    thorax_z = as.numeric(scale(thorax))
  )

if (anyNA(df$longevity) || any(df$longevity <= 0)) {
  stop("A variável 'longevity' precisa ser positiva e sem NA.", call. = FALSE)
}
if (anyNA(df$thorax_z)) {
  stop("A padronização de 'thorax' gerou NA; verifique a base.", call. = FALSE)
}
if (anyNA(df$treatment)) {
  stop("A variável 'treatment' contém NA.", call. = FALSE)
}

cat("Fonte              : Stat2Data::FruitFlies\n")
cat("N                  :", nrow(df), "\n")
cat("Tratamentos        :", paste(levels(df$treatment), collapse = ", "), "\n")
cat("Observacao         : 'sleep' foi lida, mas nao entra no modelo principal por ser potencialmente pos-tratamento.\n")

# -----------------------------
# 2) Matriz de desenho
# -----------------------------
X <- model.matrix(~ treatment + thorax_z, data = df)
colnames(X) <- make.names(colnames(X))

newdata_ref <- data.frame(
  treatment = factor(
    c("none", "1 pregnant", "1 virgin", "8 pregnant", "8 virgin"),
    levels = levels(df$treatment)
  ),
  thorax_z = 0
)
X_ref <- model.matrix(~ treatment + thorax_z, data = newdata_ref)
colnames(X_ref) <- make.names(colnames(X_ref))
group_labels <- as.character(newdata_ref$treatment)

stan_data <- list(
  N = nrow(X),
  K = ncol(X),
  X = unname(X),
  log_y = log(df$longevity),
  G = nrow(X_ref),
  X_ref = unname(X_ref)
)

# -----------------------------
# 3) Modelo Stan
# -----------------------------
stan_code <- '
data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N, K] X;
  vector[N] log_y;
  int<lower=1> G;
  matrix[G, K] X_ref;
}
parameters {
  vector[K] beta;
  real<lower=0> sigma;
}
model {
  beta ~ normal(0, 1.5);
  sigma ~ lognormal(log(0.3), 0.5);
  log_y ~ normal(X * beta, sigma);
}
generated quantities {
  vector[N] log_lik;
  vector[G] mu_ref;
  vector[G] mean_days_ref;
  vector[G] median_days_ref;

  mu_ref = X_ref * beta;

  for (n in 1:N) {
    log_lik[n] = normal_lpdf(log_y[n] | dot_product(X[n], beta), sigma);
  }

  for (g in 1:G) {
    mean_days_ref[g] = exp(mu_ref[g] + 0.5 * square(sigma));
    median_days_ref[g] = exp(mu_ref[g]);
  }
}
'

stan_file <- cmdstanr::write_stan_file(stan_code, dir = tempdir())
mod <- cmdstanr::cmdstan_model(stan_file)

fit <- mod$sample(
  data = stan_data,
  seed = opt$seed,
  chains = opt$chains,
  parallel_chains = opt$parallel_chains,
  iter_warmup = opt$iter_warmup,
  iter_sampling = opt$iter_sampling,
  refresh = opt$refresh,
  adapt_delta = opt$adapt_delta,
  max_treedepth = opt$max_treedepth,
  init = 0.1
)

# -----------------------------
# 4) Diagnosticos
# -----------------------------
cat("\n================ DIAGNOSTICOS ================\n")
print(fit$cmdstan_diagnose())

sum_fit <- fit$summary()
beta_vars <- paste0("beta[", seq_len(ncol(X)), "]")
key_vars <- c(beta_vars, "sigma")
key_pars <- sum_fit %>%
  filter(variable %in% key_vars)
print(key_pars)

# -----------------------------
# 5) Extracao posterior
# -----------------------------
draws_df <- fit$draws(format = "df")

mean_cols <- paste0("mean_days_ref[", seq_along(group_labels), "]")
median_cols <- paste0("median_days_ref[", seq_along(group_labels), "]")

posterior_means <- draws_df[, mean_cols, drop = FALSE]
posterior_medians <- draws_df[, median_cols, drop = FALSE]
colnames(posterior_means) <- group_labels
colnames(posterior_medians) <- group_labels

resumo_medias <- t(apply(posterior_means, 2, summarise_posterior)) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("tratamento")

resumo_medianas <- t(apply(posterior_medians, 2, summarise_posterior)) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("tratamento")

cat("\n================ LONGEVIDADE ESPERADA (dias) ================\n")
print(resumo_medias)

cat("\n================ MEDIANA POSTERIOR (dias) ================\n")
print(resumo_medianas)

# -----------------------------
# 6) Contrastes de interesse
# -----------------------------
contrastes <- bind_rows(
  contrast_summary(posterior_means[["1 virgin"]], posterior_means[["none"]], "1 virgin", "none"),
  contrast_summary(posterior_means[["8 virgin"]], posterior_means[["none"]], "8 virgin", "none"),
  contrast_summary(posterior_means[["1 pregnant"]], posterior_means[["none"]], "1 pregnant", "none"),
  contrast_summary(posterior_means[["8 pregnant"]], posterior_means[["none"]], "8 pregnant", "none"),
  contrast_summary(posterior_means[["1 virgin"]], posterior_means[["1 pregnant"]], "1 virgin", "1 pregnant"),
  contrast_summary(posterior_means[["8 virgin"]], posterior_means[["8 pregnant"]], "8 virgin", "8 pregnant"),
  contrast_summary(posterior_means[["8 virgin"]], posterior_means[["1 virgin"]], "8 virgin", "1 virgin")
)

cat("\n================ CONTRASTES POSTERIORES ================\n")
print(contrastes)

# -----------------------------
# 7) Efeito do thorax
# -----------------------------
beta_names <- colnames(X)
idx_thorax <- which(beta_names == "thorax_z")
thorax_draws <- draws_df[[paste0("beta[", idx_thorax, "]")]]

cat("\n================ EFEITO DE THORAX (escala log-dias) ================\n")
print(summarise_posterior(thorax_draws))

cat("\nInterpretacao aproximada:\n")
cat(
  "Multiplicador na mediana para +1 DP em thorax = ",
  round(exp(mean(thorax_draws)), 3),
  "\n",
  sep = ""
)

# -----------------------------
# 8) PPC simples
# -----------------------------
mu_draws <- posterior::as_draws_matrix(fit$draws("beta"))
n_ppc <- min(200, nrow(mu_draws))
mu_log <- mu_draws[1:n_ppc, , drop = FALSE] %*% t(X)
sigma_draws <- draws_df$sigma[1:n_ppc]
yrep_mat <- matrix(NA_real_, nrow = n_ppc, ncol = nrow(df))

for (s in seq_len(n_ppc)) {
  yrep_mat[s, ] <- rlnorm(nrow(df), meanlog = mu_log[s, ], sdlog = sigma_draws[s])
}

ppc_plot <- bayesplot::ppc_dens_overlay(
  y = df$longevity,
  yrep = yrep_mat[1:min(50, n_ppc), , drop = FALSE]
)

# -----------------------------
# 9) Grafico de medias ajustadas
# -----------------------------
plot_df <- resumo_medias %>%
  mutate(tratamento = factor(tratamento, levels = group_labels))

mean_plot <- ggplot(plot_df, aes(x = tratamento, y = q50)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = q05, ymax = q95), width = 0.15) +
  labs(
    title = "FruitFlies — longevidade ajustada por tratamento",
    subtitle = "Posterior da media em dias, com thorax fixado na media",
    x = NULL,
    y = "Dias"
  ) +
  theme_minimal(base_size = 12)

print(ppc_plot)
print(mean_plot)

# -----------------------------
# 10) Leitura objetiva
# -----------------------------
ordem <- resumo_medias %>% arrange(desc(q50)) %>% pull(tratamento)

cat("\n================ LEITURA OBJETIVA ================\n")
cat("Ordem das medianas posteriores das medias ajustadas:\n")
cat(paste(ordem, collapse = " > "), "\n")
cat(
  "Probabilidade posterior de 8 virgin ter menor longevidade esperada que none: ",
  round(mean(posterior_means[["8 virgin"]] < posterior_means[["none"]]), 4),
  "\n",
  sep = ""
)
cat(
  "Probabilidade posterior de 1 virgin ter menor longevidade esperada que 1 pregnant: ",
  round(mean(posterior_means[["1 virgin"]] < posterior_means[["1 pregnant"]]), 4),
  "\n",
  sep = ""
)
cat(
  "Probabilidade posterior de 8 virgin ter menor longevidade esperada que 8 pregnant: ",
  round(mean(posterior_means[["8 virgin"]] < posterior_means[["8 pregnant"]]), 4),
  "\n",
  sep = ""
)

cat(
  "\nObservacao metodologica: esta replicacao usa um modelo AFT log-normal.\n",
  "A parametrizacao em log(y) foi usada para melhorar a estabilidade numerica,\n",
  "mantendo equivalencia inferencial para o alvo principal do estudo.\n",
  sep = ""
)

# -----------------------------
# 11) Saidas opcionais
# -----------------------------
if (opt$save_plots == 1L) {
  fig_dir <- file.path("outputs", "figures", "fruitflies_aft")
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  fig_path <- file.path(fig_dir, "fruitflies_aft_plots.pdf")

  grDevices::pdf(fig_path, width = 10, height = 7)
  print(ppc_plot)
  print(mean_plot)
  grDevices::dev.off()

  cat("\n[OK] PDF salvo em:", fig_path, "\n")
}

if (opt$save_report == 1L) {
  tab_dir <- file.path("outputs", "tables", "fruitflies_aft")
  dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)
  report_path <- file.path(tab_dir, "fruitflies_aft_report.txt")

  report_lines <- capture.output({
    cat("FruitFlies — AFT log-normal via cmdstanr\n")
    cat("======================================\n\n")
    cat("N:", nrow(df), "\n")
    cat("Tratamentos:", paste(group_labels, collapse = ", "), "\n\n")

    cat("Diagnosticos (CmdStan)\n")
    print(fit$cmdstan_diagnose())

    cat("\nParametros-chave\n")
    print(key_pars)

    cat("\nLongevidade esperada (dias)\n")
    print(resumo_medias)

    cat("\nMediana posterior (dias)\n")
    print(resumo_medianas)

    cat("\nContrastes posteriores\n")
    print(contrastes)

    cat("\nEfeito do thorax\n")
    print(summarise_posterior(thorax_draws))
  })

  writeLines(report_lines, con = report_path, useBytes = TRUE)
  cat("[OK] Relatorio salvo em:", report_path, "\n")
}
