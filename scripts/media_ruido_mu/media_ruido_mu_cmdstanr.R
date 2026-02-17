#!/usr/bin/env Rscript

# ============================================================
# Média vs Ruído (Lei dos Grandes N + Concentração Bayesiana em μ)
#
# Experimento: "Com mais medições, ruídos aleatórios tendem a se
# cancelar, revelando o efeito subjacente (μ)"
#
# R + cmdstanr
# ============================================================

options(stringsAsFactors = FALSE)

# Este script NÃO instala pacotes automaticamente por padrão.
# Recomendado (uma vez por máquina):
#   Rscript scripts/_setup/install_deps.R
#   Rscript scripts/_setup/install_cmdstan.R
#
# Se você quiser permitir auto-instalação aqui (não recomendado), rode:
#   Rscript scripts/media_ruido_mu/media_ruido_mu_cmdstanr.R --auto-install

args_boot <- commandArgs(trailingOnly = TRUE)
AUTO_INSTALL_PKGS <- "--auto-install" %in% args_boot

# Quando roda via Rscript (não-interativo), é mais útil salvar artefatos.
# Flags:
#   --save     -> força salvar (mesmo se interactive())
#   --no-save  -> não salva (somente console)
SAVE_ARTIFACTS <- (("--save" %in% args_boot) || (!interactive() && !("--no-save" %in% args_boot))) && !("--no-save" %in% args_boot)

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

ensure_repos_for_cmdstanr <- function() {
  repos <- getOption("repos")
  if (is.null(repos) || length(repos) == 0) repos <- c()
  if (identical(repos, "@CRAN@")) repos <- c()

  # garante names
  if (length(repos) > 0 && is.null(names(repos))) names(repos) <- paste0("repo", seq_along(repos))

  if (!("CRAN" %in% names(repos)) || is.null(repos[["CRAN"]]) || repos[["CRAN"]] == "" || repos[["CRAN"]] == "@CRAN@") {
    repos[["CRAN"]] <- "https://cloud.r-project.org"
  }
  if (!("stan" %in% names(repos)) || is.null(repos[["stan"]]) || repos[["stan"]] == "") {
    repos[["stan"]] <- "https://stan-dev.r-universe.dev"
  }
  options(repos = repos)
}

ensure_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (!AUTO_INSTALL_PKGS) {
      stop(
        "Pacote '", pkg, "' não está instalado.\n\n",
        "Como resolver (recomendado):\n",
        "  Rscript scripts/_setup/install_deps.R\n\n",
        "Alternativa (manual, dentro do R):\n",
        "  install.packages('", pkg, "')\n\n",
        "Se você realmente quer auto-instalar neste script:\n",
        "  Rscript scripts/media_ruido_mu/media_ruido_mu_cmdstanr.R --auto-install\n",
        call. = FALSE
      )
    }
    message("Instalando pacote ausente: ", pkg)
    ensure_repos_for_cmdstanr()
    install.packages(pkg, dependencies = TRUE)
  }
  invisible(TRUE)
}

get_arg_value <- function(prefix, default = NULL) {
  hit <- grep(paste0("^", gsub("([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1", prefix)), args_boot, value = TRUE)
  if (length(hit) == 0) return(default)
  sub(prefix, "", hit[[1]])
}

# ------------------------------------------------------------
# Pacotes
# ------------------------------------------------------------

pkgs <- c("here", "cmdstanr", "posterior", "ggplot2", "dplyr", "tidyr", "purrr", "tibble")
invisible(lapply(pkgs, ensure_pkg))

suppressPackageStartupMessages({
  library(here)
  library(cmdstanr)
  library(posterior)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

# Fixar root do projeto (assim roda mesmo com working dir "errado")
try(here::i_am("scripts/media_ruido_mu/media_ruido_mu_cmdstanr.R"), silent = TRUE)

# ------------------------------------------------------------
# Saídas (quando aplicável)
# ------------------------------------------------------------

out_pdf   <- here::here("outputs", "figures", "media_ruido_mu_plots.pdf")
out_post  <- here::here("outputs", "tables",  "media_ruido_mu_posterior_summary.csv")
out_sim   <- here::here("outputs", "tables",  "media_ruido_mu_sim_summary.csv")

if (SAVE_ARTIFACTS) {
  dir.create(dirname(out_pdf),  recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_post), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_sim),  recursive = TRUE, showWarnings = FALSE)

  grDevices::pdf(out_pdf, width = 8, height = 6)
  on.exit(grDevices::dev.off(), add = TRUE)
}

# ------------------------------------------------------------
# Parâmetros (você pode mexer aqui ou via CLI)
# ------------------------------------------------------------

seed <- as.integer(get_arg_value("--seed=", 123))
set.seed(seed)

mu_true    <- as.numeric(get_arg_value("--mu_true=", 2.0))
sigma_true <- as.numeric(get_arg_value("--sigma_true=", 5.0))

# Ns em formato: --Ns=5,10,20,50,100,200,500,1000
Ns_str <- get_arg_value("--Ns=", "5,10,20,50,100,200,500,1000")
Ns <- as.integer(strsplit(Ns_str, ",", fixed = TRUE)[[1]])
Ns <- Ns[!is.na(Ns) & Ns > 0]
if (length(Ns) == 0) stop("Ns vazio/inválido. Use, por exemplo: --Ns=5,10,50", call. = FALSE)

R_rep <- as.integer(get_arg_value("--R_rep=", 400))
if (is.na(R_rep) || R_rep < 1) stop("R_rep inválido (>=1).", call. = FALSE)

chains <- as.integer(get_arg_value("--chains=", 4))
iter_warmup   <- as.integer(get_arg_value("--iter_warmup=", 500))
iter_sampling <- as.integer(get_arg_value("--iter_sampling=", 500))
adapt_delta   <- as.numeric(get_arg_value("--adapt_delta=", 0.95))
max_treedepth <- as.integer(get_arg_value("--max_treedepth=", 12))

options(mc.cores = parallel::detectCores())

# ============================================================
# 1) Checagem do CmdStan
# ============================================================

cmdstan_ver <- cmdstanr::cmdstan_version(error_on_NA = FALSE)
if (is.null(cmdstan_ver)) {
  stop(
    "CmdStan não encontrado.\n\n",
    "Como resolver:\n",
    "  Rscript scripts/_setup/install_cmdstan.R\n\n",
    "(ou, dentro do R: cmdstanr::install_cmdstan())\n",
    call. = FALSE
  )
} else {
  message("CmdStan detectado. Versão: ", as.character(cmdstan_ver))
}

# ============================================================
# 2) BLOCO A: SIMULAÇÃO (SEM STAN)
# ============================================================

sim_one <- function(N, mu = mu_true, sigma = sigma_true) {
  y <- rnorm(N, mean = mu, sd = sigma)
  tibble(N = N, ybar = mean(y))
}

sim_df <- purrr::map_dfr(Ns, function(N) {
  purrr::map_dfr(seq_len(R_rep), function(r) sim_one(N))
})

theory_df <- tibble(
  N = Ns,
  sd_ybar_theory = sigma_true / sqrt(N)
)

# Histograms da média amostral por N
p_sim <- sim_df %>%
  mutate(N_f = factor(N, levels = Ns)) %>%
  ggplot(aes(x = ybar)) +
  geom_histogram(bins = 40) +
  facet_wrap(~ N_f, scales = "free_y") +
  geom_vline(xintercept = mu_true, linetype = 2) +
  labs(
    title = "Mais medições ⇒ a média amostral concentra em torno de μ (ruído se cancela)",
    subtitle = "Linha tracejada = μ verdadeiro",
    x = expression(bar(y)),
    y = "contagem"
  )

# SD empírico da média vs teoria sigma/sqrt(N)
sd_emp <- sim_df %>%
  group_by(N) %>%
  summarise(sd_ybar_emp = sd(ybar), .groups = "drop") %>%
  left_join(theory_df, by = "N") %>%
  pivot_longer(
    cols = c(sd_ybar_emp, sd_ybar_theory),
    names_to = "tipo", values_to = "sd_ybar"
  )

p_sd <- sd_emp %>%
  ggplot(aes(x = N, y = sd_ybar)) +
  geom_line() +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ tipo, scales = "free_y") +
  labs(
    title = "Assinatura teórica: SD(bar(y)) ≈ sigma / sqrt(N)",
    x = "N (log10)",
    y = "SD de bar(y) (log10)"
  )

print(p_sim)
print(p_sd)

# Salva resumo do bloco A (quando aplicável)
if (SAVE_ARTIFACTS) {
  sd_emp_wide <- sd_emp %>%
    tidyr::pivot_wider(names_from = tipo, values_from = sd_ybar) %>%
    arrange(N)
  utils::write.csv(sd_emp_wide, out_sim, row.names = FALSE)
}

# ============================================================
# 3) BLOCO B: BAYES COM CMDSTAN (posterior de μ concentra)
# ============================================================

stan_code <- "
data {
  int<lower=1> N;
  vector[N] y;
}
parameters {
  real mu;
  real<lower=1e-9> sigma;   // Normal exige sigma>0 (evita sigma=0)
}
model {
  // priors fracos, mas razoáveis
  mu ~ normal(0, 10);
  sigma ~ exponential(1);

  // likelihood
  y ~ normal(mu, sigma);
}
"

stan_file <- cmdstanr::write_stan_file(stan_code)
mod <- cmdstanr::cmdstan_model(stan_file)

make_data <- function(N, mu = mu_true, sigma = sigma_true) {
  y <- rnorm(N, mu, sigma)
  list(N = N, y = y)
}

fit_oneN <- function(N) {
  dat <- make_data(N)

  fit <- mod$sample(
    data = dat,
    chains = chains,
    parallel_chains = min(chains, parallel::detectCores()),
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    refresh = 0,
    seed = seed + N,

    # init explícito: reduz chance de propostas colarem em sigma ~ 0
    init = function() list(
      mu = rnorm(1, 0, 2),
      sigma = runif(1, 0.5, 10)
    ),

    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth
  )

  draws <- fit$draws(variables = c("mu", "sigma"))

  summ <- posterior::summarise_draws(
    draws,
    mean,
    sd,
    ~posterior::quantile2(.x, probs = c(0.05, 0.5, 0.95))
  )

  mu_row <- summ %>% dplyr::filter(variable == "mu")

  tibble(
    N = N,
    mu_mean = mu_row$mean,
    mu_q05  = mu_row$q5,
    mu_q50  = mu_row$q50,
    mu_q95  = mu_row$q95,
    mu_ci_width = mu_row$q95 - mu_row$q5
  )
}

post_df <- purrr::map_dfr(Ns, fit_oneN)
print(post_df)

# Posterior interval de μ vs N
p_post <- post_df %>%
  ggplot(aes(x = N, y = mu_q50)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mu_q05, ymax = mu_q95), width = 0) +
  geom_hline(yintercept = mu_true, linetype = 2) +
  scale_x_log10() +
  labs(
    title = "Posterior de μ concentra com N (efeito subjacente aparece)",
    subtitle = "Pontos/linha = mediana posterior; barras = ICr 90%; tracejado = μ verdadeiro",
    x = "N (log10)",
    y = expression(mu)
  )

# Largura do intervalo ~ N^{-1/2}
p_width <- post_df %>%
  ggplot(aes(x = N, y = mu_ci_width)) +
  geom_line() +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Largura do ICr 90% de μ cai aproximadamente como N^{-1/2}",
    x = "N (log10)",
    y = "largura do ICr 90% (log10)"
  )

print(p_post)
print(p_width)

# Regressão log-log: slope esperado ~ -0.5
lm_fit <- lm(log(mu_ci_width) ~ log(N), data = post_df)
print(summary(lm_fit))
cat("\nInclinação estimada (esperado ~ -0.5): ", coef(lm_fit)[2], "\n")

# Salva resumo do bloco B (quando aplicável)
if (SAVE_ARTIFACTS) {
  utils::write.csv(post_df, out_post, row.names = FALSE)
  message("\nArtefatos salvos em:\n  ", out_pdf, "\n  ", out_sim, "\n  ", out_post)
}
