# ============================================================
# Therapeutic Touch (TherapeuticTouchData.csv) — Análise Bayesiana
# R + cmdstanr (Stan) com Prior Predictive e Posterior Predictive Checks
# ============================================================

# -----------------------------
# 0) Parâmetros que você pode mexer
# -----------------------------
file_path <- "C:/Users/rs44925/Downloads/dados.txt"  # use / (barra normal) no Windows
seed <- 1234

chains <- 4
iter_warmup <- 1000
iter_sampling <- 2000
adapt_delta <- 0.95
max_treedepth <- 12

# -----------------------------
# 1) Pacotes
# -----------------------------
pkg <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, dependencies = TRUE)
  invisible(TRUE)
}

pkgs <- c("readr","dplyr","tidyr","stringr","ggplot2",
          "cmdstanr","posterior","bayesplot","loo")
invisible(lapply(pkgs, pkg))

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(loo)

options(mc.cores = parallel::detectCores())
bayesplot_theme_set(theme_minimal())

# -----------------------------
# 2) Checagem do CmdStan
# -----------------------------
# Se isso falhar, rode: cmdstanr::install_cmdstan()
if (is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
  stop("CmdStan não encontrado. Rode cmdstanr::install_cmdstan() e tente novamente.")
}

# -----------------------------
# 3) Leitura e limpeza do CSV
# -----------------------------
if (!file.exists(file_path)) {
  stop("Arquivo não encontrado em: ", file_path,
       "\nConfira o caminho (dica: use barras / no Windows).")
}

df_raw <- readr::read_csv(file_path, show_col_types = FALSE)
names(df_raw) <- names(df_raw) |>
  str_trim() |>
  str_to_lower() |>
  str_replace_all("\\s+", "_")

# Esperado: colunas y e s (como no TherapeuticTouchData)
if (!all(c("y","s") %in% names(df_raw))) {
  stop("Não encontrei automaticamente as colunas 'y' e 's'.\n",
       "Colunas atuais: ", paste(names(df_raw), collapse = ", "), "\n",
       "Se estiver diferente, renomeie para y (0/1) e s (id do sujeito).")
}

df <- df_raw |>
  transmute(
    y = as.integer(y),
    s = as.character(s)
  )

# Validações
if (any(is.na(df$y)) || any(is.na(df$s))) stop("Há NA em y ou s.")
if (!all(df$y %in% c(0,1))) stop("y deve ser 0/1. Valores encontrados: ", paste(unique(df$y), collapse=", "))

# Index do sujeito (1..J)
df <- df |>
  mutate(s = as.factor(s),
         subj = as.integer(s))

N <- nrow(df)
J <- nlevels(df$s)

# Resumo por sujeito
df_by <- df |>
  group_by(s) |>
  summarise(
    n = n(),
    k = sum(y),
    prop = mean(y),
    .groups = "drop"
  ) |>
  arrange(prop)

# -----------------------------
# 4) EDA rápida (gráficos opcionais)
# -----------------------------
p1 <- ggplot(df_by, aes(x = reorder(s, prop), y = prop)) +
  geom_col() +
  coord_flip() +
  labs(title = "Proporção observada de acerto (y=1) por sujeito", x = "Sujeito", y = "Proporção")

p2 <- ggplot(df, aes(x = y)) +
  geom_bar() +
  scale_x_continuous(breaks = c(0,1)) +
  labs(title = "Distribuição global de y", x = "y", y = "Contagem")

print(p1)
print(p2)

k_total <- sum(df$y)
binom_test <- binom.test(k_total, N, p = 0.5, alternative = "two.sided")

# -----------------------------
# 5) Stan — Modelo pooled (um theta global)
# -----------------------------
stan_pooled <- "
data {
  int<lower=1> N;
  array[N] int<lower=0, upper=1> y;
  int<lower=0, upper=1> prior_only;
}
parameters {
  real alpha;
}
model {
  alpha ~ normal(0, 1.5);
  if (prior_only == 0) {
    y ~ bernoulli_logit(alpha);
  }
}
generated quantities {
  real theta = inv_logit(alpha);
  array[N] int y_rep;
  vector[N] log_lik;

  for (n in 1:N) {
    y_rep[n] = bernoulli_logit_rng(alpha);
    if (prior_only == 0) log_lik[n] = bernoulli_logit_lpmf(y[n] | alpha);
    else log_lik[n] = 0;
  }
}
"

mod_pooled <- cmdstan_model(write_stan_file(stan_pooled))

data_pooled <- list(N = N, y = df$y, prior_only = 0)
data_pooled_prior <- list(N = N, y = df$y, prior_only = 1)

fit_pooled_prior <- mod_pooled$sample(
  data = data_pooled_prior,
  seed = seed,
  chains = chains,
  iter_warmup = 0,
  iter_sampling = iter_sampling,
  fixed_param = TRUE,
  refresh = 0
)

fit_pooled <- mod_pooled$sample(
  data = data_pooled,
  seed = seed,
  chains = chains,
  iter_warmup = iter_warmup,
  iter_sampling = iter_sampling,
  adapt_delta = adapt_delta,
  max_treedepth = max_treedepth,
  refresh = 200
)

# -----------------------------
# 6) Stan — Modelo hierárquico (theta por sujeito, shrinkage)
# -----------------------------
stan_hier <- "
data {
  int<lower=1> N;
  int<lower=1> J;
  array[N] int<lower=0, upper=1> y;
  array[N] int<lower=1, upper=J> subj;
  int<lower=0, upper=1> prior_only;
}
parameters {
  real alpha;
  vector[J] z;
  real<lower=0> sigma;
}
transformed parameters {
  vector[J] a = sigma * z; // desvios por sujeito no logit
}
model {
  alpha ~ normal(0, 1.5);
  sigma ~ exponential(1);
  z ~ std_normal();

  if (prior_only == 0) {
    for (n in 1:N) {
      y[n] ~ bernoulli_logit(alpha + a[subj[n]]);
    }
  }
}
generated quantities {
  real theta_pop = inv_logit(alpha);
  vector[J] theta_subj;
  array[N] int y_rep;
  vector[N] log_lik;

  for (j in 1:J) theta_subj[j] = inv_logit(alpha + a[j]);

  for (n in 1:N) {
    y_rep[n] = bernoulli_logit_rng(alpha + a[subj[n]]);
    if (prior_only == 0) log_lik[n] = bernoulli_logit_lpmf(y[n] | alpha + a[subj[n]]);
    else log_lik[n] = 0;
  }
}
"

mod_hier <- cmdstan_model(write_stan_file(stan_hier))

data_hier <- list(N = N, J = J, y = df$y, subj = df$subj, prior_only = 0)
data_hier_prior <- list(N = N, J = J, y = df$y, subj = df$subj, prior_only = 1)

fit_hier_prior <- mod_hier$sample(
  data = data_hier_prior,
  seed = seed + 1,
  chains = chains,
  iter_warmup = 0,
  iter_sampling = iter_sampling,
  fixed_param = TRUE,
  refresh = 0
)

fit_hier <- mod_hier$sample(
  data = data_hier,
  seed = seed + 2,
  chains = chains,
  iter_warmup = iter_warmup,
  iter_sampling = iter_sampling,
  adapt_delta = adapt_delta,
  max_treedepth = max_treedepth,
  refresh = 200
)

# -----------------------------
# 7) Funções auxiliares: diagnósticos + PPC
# -----------------------------
summ_draws <- function(fit, vars) {
  draws <- fit$draws(vars)
  
  q05_q95 <- function(x) {
    out <- posterior::quantile2(x, probs = c(0.05, 0.95))
    names(out) <- c("q05", "q95")  # opcional, mas deixa explícito
    out
  }
  
  posterior::summarise_draws(
    draws,
    mean   = base::mean,
    sd     = stats::sd,
    median = stats::median,
    mad    = stats::mad,
    q05_q95,
    rhat     = posterior::rhat,
    ess_bulk = posterior::ess_bulk,
    ess_tail = posterior::ess_tail
  )
}

sampler_diag_summary <- function(fit) {
  sdg <- fit$sampler_diagnostics()
  div <- sum(sdg[, , "divergent__"])
  td <- sdg[, , "treedepth__"]
  list(
    divergences = div,
    max_treedepth_seen = max(td)
  )
}

extract_yrep_matrix <- function(fit) {
  yrep <- fit$draws("y_rep")
  yrep_df <- posterior::as_draws_df(yrep)
  cols <- grep("^y_rep\\[", names(yrep_df), value = TRUE)
  as.matrix(yrep_df[, cols, drop = FALSE])
}

ppc_stats <- function(y_obs, yrep_mat, subj_index = NULL, J = NULL) {
  stat_obs_mean <- mean(y_obs)
  stat_rep_mean <- rowMeans(yrep_mat)
  
  out <- list(
    obs_mean = stat_obs_mean,
    rep_mean = stat_rep_mean,
    p_bayes_mean = mean(stat_rep_mean >= stat_obs_mean)
  )
  
  if (!is.null(subj_index) && !is.null(J)) {
    idx_list <- split(seq_along(y_obs), subj_index)
    stat_obs_maxj <- max(sapply(idx_list, function(ii) sum(y_obs[ii])))
    stat_rep_maxj <- apply(yrep_mat, 1, function(rr) max(sapply(idx_list, function(ii) sum(rr[ii]))))
    
    out$obs_max_subj_k <- stat_obs_maxj
    out$rep_max_subj_k <- stat_rep_maxj
    out$p_bayes_max_subj_k <- mean(stat_rep_maxj >= stat_obs_maxj)
  }
  
  out
}

# -----------------------------
# 8) Prior Predictive Checks (prior checks)
# -----------------------------
yrep_pooled_prior <- extract_yrep_matrix(fit_pooled_prior)
yrep_hier_prior <- extract_yrep_matrix(fit_hier_prior)

ppc_pooled_prior <- ppc_stats(df$y, yrep_pooled_prior)
ppc_hier_prior <- ppc_stats(df$y, yrep_hier_prior, subj_index = df$subj, J = J)

df_prior_mean <- tibble(mean_rep = ppc_hier_prior$rep_mean)
p_prior_mean <- ggplot(df_prior_mean, aes(x = mean_rep)) +
  geom_histogram(bins = 40) +
  geom_vline(xintercept = mean(df$y), linetype = 2) +
  labs(title = "Prior predictive: distribuição da taxa global (média de y_rep)",
       x = "média(y_rep)", y = "freq")
print(p_prior_mean)

# -----------------------------
# 9) Posterior Predictive Checks (PPC)
# -----------------------------
yrep_pooled_post <- extract_yrep_matrix(fit_pooled)
yrep_hier_post <- extract_yrep_matrix(fit_hier)

ppc_pooled_post <- ppc_stats(df$y, yrep_pooled_post)
ppc_hier_post <- ppc_stats(df$y, yrep_hier_post, subj_index = df$subj, J = J)

df_post_mean <- tibble(mean_rep = ppc_hier_post$rep_mean)
p_post_mean <- ggplot(df_post_mean, aes(x = mean_rep)) +
  geom_histogram(bins = 40) +
  geom_vline(xintercept = mean(df$y), linetype = 2) +
  labs(title = "Posterior predictive: distribuição da taxa global (média de y_rep)",
       x = "média(y_rep)", y = "freq")
print(p_post_mean)

# -----------------------------
# 10) Diagnósticos MCMC
# -----------------------------
diag_pooled <- sampler_diag_summary(fit_pooled)
diag_hier <- sampler_diag_summary(fit_hier)

sum_pooled_main <- summ_draws(fit_pooled, c("theta","alpha"))
sum_hier_main   <- summ_draws(fit_hier, c("theta_pop","alpha","sigma"))
sum_hier_subj   <- summ_draws(fit_hier, "theta_subj")

draws_hier <- posterior::as_draws_df(fit_hier$draws(c("theta_pop","sigma")))
p_theta_pop_gt_05 <- mean(draws_hier$theta_pop > 0.5)

draws_theta_subj <- posterior::as_draws_df(fit_hier$draws("theta_subj"))
theta_cols <- grep("^theta_subj\\[", names(draws_theta_subj), value = TRUE)
p_subj_gt_05 <- sapply(theta_cols, function(cc) mean(draws_theta_subj[[cc]] > 0.5))

subj_levels <- levels(df$s)
subj_table <- tibble(
  s = subj_levels,
  n = df_by$n[match(subj_levels, df_by$s)],
  k = df_by$k[match(subj_levels, df_by$s)],
  prop_obs = df_by$prop[match(subj_levels, df_by$s)],
  p_theta_gt_05 = as.numeric(p_subj_gt_05)
) |>
  arrange(desc(p_theta_gt_05))

# -----------------------------
# 11) LOO (comparação pooled vs hierárquico)
# -----------------------------
loo_pooled <- fit_pooled$loo()
loo_hier <- fit_hier$loo()
loo_cmp <- loo::loo_compare(loo_pooled, loo_hier)

# -----------------------------
# 12) RELATÓRIO FINAL (colável)
# -----------------------------
report <- c()

report <- c(report, "==================== RELATORIO FINAL ====================")
report <- c(report, sprintf("Arquivo: %s", file_path))
report <- c(report, sprintf("N observacoes = %d | J sujeitos = %d", N, J))
report <- c(report, sprintf("Total de sucessos (y=1) = %d | Taxa observada = %.3f",
                            k_total, mean(df$y)))

report <- c(report, "\n--- Resumo por sujeito (observado):")
df_by_ord <- df_by |> arrange(prop)
bottom5 <- head(df_by_ord, 5)
top5 <- tail(df_by_ord, 5)

fmt_block <- function(dd) {
  paste0(apply(dd, 1, function(r) sprintf("  %s: k=%d/%d (%.2f)",
                                          r[["s"]], as.integer(r[["k"]]), as.integer(r[["n"]]), as.numeric(r[["prop"]]))),
         collapse = "\n")
}
report <- c(report, "Bottom 5:")
report <- c(report, fmt_block(bottom5))
report <- c(report, "Top 5:")
report <- c(report, fmt_block(top5))

report <- c(report, "\n--- Referencia frequentista (binom.test, p=0.5):")
report <- c(report, sprintf("  p-valor (2-sided) = %.6f", binom_test$p.value))
report <- c(report, sprintf("  IC95%% (freq) para p = [%.3f, %.3f]", binom_test$conf.int[1], binom_test$conf.int[2]))

report <- c(report, "\n--- Diagnosticos (pooled):")
report <- c(report, sprintf("  divergencias = %d | max treedepth visto = %d",
                            diag_pooled$divergences, diag_pooled$max_treedepth_seen))
report <- c(report, "\nResumo posterior (pooled):")
report <- c(report, capture.output(print(sum_pooled_main, n = Inf)))

report <- c(report, "\n--- Diagnosticos (hierarquico):")
report <- c(report, sprintf("  divergencias = %d | max treedepth visto = %d",
                            diag_hier$divergences, diag_hier$max_treedepth_seen))
report <- c(report, "\nResumo posterior (hierarquico):")
report <- c(report, capture.output(print(sum_hier_main, n = Inf)))
report <- c(report, sprintf("\n  P(theta_pop > 0.5) = %.3f", p_theta_pop_gt_05))

report <- c(report, "\n--- PPC / Prior check (pooled):")
report <- c(report, sprintf("  Obs mean(y)=%.3f | Bayes p-value (mean) = %.3f",
                            ppc_pooled_prior$obs_mean, ppc_pooled_prior$p_bayes_mean))

report <- c(report, "\n--- PPC / Prior check (hierarquico):")
report <- c(report, sprintf("  Obs mean(y)=%.3f | Bayes p-value (mean) = %.3f",
                            ppc_hier_prior$obs_mean, ppc_hier_prior$p_bayes_mean))
report <- c(report, sprintf("  Obs max subj successes=%d | Bayes p-value (max subj) = %.3f",
                            ppc_hier_prior$obs_max_subj_k, ppc_hier_prior$p_bayes_max_subj_k))

report <- c(report, "\n--- PPC / Posterior check (pooled):")
report <- c(report, sprintf("  Obs mean(y)=%.3f | Bayes p-value (mean) = %.3f",
                            ppc_pooled_post$obs_mean, ppc_pooled_post$p_bayes_mean))

report <- c(report, "\n--- PPC / Posterior check (hierarquico):")
report <- c(report, sprintf("  Obs mean(y)=%.3f | Bayes p-value (mean) = %.3f",
                            ppc_hier_post$obs_mean, ppc_hier_post$p_bayes_mean))
report <- c(report, sprintf("  Obs max subj successes=%d | Bayes p-value (max subj) = %.3f",
                            ppc_hier_post$obs_max_subj_k, ppc_hier_post$p_bayes_max_subj_k))

report <- c(report, "\n--- Ranking sujeitos (hier): P(theta_subj > 0.5) (top 10):")
report <- c(report, capture.output(print(head(subj_table, 10), n = Inf)))

report <- c(report, "\n--- Comparacao LOO (pooled vs hier):")
report <- c(report, capture.output(print(loo_cmp)))

report <- c(report, "\n--- Session info:")
report <- c(report, capture.output(print(sessionInfo())))

report <- c(report, "================== FIM RELATORIO FINAL ==================")

cat(paste0(report, collapse = "\n"))

# ============================================================
# FIM
# ============================================================
