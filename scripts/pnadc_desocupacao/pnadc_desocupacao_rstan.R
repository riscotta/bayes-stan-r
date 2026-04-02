#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

############################################################
# PNAD Contínua — Desocupação
# Manchete-alvo: 5,4% no trimestre móvel encerrado em jan/2026
# Padrão do repositório:
# - rodar a partir do root do repositório
# - sem setwd()
# - entrada padrão em data/raw/
# - resultados finais em console por padrão
# - sem gravação de saídas analíticas em disco
#
# Como rodar:
#   Rscript scripts/pnadc_desocupacao/pnadc_desocupacao_rstan.R
#
# Exemplos:
#   Rscript scripts/pnadc_desocupacao/pnadc_desocupacao_rstan.R
#   Rscript scripts/pnadc_desocupacao/pnadc_desocupacao_rstan.R --modo_debug=0
#   Rscript scripts/pnadc_desocupacao/pnadc_desocupacao_rstan.R \
#     --input_dir=data/raw/pnadc_desocupacao \
#     --file_mensal=pnadc_mensal_taxa_desocupacao_6381.csv \
#     --file_sexo=pnadc_trimestral_taxa_desocupacao_por_sexo_4093.csv
############################################################

args <- commandArgs(trailingOnly = TRUE)
AUTO_INSTALL_PKGS <- "--auto-install" %in% args

parse_args <- function(args) {
  out <- list(
    input_dir = file.path("data", "raw", "pnadc_desocupacao"),
    file_mensal = "pnadc_mensal_taxa_desocupacao_6381.csv",
    file_jan26 = "pnadc_mensal_taxa_desocupacao_jan_2026.csv",
    file_sexo = "pnadc_trimestral_taxa_desocupacao_por_sexo_4093.csv",

    seed_model = 20260402L,
    modo_debug = 1L,

    chains = NA_integer_,
    iter = NA_integer_,
    warmup = NA_integer_,
    adapt_delta = NA_real_,
    max_treedepth = NA_integer_,
    cores = NA_integer_,
    refresh = NA_integer_
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

  out$seed_model <- as.integer(out$seed_model)
  out$modo_debug <- as.integer(out$modo_debug)

  if (!is.na(out$chains)) out$chains <- as.integer(out$chains)
  if (!is.na(out$iter)) out$iter <- as.integer(out$iter)
  if (!is.na(out$warmup)) out$warmup <- as.integer(out$warmup)
  if (!is.na(out$adapt_delta)) out$adapt_delta <- as.numeric(out$adapt_delta)
  if (!is.na(out$max_treedepth)) out$max_treedepth <- as.integer(out$max_treedepth)
  if (!is.na(out$cores)) out$cores <- as.integer(out$cores)
  if (!is.na(out$refresh)) out$refresh <- as.integer(out$refresh)

  debug_defaults <- list(
    chains = 2L,
    iter = 1000L,
    warmup = 500L,
    adapt_delta = 0.90,
    max_treedepth = 10L,
    cores = 1L,
    refresh = 50L
  )

  final_defaults <- list(
    chains = 4L,
    iter = 2000L,
    warmup = 1000L,
    adapt_delta = 0.95,
    max_treedepth = 12L,
    cores = max(1L, parallel::detectCores(logical = TRUE) - 1L),
    refresh = 100L
  )

  defaults <- if (isTRUE(out$modo_debug == 1L)) debug_defaults else final_defaults

  for (nm in names(defaults)) {
    if (is.na(out[[nm]])) out[[nm]] <- defaults[[nm]]
  }

  if (out$warmup >= out$iter) {
    stop("'warmup' deve ser menor que 'iter'.", call. = FALSE)
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
        "  Rscript scripts/pnadc_desocupacao/pnadc_desocupacao_rstan.R --auto-install\n",
        call. = FALSE
      )
    }
    message("Instalando pacote ausente: ", pkg)
    install.packages(pkg, dependencies = TRUE)
  }
  invisible(TRUE)
}

pkgs <- c("dplyr", "tidyr", "stringr", "rstan")
invisible(lapply(pkgs, ensure_pkg))

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(rstan)
})

opts <- parse_args(args)

options(mc.cores = opts$cores)
rstan_options(auto_write = FALSE)

stopf <- function(...) stop(sprintf(...), call. = FALSE)

parse_br_num <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("\\.", "", x, fixed = FALSE)
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

safe_qlogis_pct <- function(x_pct, eps = 1e-6) {
  p <- x_pct / 100
  p <- pmin(pmax(p, eps), 1 - eps)
  qlogis(p)
}

fmt <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits, decimal.mark = ",")
}

summ_vec <- function(x, probs = c(0.025, 0.5, 0.975)) {
  c(
    media   = mean(x),
    dp      = sd(x),
    q025    = unname(quantile(x, probs[1])),
    mediana = unname(quantile(x, probs[2])),
    q975    = unname(quantile(x, probs[3]))
  )
}

print_summ <- function(label, x, digits = 2) {
  s <- summ_vec(x)
  cat(
    label, "\n",
    "  média   :", fmt(s["media"], digits), "\n",
    "  dp      :", fmt(s["dp"], digits), "\n",
    "  mediana :", fmt(s["mediana"], digits), "\n",
    "  IC95%   : [", fmt(s["q025"], digits), "; ", fmt(s["q975"], digits), "]\n",
    sep = ""
  )
}

read_csv_pnad <- function(path) {
  if (!file.exists(path)) stopf("Arquivo não encontrado: %s", path)
  read.csv(
    path,
    sep = ";",
    header = TRUE,
    encoding = "UTF-8",
    stringsAsFactors = FALSE,
    check.names = TRUE
  )
}

assert_has_cols <- function(df, cols, nome_df) {
  miss <- setdiff(cols, names(df))
  if (length(miss) > 0) {
    stopf(
      "Faltam colunas em %s: %s",
      nome_df,
      paste(miss, collapse = ", ")
    )
  }
}

qcode_to_month_code <- function(qcode) {
  qcode <- as.integer(qcode)
  ano   <- qcode %/% 100
  tri   <- qcode %% 100
  if (any(!tri %in% 1:4)) stopf("Código trimestral inválido encontrado.")
  ano * 100 + tri * 3
}

file_mensal <- file.path(opts$input_dir, opts$file_mensal)
file_jan26  <- file.path(opts$input_dir, opts$file_jan26)
file_sexo   <- file.path(opts$input_dir, opts$file_sexo)

cat("\n============================================================\n")
cat("PNAD Contínua — Desocupação (rstan)\n")
cat("============================================================\n")
cat("Input dir    :", opts$input_dir, "\n")
cat("Arquivo mensal:", file_mensal, "\n")
cat("Arquivo sexo :", file_sexo, "\n")
cat("Arquivo jan26:", file_jan26, "\n")
cat("modo_debug   :", opts$modo_debug, "\n")
cat("chains       :", opts$chains, "\n")
cat("iter         :", opts$iter, "\n")
cat("warmup       :", opts$warmup, "\n")
cat("cores        :", opts$cores, "\n")
cat("refresh      :", opts$refresh, "\n")
cat("============================================================\n\n")

# ------------------------------------------------------------
# 1) Leitura e padronização
# ------------------------------------------------------------
mensal <- read_csv_pnad(file_mensal)
assert_has_cols(
  mensal,
  c("periodo_codigo", "periodo", "variavel", "taxa_desocupacao"),
  "mensal"
)

mensal <- mensal %>%
  transmute(
    periodo_codigo = as.integer(periodo_codigo),
    periodo        = trimws(periodo),
    variavel       = trimws(variavel),
    taxa_pp        = parse_br_num(taxa_desocupacao)
  ) %>%
  arrange(periodo_codigo)

if (anyNA(mensal$periodo_codigo)) stopf("Há NA em periodo_codigo na base mensal.")
if (anyNA(mensal$taxa_pp)) stopf("Há NA em taxa_pp na base mensal.")
if (anyDuplicated(mensal$periodo_codigo) > 0) {
  dup <- mensal$periodo_codigo[duplicated(mensal$periodo_codigo)]
  stopf("Há períodos duplicados na base mensal: %s", paste(unique(dup), collapse = ", "))
}

# Arquivo específico de jan/2026: usado apenas como checagem de consistência
jan26_check_exists <- file.exists(file_jan26)
jan26_match_ok <- NA

if (jan26_check_exists) {
  mensal_jan <- read_csv_pnad(file_jan26)
  assert_has_cols(
    mensal_jan,
    c("periodo_codigo", "periodo", "variavel", "taxa_desocupacao"),
    "mensal_jan26"
  )

  mensal_jan <- mensal_jan %>%
    transmute(
      periodo_codigo = as.integer(periodo_codigo),
      periodo        = trimws(periodo),
      taxa_pp        = parse_br_num(taxa_desocupacao)
    )

  if (nrow(mensal_jan) != 1L) {
    stopf("O arquivo mensal de jan/2026 deveria ter exatamente 1 linha.")
  }

  if (mensal_jan$periodo_codigo != 202601L) {
    stopf("O arquivo de checagem de jan/2026 não contém o período 202601.")
  }

  if (202601L %in% mensal$periodo_codigo) {
    taxa_main <- mensal$taxa_pp[mensal$periodo_codigo == 202601L]
    jan26_match_ok <- isTRUE(all.equal(as.numeric(taxa_main), as.numeric(mensal_jan$taxa_pp), tolerance = 1e-12))
    if (!jan26_match_ok) {
      stopf(
        "Inconsistência entre a base mensal principal e o arquivo específico de jan/2026: %.3f vs %.3f",
        taxa_main, mensal_jan$taxa_pp
      )
    }
  } else {
    mensal <- bind_rows(
      mensal,
      mensal_jan %>%
        mutate(variavel = "Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade") %>%
        select(periodo_codigo, periodo, variavel, taxa_pp)
    ) %>%
      arrange(periodo_codigo)
    jan26_match_ok <- TRUE
  }
}

sexo_tri <- read_csv_pnad(file_sexo)
assert_has_cols(
  sexo_tri,
  c("periodo_codigo", "periodo", "sexo", "variavel", "taxa_desocupacao"),
  "sexo_tri"
)

sexo_tri <- sexo_tri %>%
  transmute(
    periodo_codigo = as.integer(periodo_codigo),
    periodo        = trimws(periodo),
    sexo           = trimws(sexo),
    variavel       = trimws(variavel),
    taxa_pp        = parse_br_num(taxa_desocupacao)
  ) %>%
  arrange(periodo_codigo, sexo)

if (anyNA(sexo_tri$periodo_codigo)) stopf("Há NA em periodo_codigo na base trimestral por sexo.")
if (anyNA(sexo_tri$taxa_pp)) stopf("Há NA em taxa_pp na base trimestral por sexo.")

if (anyDuplicated(sexo_tri[, c("periodo_codigo", "sexo")]) > 0) {
  stopf("Há combinações duplicadas de periodo_codigo x sexo na base trimestral.")
}

# ------------------------------------------------------------
# 2) Validações de coerência
# ------------------------------------------------------------
if (!all(diff(mensal$periodo_codigo) >= 1)) {
  stopf("A base mensal não está ordenada corretamente.")
}

tri_total <- sexo_tri %>%
  filter(sexo == "Total") %>%
  mutate(month_code = qcode_to_month_code(periodo_codigo)) %>%
  select(periodo_codigo, month_code, taxa_pp)

mensal_qend <- mensal %>%
  filter((periodo_codigo %% 100) %in% c(3, 6, 9, 12)) %>%
  select(periodo_codigo, taxa_pp)

chk_tri_total <- tri_total %>%
  left_join(mensal_qend, by = c("month_code" = "periodo_codigo"), suffix = c("_tri", "_mensal"))

if (anyNA(chk_tri_total$taxa_pp_mensal)) {
  stopf("Nem todo trimestre do arquivo por sexo encontrou correspondência na série mensal.")
}

max_abs_diff_tri_total <- max(abs(chk_tri_total$taxa_pp_tri - chk_tri_total$taxa_pp_mensal))
if (max_abs_diff_tri_total > 1e-12) {
  stopf("O total trimestral não bate com a série mensal nos finais de trimestre. Máx. dif.: %.6f", max_abs_diff_tri_total)
}

sex_panel <- sexo_tri %>%
  filter(sexo %in% c("Homens", "Mulheres")) %>%
  select(periodo_codigo, sexo, taxa_pp) %>%
  tidyr::pivot_wider(names_from = sexo, values_from = taxa_pp) %>%
  arrange(periodo_codigo)

assert_has_cols(sex_panel, c("periodo_codigo", "Homens", "Mulheres"), "sex_panel")

if (anyNA(sex_panel$Homens) || anyNA(sex_panel$Mulheres)) {
  stopf("Há NA na base trimestral wide por sexo.")
}

sex_panel <- sex_panel %>%
  mutate(
    month_code = qcode_to_month_code(periodo_codigo),
    t_index    = match(month_code, mensal$periodo_codigo)
  )

if (anyNA(sex_panel$t_index)) {
  stopf("Falha ao mapear trimestres por sexo para a série mensal.")
}

# ------------------------------------------------------------
# 3) Base analítica para Stan
# ------------------------------------------------------------
mensal <- mensal %>%
  mutate(
    month_end = periodo_codigo %% 100,
    y_logit   = safe_qlogis_pct(taxa_pp)
  )

sex_panel <- sex_panel %>%
  mutate(
    y_male_logit   = safe_qlogis_pct(Homens),
    y_female_logit = safe_qlogis_pct(Mulheres)
  )

stan_data <- list(
  T              = nrow(mensal),
  y_total_logit  = as.vector(mensal$y_logit),
  month_id       = as.integer(mensal$month_end),
  Q              = nrow(sex_panel),
  tq             = as.integer(sex_panel$t_index),
  y_male_logit   = as.vector(sex_panel$y_male_logit),
  y_female_logit = as.vector(sex_panel$y_female_logit)
)

idx_jan26 <- match(202601L, mensal$periodo_codigo)
idx_oct25 <- match(202510L, mensal$periodo_codigo)
idx_jan25 <- match(202501L, mensal$periodo_codigo)

if (anyNA(c(idx_jan26, idx_oct25, idx_jan25))) {
  stopf("Não foi possível localizar os períodos-chave da manchete na série mensal.")
}

# ------------------------------------------------------------
# 4) Modelo Stan
# ------------------------------------------------------------
stan_code <- "
data {
  int<lower=1> T;
  vector[T] y_total_logit;
  array[T] int<lower=1, upper=12> month_id;

  int<lower=1> Q;
  array[Q] int<lower=1, upper=T> tq;
  vector[Q] y_male_logit;
  vector[Q] y_female_logit;
}
parameters {
  real level0;
  vector[T-1] z_level;
  real<lower=0> sigma_level;

  vector[12] season_raw;
  real<lower=0> sigma_season;

  real<lower=0> sigma_obs_total;
  real<lower=0> sigma_obs_sex;

  real gap0_m;
  real gap0_f;
  vector[Q-1] z_gap_m;
  vector[Q-1] z_gap_f;
  real<lower=0> sigma_gap_m;
  real<lower=0> sigma_gap_f;
}
transformed parameters {
  vector[T] level;
  vector[12] season;
  vector[T] eta_total;
  vector[Q] gap_m;
  vector[Q] gap_f;

  season = sigma_season * (season_raw - mean(season_raw));

  level[1] = level0;
  for (t in 2:T) {
    level[t] = level[t-1] + sigma_level * z_level[t-1];
  }

  for (t in 1:T) {
    eta_total[t] = level[t] + season[month_id[t]];
  }

  gap_m[1] = gap0_m;
  gap_f[1] = gap0_f;
  for (q in 2:Q) {
    gap_m[q] = gap_m[q-1] + sigma_gap_m * z_gap_m[q-1];
    gap_f[q] = gap_f[q-1] + sigma_gap_f * z_gap_f[q-1];
  }
}
model {
  level0          ~ normal(logit(0.08), 0.60);
  z_level         ~ normal(0, 1);
  sigma_level     ~ exponential(12);

  season_raw      ~ normal(0, 1);
  sigma_season    ~ exponential(8);

  sigma_obs_total ~ exponential(30);
  sigma_obs_sex   ~ exponential(30);

  gap0_m          ~ normal(0, 0.50);
  gap0_f          ~ normal(0, 0.50);
  z_gap_m         ~ normal(0, 1);
  z_gap_f         ~ normal(0, 1);
  sigma_gap_m     ~ exponential(12);
  sigma_gap_f     ~ exponential(12);

  y_total_logit ~ normal(eta_total, sigma_obs_total);

  for (q in 1:Q) {
    y_male_logit[q]   ~ normal(eta_total[tq[q]] + gap_m[q], sigma_obs_sex);
    y_female_logit[q] ~ normal(eta_total[tq[q]] + gap_f[q], sigma_obs_sex);
  }
}
generated quantities {
  vector[T] p_total;
  vector[Q] p_male;
  vector[Q] p_female;
  vector[Q] gap_fm_pp;

  for (t in 1:T) {
    p_total[t] = 100 * inv_logit(eta_total[t]);
  }

  for (q in 1:Q) {
    p_male[q]    = 100 * inv_logit(eta_total[tq[q]] + gap_m[q]);
    p_female[q]  = 100 * inv_logit(eta_total[tq[q]] + gap_f[q]);
    gap_fm_pp[q] = p_female[q] - p_male[q];
  }
}
"

# ------------------------------------------------------------
# 5) Compilação + ajuste
# ------------------------------------------------------------
cat("\n============================================================\n")
cat("[STAN] Iniciando compilação do modelo...\n")
cat("============================================================\n")

start_compile <- Sys.time()
sm <- rstan::stan_model(
  model_code = stan_code,
  verbose = TRUE
)
end_compile <- Sys.time()

cat("\n============================================================\n")
cat("[STAN] Compilação concluída.\n")
cat("Tempo de compilação:", round(as.numeric(difftime(end_compile, start_compile, units = "secs")), 2), "segundos\n")
cat("[STAN] Iniciando MCMC...\n")
cat("modo_debug  :", opts$modo_debug, "\n")
cat("chains      :", opts$chains, "\n")
cat("iter        :", opts$iter, "\n")
cat("warmup      :", opts$warmup, "\n")
cat("cores       :", opts$cores, "\n")
cat("refresh     :", opts$refresh, "\n")
cat("adapt_delta :", opts$adapt_delta, "\n")
cat("max_td      :", opts$max_treedepth, "\n")
cat("============================================================\n\n")

start_sampling <- Sys.time()
fit <- rstan::sampling(
  object  = sm,
  data    = stan_data,
  seed    = opts$seed_model,
  chains  = opts$chains,
  iter    = opts$iter,
  warmup  = opts$warmup,
  cores   = opts$cores,
  refresh = opts$refresh,
  control = list(
    adapt_delta   = opts$adapt_delta,
    max_treedepth = opts$max_treedepth
  )
)
end_sampling <- Sys.time()

cat("\n============================================================\n")
cat("[STAN] MCMC concluído.\n")
cat("Tempo de amostragem:", round(as.numeric(difftime(end_sampling, start_sampling, units = "secs")), 2), "segundos\n")
cat("Tempo total ajuste :", round(as.numeric(difftime(end_sampling, start_compile, units = "secs")), 2), "segundos\n")
cat("============================================================\n\n")

cat("[CHECK] Resumo rápido dos parâmetros principais:\n")
print(
  fit,
  pars = c(
    "level0",
    "sigma_level",
    "sigma_season",
    "sigma_obs_total",
    "sigma_obs_sex",
    "sigma_gap_m",
    "sigma_gap_f"
  ),
  probs = c(0.025, 0.5, 0.975)
)
cat("\n")

# ------------------------------------------------------------
# 6) Diagnóstico
# ------------------------------------------------------------
fit_sum <- summary(fit)$summary
max_rhat <- max(fit_sum[, "Rhat"], na.rm = TRUE)
min_neff <- min(fit_sum[, "n_eff"], na.rm = TRUE)

sampler_params <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
n_divergent <- sum(vapply(sampler_params, function(x) sum(x[, "divergent__"]), numeric(1)))
n_td_sat    <- sum(vapply(sampler_params, function(x) sum(x[, "treedepth__"] >= opts$max_treedepth), numeric(1)))

# ------------------------------------------------------------
# 7) Extração posterior
# ------------------------------------------------------------
post <- rstan::extract(fit, pars = c("p_total", "p_male", "p_female", "gap_fm_pp"))

p_total  <- post$p_total
p_male   <- post$p_male
p_female <- post$p_female
gap_fm   <- post$gap_fm_pp

p_jan26 <- p_total[, idx_jan26]
p_oct25 <- p_total[, idx_oct25]
p_jan25 <- p_total[, idx_jan25]

diff_prev <- p_jan26 - p_oct25
diff_yoy  <- p_jan26 - p_jan25

stable_band_pp <- 0.30
pr_stable_prev <- mean(abs(diff_prev) <= stable_band_pp)
pr_down_yoy    <- mean(diff_yoy < 0)
pr_down_1pp    <- mean(diff_yoy <= -1.0)
pr_down_11pp   <- mean(diff_yoy <= -1.1)

q_last <- nrow(sex_panel)
male_last   <- p_male[, q_last]
female_last <- p_female[, q_last]
gap_last    <- gap_fm[, q_last]
pr_f_gt_m   <- mean(gap_last > 0)

fitted_total_mean <- colMeans(p_total)
rmse_total <- sqrt(mean((fitted_total_mean - mensal$taxa_pp)^2))

# ------------------------------------------------------------
# 8) Valores observados da manchete
# ------------------------------------------------------------
raw_jan26 <- mensal$taxa_pp[idx_jan26]
raw_oct25 <- mensal$taxa_pp[idx_oct25]
raw_jan25 <- mensal$taxa_pp[idx_jan25]

# ------------------------------------------------------------
# 9) Relatório final — console
# ------------------------------------------------------------
cat("============================================================\n")
cat("RELATÓRIO FINAL — PNAD CONTÍNUA / DESOCUPAÇÃO (1 modelo STAN)\n")
cat("============================================================\n\n")

cat("[1] Base analítica\n")
cat("Série mensal total (trimestre móvel):", nrow(mensal), "observações\n")
cat("Período mensal:", min(mensal$periodo_codigo), "a", max(mensal$periodo_codigo), "\n")
cat("Série trimestral por sexo:", nrow(sex_panel), "trimestres\n")
cat("Período trimestral:", min(sex_panel$periodo_codigo), "a", max(sex_panel$periodo_codigo), "\n")
cat("Checagem total trimestral vs mensal (fim de trimestre), máx. dif. abs.:", fmt(max_abs_diff_tri_total, 6), "\n")
cat("Arquivo específico jan/2026 presente:", ifelse(jan26_check_exists, "sim", "não"), "\n")
cat("Arquivo específico jan/2026 consistente com a base principal:", ifelse(isTRUE(jan26_match_ok), "sim", ifelse(is.na(jan26_match_ok), "não aplicável", "não")), "\n")
cat("\n")

cat("[2] Observados oficiais\n")
cat("Nov-Dez-Jan/2026 (202601):", fmt(raw_jan26), "%\n")
cat("Ago-Set-Out/2025 (202510):", fmt(raw_oct25), "%\n")
cat("Nov-Dez-Jan/2025 (202501):", fmt(raw_jan25), "%\n")
cat("Diferença observada vs período comparável anterior:", fmt(raw_jan26 - raw_oct25), "p.p.\n")
cat("Diferença observada vs mesmo trimestre móvel do ano anterior:", fmt(raw_jan26 - raw_jan25), "p.p.\n")
cat("\n")

cat("[3] Diagnóstico do Stan\n")
cat("Rhat máximo :", fmt(max_rhat, 3), "\n")
cat("n_eff mínimo:", fmt(min_neff, 0), "\n")
cat("Divergências:", n_divergent, "\n")
cat("Treedepth saturado:", n_td_sat, "\n")
cat("RMSE série observada vs média posterior latente:", fmt(rmse_total, 3), "p.p.\n")
cat("\n")

cat("[4] Resultado bayesiano — manchete principal\n")
print_summ("Taxa latente total — nov-dez-jan/2026 (%)", p_jan26, digits = 2)
print_summ("Diferença — jan/2026 menos out/2025 (p.p.)", diff_prev, digits = 3)
cat(
  "Probabilidade de estabilidade prática frente ao período anterior (|dif| <= ",
  fmt(stable_band_pp, 2),
  " p.p.): ",
  fmt(100 * pr_stable_prev, 1),
  "%\n",
  sep = ""
)
print_summ("Diferença — jan/2026 menos jan/2025 (p.p.)", diff_yoy, digits = 3)
cat("Probabilidade de queda vs ano anterior:", fmt(100 * pr_down_yoy, 1), "%\n")
cat("Probabilidade de queda >= 1,0 p.p. vs ano anterior:", fmt(100 * pr_down_1pp, 1), "%\n")
cat("Probabilidade de queda >= 1,1 p.p. vs ano anterior:", fmt(100 * pr_down_11pp, 1), "%\n")
cat("\n")

cat("[5] Heterogeneidade por sexo — último trimestre disponível (4T2025)\n")
cat("Trimestre:", max(sex_panel$periodo_codigo), "\n")
print_summ("Homens (%)", male_last, digits = 2)
print_summ("Mulheres (%)", female_last, digits = 2)
print_summ("Gap mulheres - homens (p.p.)", gap_last, digits = 3)
cat("Probabilidade de taxa feminina > taxa masculina:", fmt(100 * pr_f_gt_m, 1), "%\n")
cat("\n")

cat("[6] Interpretação prática\n")
if (pr_stable_prev >= 0.80) {
  cat("- A leitura de estabilidade frente ao trimestre móvel comparável anterior está bem sustentada pelo modelo.\n")
} else if (pr_stable_prev >= 0.60) {
  cat("- Há suporte moderado para estabilidade frente ao trimestre móvel comparável anterior, mas não é uma conclusão muito forte.\n")
} else {
  cat("- O modelo não sustenta com muita força a ideia de estabilidade frente ao trimestre móvel comparável anterior.\n")
}

if (pr_down_yoy >= 0.95) {
  cat("- A queda frente ao mesmo trimestre móvel do ano anterior está fortemente sustentada.\n")
} else if (pr_down_yoy >= 0.80) {
  cat("- Há evidência relevante de queda frente ao mesmo trimestre móvel do ano anterior.\n")
} else {
  cat("- A evidência de queda frente ao mesmo trimestre móvel do ano anterior é fraca ou moderada.\n")
}

if (pr_f_gt_m >= 0.95) {
  cat("- A heterogeneidade por sexo é clara: a taxa feminina permanece acima da masculina no último trimestre disponível.\n")
} else if (pr_f_gt_m >= 0.80) {
  cat("- Há evidência importante de taxa feminina acima da masculina, embora com alguma incerteza.\n")
} else {
  cat("- A diferença por sexo no último trimestre disponível não ficou fortemente definida no modelo.\n")
}
cat("\n")

cat("[7] Limitação metodológica\n")
cat("- Este estudo modela as taxas oficiais publicadas como medidas ruidosas de uma trajetória latente.\n")
cat("- Ele não substitui uma inferência com microdados da PNAD Contínua, pesos amostrais e desenho complexo.\n")
cat("- Para manchete e robustez temporal, este desenho é adequado; para inferência populacional fina, o ideal é partir dos microdados.\n")
cat("\n")

cat("============================================================\n")
cat("FIM\n")
cat("============================================================\n")
