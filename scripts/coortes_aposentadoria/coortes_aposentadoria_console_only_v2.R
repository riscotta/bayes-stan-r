#!/usr/bin/env Rscript

############################################################
# Coortes de Aposentadoria (30 anos) — Brasil (CONSOLE ONLY)
#
# Ideia:
#   - Mesma pessoa, mesmo aporte mensal real
#   - Mesma janela de contribuição: 30 anos (360 meses)
#   - Só muda a data de aposentadoria (fim da janela) => muda a sequência de retornos
#
# Dados (mensal):
#   - Ações: Ibovespa (^BVSP, Yahoo Finance, Adj Close)
#   - RF: Selic mensal (SGS 4390)
#   - Inflação: IPCA (SGS 433) -> converte tudo para retorno REAL
#
# Estratégias:
#   - 100% ações
#   - 100% RF
#   - Mix fixo (60/40, 50/50, 40/60)
#   - Glide path (linear e por degraus)
#
# Importante:
#   - NÃO grava CSV/TXT/PNG/PDF.
#   - Resultados são impressos no console ao FINAL.
#
# Rode a partir do ROOT do repo:
#   Rscript scripts/coortes_aposentadoria/coortes_aposentadoria_console_only_v2.R
#
# Opções (formato --chave=valor):
#   --ticker_eq=^BVSP
#   --from_date=1990-01-01
#   --to_date=YYYY-MM-DD  (padrão: Sys.Date())
#
#   --work_years=30
#   --contrib_real=1
#
#   --w_mix_60_40=0.60
#   --w_mix_50_50=0.50
#   --w_mix_40_60=0.40
#
#   --tdf_lin_w_start=0.90
#   --tdf_lin_w_end=0.50
#
#   --tdf_step_w_a=0.70
#   --tdf_step_w_b=0.50
#   --tdf_step_w_c=0.30
#   --tdf_step_y_a=20
#   --tdf_step_y_b=25
#   --tdf_step_y_c=30
#
#   --do_sequence_diagnostics=0|1
#   --end_years=5
#   --seq_strategies=eq_100,tdf_step
#
#   --show_plots=0|1
#   --seed=123
############################################################

options(stringsAsFactors = FALSE)

# ----------------------------
# 0) Helpers (args + deps)
# ----------------------------
parse_args <- function(args) {
  out <- list(
    ticker_eq = "^BVSP",
    from_date = "1990-01-01",
    to_date   = as.character(Sys.Date()),

    work_years   = 30L,
    contrib_real = 1.0,

    w_mix_60_40 = 0.60,
    w_mix_50_50 = 0.50,
    w_mix_40_60 = 0.40,

    tdf_lin_w_start = 0.90,
    tdf_lin_w_end   = 0.50,

    tdf_step_w_a = 0.70,
    tdf_step_w_b = 0.50,
    tdf_step_w_c = 0.30,
    tdf_step_y_a = 20L,
    tdf_step_y_b = 25L,
    tdf_step_y_c = 30L,

    do_sequence_diagnostics = 1L,
    end_years = 5L,
    seq_strategies = "eq_100,tdf_step",

    show_plots = 0L,
    seed = 123L
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
  out$work_years <- as.integer(out$work_years)
  out$contrib_real <- as.numeric(out$contrib_real)

  out$w_mix_60_40 <- as.numeric(out$w_mix_60_40)
  out$w_mix_50_50 <- as.numeric(out$w_mix_50_50)
  out$w_mix_40_60 <- as.numeric(out$w_mix_40_60)

  out$tdf_lin_w_start <- as.numeric(out$tdf_lin_w_start)
  out$tdf_lin_w_end   <- as.numeric(out$tdf_lin_w_end)

  out$tdf_step_w_a <- as.numeric(out$tdf_step_w_a)
  out$tdf_step_w_b <- as.numeric(out$tdf_step_w_b)
  out$tdf_step_w_c <- as.numeric(out$tdf_step_w_c)
  out$tdf_step_y_a <- as.integer(out$tdf_step_y_a)
  out$tdf_step_y_b <- as.integer(out$tdf_step_y_b)
  out$tdf_step_y_c <- as.integer(out$tdf_step_y_c)

  out$do_sequence_diagnostics <- as.integer(out$do_sequence_diagnostics)
  out$end_years <- as.integer(out$end_years)

  out$show_plots <- as.integer(out$show_plots)
  out$seed <- as.integer(out$seed)

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

# -------------------------
# 1) Parâmetros
# -------------------------
args <- parse_args(commandArgs(trailingOnly = TRUE))
set.seed(args$seed)

TICKER_EQ <- args$ticker_eq
FROM_DATE <- as.Date(args$from_date)
TO_DATE   <- as.Date(args$to_date)

WORK_YEARS   <- args$work_years
N_MONTHS     <- WORK_YEARS * 12L
CONTRIB_REAL <- args$contrib_real

W_MIX_60_40 <- args$w_mix_60_40
W_MIX_50_50 <- args$w_mix_50_50
W_MIX_40_60 <- args$w_mix_40_60

TDF_LIN_W_START <- args$tdf_lin_w_start
TDF_LIN_W_END   <- args$tdf_lin_w_end

TDF_STEP_W_A <- args$tdf_step_w_a
TDF_STEP_W_B <- args$tdf_step_w_b
TDF_STEP_W_C <- args$tdf_step_w_c
TDF_STEP_Y_A <- args$tdf_step_y_a
TDF_STEP_Y_B <- args$tdf_step_y_b
TDF_STEP_Y_C <- args$tdf_step_y_c

DO_SEQUENCE_DIAGNOSTICS <- (args$do_sequence_diagnostics == 1L)
END_YEARS <- args$end_years
END_MONTHS <- END_YEARS * 12L
SEQ_STRATEGIES <- strsplit(args$seq_strategies, ",", fixed = TRUE)[[1]]
SEQ_STRATEGIES <- trimws(SEQ_STRATEGIES)
SEQ_STRATEGIES <- SEQ_STRATEGIES[nzchar(SEQ_STRATEGIES)]

SHOW_PLOTS <- (args$show_plots == 1L)

# -------------------------
# 2) Pacotes
# -------------------------
pkgs <- c(
  "dplyr", "tidyr", "purrr", "tibble", "ggplot2",
  "quantmod", "xts", "zoo", "rbcb"
)
require_pkgs(pkgs)

suppressPackageStartupMessages({
  invisible(lapply(pkgs, library, character.only = TRUE))
})

# -------------------------
# 3) Funções utilitárias
# -------------------------
as_yearmon_safe <- function(x) zoo::as.yearmon(as.Date(x))

# IPCA pode vir como % mensal OU como índice.
# Se for índice -> taxa = (I_t / I_{t-1}) - 1.
build_ipca_monthly_rate <- function(df_ipca) {
  tmp <- df_ipca %>% dplyr::arrange(.data$ym)
  med <- median(tmp$IPCA_val, na.rm = TRUE)

  if (is.finite(med) && med > 20) {
    tmp %>%
      dplyr::mutate(ipca_m = (.data$IPCA_val / dplyr::lag(.data$IPCA_val)) - 1) %>%
      dplyr::select(.data$ym, .data$ipca_m)
  } else {
    tmp %>%
      dplyr::mutate(ipca_m = .data$IPCA_val / 100) %>%
      dplyr::select(.data$ym, .data$ipca_m)
  }
}

glide_weight_linear <- function(m, n, w_start, w_end) {
  if (n <= 1) return(w_end)
  frac <- (m - 1) / (n - 1)
  w_start + frac * (w_end - w_start)
}

weights_tdf_step <- function(n,
                             w_a = 0.70, w_b = 0.50, w_c = 0.30,
                             y_a = 20, y_b = 25, y_c = 30) {
  if (y_c * 12L != n) stop("TDF_STEP_Y_C * 12 precisa bater N_MONTHS (WORK_YEARS*12).", call. = FALSE)

  b1 <- y_a * 12L
  b2 <- y_b * 12L

  w <- numeric(n)

  # 1) fixo em w_a
  w[1:b1] <- w_a

  # 2) cai linearmente w_a -> w_b
  if (b2 > b1) {
    idx <- (b1 + 1L):b2
    w[idx] <- vapply(seq_along(idx), function(i) {
      glide_weight_linear(i, length(idx), w_a, w_b)
    }, numeric(1))
  }

  # 3) cai linearmente w_b -> w_c
  if (n > b2) {
    idx <- (b2 + 1L):n
    w[idx] <- vapply(seq_along(idx), function(i) {
      glide_weight_linear(i, length(idx), w_b, w_c)
    }, numeric(1))
  }

  pmin(pmax(w, 0), 1)
}

get_weight_vector <- function(strategy_id, n) {
  switch(
    strategy_id,
    "eq_100"     = rep(1.0, n),
    "rf_100"     = rep(0.0, n),
    "mix_60_40"  = rep(W_MIX_60_40, n),
    "mix_50_50"  = rep(W_MIX_50_50, n),
    "mix_40_60"  = rep(W_MIX_40_60, n),
    "tdf_lin"    = vapply(seq_len(n), glide_weight_linear, numeric(1),
                          n = n, w_start = TDF_LIN_W_START, w_end = TDF_LIN_W_END),
    "tdf_step"   = weights_tdf_step(n,
                                    w_a = TDF_STEP_W_A, w_b = TDF_STEP_W_B, w_c = TDF_STEP_W_C,
                                    y_a = TDF_STEP_Y_A, y_b = TDF_STEP_Y_B, y_c = TDF_STEP_Y_C),
    stop("strategy_id inválida: ", strategy_id, call. = FALSE)
  )
}

wealth_from_returns <- function(r_p, contrib_real) {
  W <- 0
  for (m in seq_along(r_p)) {
    W <- (W + contrib_real) * (1 + r_p[m])
  }
  W
}

get_cohort_portfolio_returns <- function(retire_ym, df_m, n_months, strategy_id) {
  end_i <- which(df_m$ym == retire_ym)
  if (length(end_i) != 1) return(NULL)
  start_i <- end_i - n_months + 1L
  if (start_i < 1L) return(NULL)

  path <- df_m[start_i:end_i, ] %>% tibble::as_tibble()
  w <- get_weight_vector(strategy_id, n_months)

  w * path$r_eq_real + (1 - w) * path$r_rf_real
}

simulate_cohort_wealth <- function(retire_ym, df_m, n_months, contrib_real, strategy_id) {
  r_p <- get_cohort_portfolio_returns(retire_ym, df_m, n_months, strategy_id)
  if (is.null(r_p)) return(NA_real_)
  wealth_from_returns(r_p, contrib_real)
}

summarise_by_strategy <- function(res_long) {
  res_long %>%
    dplyr::group_by(.data$strategy_id, .data$label) %>%
    dplyr::summarise(
      n = dplyr::n(),
      p05 = as.numeric(stats::quantile(.data$wealth, 0.05, na.rm = TRUE)),
      p10 = as.numeric(stats::quantile(.data$wealth, 0.10, na.rm = TRUE)),
      p25 = as.numeric(stats::quantile(.data$wealth, 0.25, na.rm = TRUE)),
      p50 = as.numeric(stats::quantile(.data$wealth, 0.50, na.rm = TRUE)),
      p75 = as.numeric(stats::quantile(.data$wealth, 0.75, na.rm = TRUE)),
      p90 = as.numeric(stats::quantile(.data$wealth, 0.90, na.rm = TRUE)),
      p95 = as.numeric(stats::quantile(.data$wealth, 0.95, na.rm = TRUE)),
      mean = mean(.data$wealth, na.rm = TRUE),
      sd   = stats::sd(.data$wealth, na.rm = TRUE),
      iqr  = as.numeric(stats::quantile(.data$wealth, 0.75, na.rm = TRUE) - stats::quantile(.data$wealth, 0.25, na.rm = TRUE)),
      iqr_over_p50 = iqr / p50,
      p90_over_p10 = p90 / p10,
      best_retire_ym  = as.character(.data$retire_ym[which.max(.data$wealth)]),
      best_wealth     = max(.data$wealth, na.rm = TRUE),
      worst_retire_ym = as.character(.data$retire_ym[which.min(.data$wealth)]),
      worst_wealth    = min(.data$wealth, na.rm = TRUE),
      best_over_worst = best_wealth / worst_wealth,
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$p50))
}

top_bottom <- function(res_long, strategy_id, k = 10) {
  df <- res_long %>%
    dplyr::filter(.data$strategy_id == .env$strategy_id) %>%
    dplyr::arrange(dplyr::desc(.data$wealth))

  n <- nrow(df)
  if (n == 0) {
    return(list(
      top = tibble::tibble(rank = integer(0), retire_ym = zoo::as.yearmon(character(0)), wealth = numeric(0)),
      bottom = tibble::tibble(rank = integer(0), retire_ym = zoo::as.yearmon(character(0)), wealth = numeric(0))
    ))
  }

  k_eff <- min(k, n)

  top <- df %>%
    dplyr::slice_head(n = k_eff) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::select(.data$rank, .data$retire_ym, .data$wealth)

  bottom <- df %>%
    dplyr::slice_tail(n = k_eff) %>%
    dplyr::arrange(.data$wealth) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::select(.data$rank, .data$retire_ym, .data$wealth)

  list(top = top, bottom = bottom)
}

sequence_diag_one <- function(retire_ym, df_m, n_months, contrib_real, strategy_id, end_months) {
  r_p <- get_cohort_portfolio_returns(retire_ym, df_m, n_months, strategy_id)
  if (is.null(r_p)) return(tibble::tibble())

  L <- end_months
  if (L >= length(r_p)) stop("END_MONTHS precisa ser menor que N_MONTHS.", call. = FALSE)

  r_head <- r_p[1:(length(r_p) - L)]
  r_tail <- r_p[(length(r_p) - L + 1):length(r_p)]

  W_actual <- wealth_from_returns(c(r_head, r_tail), contrib_real)
  W_good   <- wealth_from_returns(c(r_head, sort(r_tail, decreasing = FALSE)), contrib_real)  # baixo->alto no fim (melhor p/ acumulação)
  W_bad    <- wealth_from_returns(c(r_head, sort(r_tail, decreasing = TRUE)),  contrib_real)  # alto->baixo no fim (pior)

  tibble::tibble(
    retire_ym = retire_ym,
    strategy_id = strategy_id,
    W_actual = W_actual,
    W_good_order = W_good,
    W_bad_order  = W_bad,
    good_over_bad = W_good / W_bad,
    actual_vs_bad = W_actual / W_bad,
    actual_vs_good = W_actual / W_good
  )
}

# -------------------------
# 4) Dados: Ibovespa (mensal) + BCB/SGS
# -------------------------
message(">> Baixando Ibovespa (", TICKER_EQ, ", Yahoo Finance)...")

suppressWarnings({
  eq_xts <- quantmod::getSymbols(Symbols = TICKER_EQ, src = "yahoo",
                                from = FROM_DATE, to = TO_DATE,
                                auto.assign = FALSE)
})

px_d <- quantmod::Ad(eq_xts)
px_m <- quantmod::to.monthly(px_d, indexAt = "lastof", OHLC = FALSE)

r_eq_log_nom <- diff(log(px_m)) %>% stats::na.omit()
eq_ym <- as_yearmon_safe(zoo::index(r_eq_log_nom))

message(">> Baixando IPCA (SGS 433) e Selic mensal (SGS 4390) do BCB/SGS...")

ipca_raw <- rbcb::get_series(433, start_date = FROM_DATE, end_date = TO_DATE) %>%
  tibble::as_tibble() %>%
  dplyr::rename(date = 1, IPCA = 2) %>%
  dplyr::mutate(ym = zoo::as.yearmon(.data$date)) %>%
  dplyr::group_by(.data$ym) %>%
  dplyr::summarise(IPCA_val = dplyr::last(.data$IPCA), .groups = "drop")

ipca_m_tbl <- build_ipca_monthly_rate(ipca_raw) %>% tidyr::drop_na(.data$ipca_m)

selic_raw <- rbcb::get_series(4390, start_date = FROM_DATE, end_date = TO_DATE) %>%
  tibble::as_tibble() %>%
  dplyr::rename(date = 1, SELIC_M = 2) %>%
  dplyr::mutate(ym = zoo::as.yearmon(.data$date)) %>%
  dplyr::group_by(.data$ym) %>%
  dplyr::summarise(SELIC_val = dplyr::last(.data$SELIC_M), .groups = "drop") %>%
  dplyr::mutate(selic_m = as.numeric(.data$SELIC_val) / 100) %>%
  dplyr::select(.data$ym, .data$selic_m)

bcb <- ipca_m_tbl %>% dplyr::inner_join(selic_raw, by = "ym")

# -------------------------
# 5) Merge + retornos reais
# -------------------------
df_m <- tibble::tibble(
  ym = eq_ym,
  r_eq_log_nom = as.numeric(r_eq_log_nom)
) %>%
  dplyr::inner_join(bcb, by = "ym") %>%
  dplyr::mutate(
    r_eq_nom  = exp(.data$r_eq_log_nom) - 1,
    r_eq_real = (1 + .data$r_eq_nom) / (1 + .data$ipca_m) - 1,
    r_rf_real = (1 + .data$selic_m) / (1 + .data$ipca_m) - 1
  ) %>%
  dplyr::select(.data$ym, .data$r_eq_real, .data$r_rf_real) %>%
  dplyr::arrange(.data$ym) %>%
  tidyr::drop_na(.data$r_eq_real, .data$r_rf_real) %>%
  dplyr::filter(is.finite(.data$r_eq_real), is.finite(.data$r_rf_real))

message(">> Meses disponíveis após merge: ", nrow(df_m))
if (nrow(df_m) < (N_MONTHS + 12L)) stop("Poucos meses para janela de ", WORK_YEARS, " anos. Ajuste --from_date para mais antigo.", call. = FALSE)

if (any(df_m$r_eq_real <= -0.999, na.rm = TRUE)) stop("Retorno real de ações <= -99.9% (dados estranhos).", call. = FALSE)
if (any(df_m$r_rf_real <= -0.999, na.rm = TRUE)) stop("Retorno real de RF <= -99.9% (dados estranhos).", call. = FALSE)

# -------------------------
# 6) Coortes e estratégias
# -------------------------
first_ok <- df_m$ym[N_MONTHS]
retire_ym_vec <- df_m$ym[df_m$ym >= first_ok]

strategies <- tibble::tribble(
  ~strategy_id, ~label,
  "eq_100",    "100% Ações (Ibovespa)",
  "rf_100",    "100% RF (Selic)",
  "mix_60_40", "Mix fixo 60/40 (Ações/RF)",
  "mix_50_50", "Mix fixo 50/50 (Ações/RF)",
  "mix_40_60", "Mix fixo 40/60 (Ações/RF)",
  "tdf_lin",   paste0("Glide linear ", round(100*TDF_LIN_W_START), "% → ", round(100*TDF_LIN_W_END), "% (ações)"),
  "tdf_step",  paste0("Glide por degraus ",
                      round(100*TDF_STEP_W_A), "% (", TDF_STEP_Y_A, "a) → ",
                      round(100*TDF_STEP_W_B), "% (", (TDF_STEP_Y_B - TDF_STEP_Y_A), "a) → ",
                      round(100*TDF_STEP_W_C), "% (", (TDF_STEP_Y_C - TDF_STEP_Y_B), "a)")
)

message(">> Simulando coortes (isso pode levar alguns segundos)...")
res_long <- tidyr::crossing(
  retire_ym = retire_ym_vec,
  strategy_id = strategies$strategy_id
) %>%
  dplyr::left_join(strategies, by = "strategy_id") %>%
  dplyr::mutate(
    wealth = purrr::map2_dbl(.data$retire_ym, .data$strategy_id, ~simulate_cohort_wealth(
      retire_ym = .x,
      df_m = df_m,
      n_months = N_MONTHS,
      contrib_real = CONTRIB_REAL,
      strategy_id = .y
    ))
  ) %>%
  tidyr::drop_na(.data$wealth)

# -------------------------
# 7) Relatório FINAL (console)
# -------------------------
summary_tbl <- summarise_by_strategy(res_long)

# Top/Bottom por estratégia (para inspeção)
tb_eq  <- top_bottom(res_long, "eq_100", k = 10)
tb_tdf <- top_bottom(res_long, "tdf_step", k = 10)

# Diagnóstico de sequence of returns no fim
seq_diag <- NULL
seq_summary <- NULL
if (DO_SEQUENCE_DIAGNOSTICS) {
  message(">> Rodando diagnóstico de sequence of returns (últimos ", END_YEARS, " anos)...")

  seq_diag <- tidyr::crossing(
    retire_ym = retire_ym_vec,
    strategy_id = SEQ_STRATEGIES
  ) %>%
    dplyr::mutate(tmp = purrr::map2(.data$retire_ym, .data$strategy_id, ~sequence_diag_one(
      retire_ym = .x,
      df_m = df_m,
      n_months = N_MONTHS,
      contrib_real = CONTRIB_REAL,
      strategy_id = .y,
      end_months = END_MONTHS
    ))) %>%
    dplyr::select(.data$tmp) %>%
    tidyr::unnest(.data$tmp)

  if (nrow(seq_diag) > 0) {
    seq_summary <- seq_diag %>%
      dplyr::group_by(.data$strategy_id) %>%
      dplyr::summarise(
        n = dplyr::n(),
        p10 = as.numeric(stats::quantile(.data$good_over_bad, 0.10, na.rm = TRUE)),
        p50 = as.numeric(stats::quantile(.data$good_over_bad, 0.50, na.rm = TRUE)),
        p90 = as.numeric(stats::quantile(.data$good_over_bad, 0.90, na.rm = TRUE)),
        max_ratio = max(.data$good_over_bad, na.rm = TRUE),
        retire_ym_max = as.character(.data$retire_ym[which.max(.data$good_over_bad)]),
        .groups = "drop"
      ) %>%
      dplyr::left_join(strategies, by = "strategy_id") %>%
      dplyr::select(.data$label, dplyr::everything())
  }
}

# --------- IMPRESSÃO FINAL ---------
cat("\n============================================================\n")
cat("COORTES DE APOSENTADORIA — BRASIL (", WORK_YEARS, " anos, retornos reais)\n", sep = "")
cat("Aporte mensal real: ", CONTRIB_REAL, "\n", sep = "")
cat("Janela: ", WORK_YEARS, " anos (", N_MONTHS, " meses)\n", sep = "")
cat("Base mensal disponível: ", as.character(min(df_m$ym)), " até ", as.character(max(df_m$ym)), "\n", sep = "")
cat("Primeira aposentadoria possível: ", as.character(first_ok), "\n", sep = "")
cat("Número de coortes: ", length(retire_ym_vec), "\n", sep = "")
cat("============================================================\n\n")

cat(">>> RESUMO POR ESTRATÉGIA (dispersão entre coortes)\n")
print(summary_tbl)

cat("\n\n>>> TOP 10 / BOTTOM 10 (para ver datas 'boas' e 'ruins')\n")
cat("\n-- 100% Ações (Ibovespa): TOP 10\n"); print(tb_eq$top)
cat("\n-- 100% Ações (Ibovespa): BOTTOM 10\n"); print(tb_eq$bottom)

cat("\n-- Glide por degraus: TOP 10\n"); print(tb_tdf$top)
cat("\n-- Glide por degraus: BOTTOM 10\n"); print(tb_tdf$bottom)

if (!is.null(seq_summary)) {
  cat("\n\n>>> DIAGNÓSTICO: 'sequence of returns' nos ÚLTIMOS ", END_YEARS, " anos\n", sep = "")
  cat("Interpretação: good_over_bad é o quanto a ORDEM dos retornos no fim pode mudar o resultado,\n")
  cat("mantendo o mesmo conjunto de retornos (só reordenando o final da janela).\n\n")
  print(seq_summary)

  cat("\nExemplos (maior sensibilidade à ordem no fim):\n")
  ex <- seq_diag %>%
    dplyr::group_by(.data$strategy_id) %>%
    dplyr::slice_max(.data$good_over_bad, n = 3, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(strategies, by = "strategy_id") %>%
    dplyr::select(.data$label, .data$retire_ym, .data$good_over_bad,
                 .data$W_bad_order, .data$W_actual, .data$W_good_order) %>%
    dplyr::arrange(dplyr::desc(.data$good_over_bad))
  print(ex)
}

cat("\n\n>>> Leitura prática\n")
cat("- Compare 'p90_over_p10' e 'iqr_over_p50' entre estratégias: menor = menos dependente do ano de aposentadoria.\n")
cat("- Olhe 'worst_wealth' e 'worst_retire_ym': isso materializa o 'se aposentar no ano errado'.\n")
cat("- Se o glide path reduzir p90/p10 e elevar o piso (p10 / worst), ele está fazendo o papel de robustez.\n")
cat("============================================================\n\n")

# -------------------------
# 8) Plots (opcional; não salva)
# -------------------------
if (SHOW_PLOTS) {
  p <- res_long %>%
    dplyr::mutate(retire_date = as.Date(.data$retire_ym)) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$retire_date, y = .data$wealth)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ .data$label, scales = "free_y") +
    ggplot2::labs(
      x = "Data de aposentadoria (fim da janela)",
      y = "Riqueza real acumulada (unid.)",
      title = paste0("Coortes (", WORK_YEARS, " anos) — Riqueza final real por estratégia")
    ) +
    ggplot2::theme_minimal()
  print(p)
}
