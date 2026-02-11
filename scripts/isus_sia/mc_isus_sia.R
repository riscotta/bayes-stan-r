#!/usr/bin/env Rscript

############################################################
# ISUS / SIA — Robust Monte Carlo (cluster bootstrap por CNES)
# Precificação SIGTAP a partir de ISUS_SIA_PARS (CSV ';')
# Console-only (nada é gravado em arquivo)
#
# IMPORTANTE (assunção do experimento):
#  - VLR_APROVADA é VALOR UNITÁRIO aprovado.
#  - Logo o valor total por linha é: VLR_TOTAL = VLR_APROVADA * QTD_APROVADA
#
# Rode a partir do ROOT do repo:
#   Rscript scripts/isus_sia/mc_isus_sia.R
#
# Opções (formato --chave=valor):
#   --csv_path=data/raw/isus_sia/ISUS_SIA_PARS.csv
#   --zip_path=data/raw/isus_sia/ISUS_SIA_PARS.zip
#   --seed=123
#   --min_total_q=5000
#   --top_n_groups_mc=15
#   --B_state=400
#   --B_split=200
#   --outlier_k_ratio=5
#   --outlier_prev_max=0.10
#   --outlier_qshare_max=0.20
#   --outlier_top_k=3
#   --trim_alpha=0.05
############################################################

# ----------------------------
# 0) Helpers (args + checks)
# ----------------------------
parse_args <- function(args) {
  out <- list(
    csv_path = file.path("data", "raw", "isus_sia", "ISUS_SIA_PARS.csv"),
    zip_path = file.path("data", "raw", "isus_sia", "ISUS_SIA_PARS.zip"),
    seed = 123L,
    min_total_q = 5000,
    top_n_groups_mc = 15L,
    B_state = 400L,
    B_split = 200L,
    outlier_k_ratio = 5,
    outlier_prev_max = 0.10,
    outlier_qshare_max = 0.20,
    outlier_top_k = 3L,
    trim_alpha = 0.05
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
  out$min_total_q <- as.numeric(out$min_total_q)
  out$top_n_groups_mc <- as.integer(out$top_n_groups_mc)
  out$B_state <- as.integer(out$B_state)
  out$B_split <- as.integer(out$B_split)
  out$outlier_k_ratio <- as.numeric(out$outlier_k_ratio)
  out$outlier_prev_max <- as.numeric(out$outlier_prev_max)
  out$outlier_qshare_max <- as.numeric(out$outlier_qshare_max)
  out$outlier_top_k <- as.integer(out$outlier_top_k)
  out$trim_alpha <- as.numeric(out$trim_alpha)

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

locate_input <- function(csv_path, zip_path) {
  # 1) CSV direto
  if (file.exists(csv_path)) {
    return(list(path = csv_path, kind = "csv", tempdir = NA_character_))
  }

  # 2) Fallback útil (ambiente sandbox /mnt/data)
  if (!file.exists(csv_path) && file.exists("/mnt/data/ISUS_SIA_PARS.csv")) {
    return(list(path = "/mnt/data/ISUS_SIA_PARS.csv", kind = "csv", tempdir = NA_character_))
  }
  if (!file.exists(zip_path) && file.exists("/mnt/data/ISUS_SIA_PARS.zip")) {
    zip_path <- "/mnt/data/ISUS_SIA_PARS.zip"
  }

  # 3) ZIP -> extrai para tempdir (não polui o repo)
  if (file.exists(zip_path)) {
    td <- tempfile("isus_sia_")
    dir.create(td, recursive = TRUE, showWarnings = FALSE)
    utils::unzip(zip_path, exdir = td)
    csv_in_zip <- file.path(td, "ISUS_SIA_PARS.csv")
    if (!file.exists(csv_in_zip)) {
      stop(
        "ZIP encontrado, mas não achei ISUS_SIA_PARS.csv dentro dele: ", zip_path,
        call. = FALSE
      )
    }
    return(list(path = csv_in_zip, kind = "zip", tempdir = td))
  }

  stop(
    "Não encontrei o dataset. Opções:\n",
    " - Coloque o CSV em data/raw/isus_sia/ISUS_SIA_PARS.csv e rode novamente; ou\n",
    " - Coloque o ZIP em data/raw/isus_sia/ISUS_SIA_PARS.zip (contendo ISUS_SIA_PARS.csv); ou\n",
    " - Informe via --csv_path=... ou --zip_path=...\n",
    call. = FALSE
  )
}

args <- commandArgs(trailingOnly = TRUE)
opt  <- parse_args(args)

pkgs <- c("data.table")
require_pkgs(pkgs)

suppressPackageStartupMessages({
  library(data.table)
})

inp <- locate_input(opt$csv_path, opt$zip_path)
on.exit(if (is.character(inp$tempdir) && !is.na(inp$tempdir) && dir.exists(inp$tempdir)) {
  unlink(inp$tempdir, recursive = TRUE, force = TRUE)
}, add = TRUE)

csv_path <- inp$path

set.seed(opt$seed)

# Análise principal (apenas g4)
min_total_q     <- opt$min_total_q   # ignora grupos com pouca produção (evita ruído)
top_n_groups_mc <- opt$top_n_groups_mc

# Monte Carlo (cluster bootstrap por CNES)
B_state <- opt$B_state
B_split <- opt$B_split

# Outliers (procedimentos caros e raros dentro de um grupo g4)
outlier_k_ratio    <- opt$outlier_k_ratio
outlier_prev_max   <- opt$outlier_prev_max
outlier_qshare_max <- opt$outlier_qshare_max
outlier_top_k      <- opt$outlier_top_k

# Robustez (alternativas)
trim_alpha <- opt$trim_alpha

############################
# 2) FUNÇÕES UTILITÁRIAS
############################

pad_zeros <- function(x, width = 10) {
  x <- trimws(as.character(x))
  x[is.na(x)] <- ""
  n <- nchar(x)
  x <- ifelse(n >= width, substr(x, 1, width), paste0(strrep("0", width - n), x))
  x
}

# Parse robusto PT-BR (aceita mix de "8.82", "8,82", "530," etc.)
parse_ptbr_number <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x)] <- ""
  x <- gsub("[^0-9,\\.\\-]", "", x)

  out <- rep(NA_real_, length(x))
  ok  <- nzchar(x) & x != "-"
  y   <- x[ok]

  has_comma <- grepl(",", y, fixed = TRUE)
  has_dot   <- grepl("\\.", y)

  # "." e "," e termina com ",dd" => vírgula decimal, ponto milhar
  case1 <- has_comma & has_dot & grepl(",\\d{0,2}$", y)
  if (any(case1)) {
    y1 <- y[case1]
    y1 <- gsub("\\.", "", y1)
    y1 <- sub(",", ".", y1, fixed = TRUE)
    y[case1] <- y1
  }

  # "." e "," e termina com ".dd" => ponto decimal, vírgula milhar
  case2 <- has_comma & has_dot & grepl("\\.\\d{1,2}$", y) & !case1
  if (any(case2)) {
    y2 <- y[case2]
    y2 <- gsub(",", "", y2, fixed = TRUE)
    y[case2] <- y2
  }

  # só vírgula => vírgula decimal
  case3 <- has_comma & !has_dot & !case1 & !case2
  if (any(case3)) {
    y3 <- y[case3]
    y3 <- sub(",", ".", y3, fixed = TRUE)
    y[case3] <- y3
  }

  suppressWarnings(out[ok] <- as.numeric(y))
  out
}

weighted_quantile <- function(x, w, probs) {
  stopifnot(length(x) == length(w))
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]; w <- w[keep]
  if (!length(x)) return(rep(NA_real_, length(probs)))

  o <- order(x)
  x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)

  sapply(probs, function(p) x[which(cw >= p)[1]])
}

weighted_median <- function(x, w) weighted_quantile(x, w, 0.5)[1]

weighted_trimmed_mean <- function(x, w, alpha = 0.05) {
  if (!length(x)) return(NA_real_)
  lo <- weighted_quantile(x, w, alpha)
  hi <- weighted_quantile(x, w, 1 - alpha)
  keep <- is.finite(x) & is.finite(w) & w > 0 & x >= lo & x <= hi
  if (!any(keep)) return(NA_real_)
  sum(x[keep] * w[keep]) / sum(w[keep])
}

weighted_winsor_mean <- function(x, w, alpha = 0.05) {
  if (!length(x)) return(NA_real_)
  lo <- weighted_quantile(x, w, alpha)
  hi <- weighted_quantile(x, w, 1 - alpha)
  xw <- pmin(pmax(x, lo), hi)
  keep <- is.finite(xw) & is.finite(w) & w > 0
  if (!any(keep)) return(NA_real_)
  sum(xw[keep] * w[keep]) / sum(w[keep])
}

weighted_geo_mean <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]; w <- w[keep]
  if (!length(x)) return(NA_real_)

  pos <- x[x > 0]
  if (!length(pos)) return(NA_real_)
  eps <- 1e-6 * median(pos)
  x2 <- pmax(x, eps)

  exp(sum(w * log(x2)) / sum(w))
}

huber_location <- function(x, w, k = 1.345, max_iter = 30, tol = 1e-6) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]; w <- w[keep]
  if (!length(x)) return(NA_real_)

  mu <- weighted_median(x, w)
  for (it in 1:max_iter) {
    r <- x - mu
    s <- 1.4826 * weighted_median(abs(r), w)
    s <- ifelse(is.finite(s) && s > 0, s, 1e-9)

    u <- r / (k * s)
    w_rob <- pmin(1, 1 / pmax(abs(u), 1e-12))
    w_new <- w * w_rob

    mu_new <- sum(w_new * x) / sum(w_new)
    if (abs(mu_new - mu) <= tol * (1 + abs(mu))) break
    mu <- mu_new
  }
  mu
}

safe_int_seed <- function(x, base = 0L) {
  xi <- suppressWarnings(as.integer(x))
  if (length(xi) == 0 || is.na(xi)) {
    s <- sum(utf8ToInt(as.character(x)))
    return(as.integer(base + s))
  }
  as.integer(base + xi)
}

alternatives_catalog <- c(
  "1) Média (pooled): soma(VLR_total)/soma(QTD)  [atual; ótima p/ erro quadrático, sensível a cauda]",
  "2) Mediana ponderada por volume (CNES-level): robusta; reduz 'contaminação' por procedimentos caros/raros",
  "3) Trimmed mean ponderada (ex.: 5%): corta caudas; bom meio-termo",
  "4) Winsor mean ponderada (ex.: 5%): limita extremos; estável",
  "5) Média geométrica ponderada (log-mean): reduz efeito multiplicativo de outliers",
  "6) Huber M-estimator (ponderado): robusto contínuo; compromisso entre média e mediana",
  "7) Split-tail (core + procedimentos caros/raros em separado): solução estrutural; evita item caro contaminar pacote",
  "8) Contratar em granularidade maior (6 dígitos ou 10 dígitos) para itens de alta variância / alta cauda",
  "9) Cap/ceiling (teto por grupo) ou regra do tipo 'máximo = p95' para precificação do pacote",
  "10) Modelos hierárquicos / Empirical Bayes (parcial pooling) por região e procedimento (mais complexo, melhor governança)",
  "11) Mistura / two-part: separar 'procedimentos especiais' por regra tecnológica/clínica e o resto por pacote",
  "12) Elegibilidade: média do pacote só entre CNES aptos (capacidade instalada), evitando 'benefício' do não-ofertante"
)

############################
# 3) CARREGAMENTO E PREPARO
############################
dt_raw <- fread(
  csv_path,
  sep = ";",
  colClasses = list(character = c("COD_CNES", "COD_PROCEDIMENTO", "QTD_APROVADA", "VLR_APROVADA")),
  encoding = "UTF-8",
  showProgress = FALSE
)

dt <- copy(dt_raw)
dt[, COD_CNES := trimws(COD_CNES)]
dt[, proc10   := pad_zeros(COD_PROCEDIMENTO, 10)]

# QTD: parse PT-BR + checagem de integridade (quantidade deve ser inteira)
qtd_raw <- parse_ptbr_number(dt$QTD_APROVADA)
qtd_round <- suppressWarnings(as.integer(round(qtd_raw)))
nonint_flag <- is.finite(qtd_raw) & is.finite(qtd_round) & abs(qtd_raw - qtd_round) > 1e-8

dt[, qtd := qtd_round]

# VLR_APROVADA: unitário (assumido)
dt[, vlr_unit := parse_ptbr_number(VLR_APROVADA)]

# Mantém apenas linhas válidas (QTD>0, VLR_UNIT>=0)
dt <- dt[is.finite(qtd) & is.finite(vlr_unit) & qtd > 0 & vlr_unit >= 0]

# Valor total monetário derivado
#  - unit: alias do unitário (para estatística de cauda/outliers)
#  - vlr:  total monetário (unit * qtd), usado nas somas e no MC
dt[, unit := vlr_unit]
dt[, vlr  := unit * qtd]

# Prefixos
dt[, g4 := substr(proc10, 1, 4)]

# Área (apenas ESTADO, conforme solicitado)
dt[, ESTADO := "ESTADO"]

############################
# 3.1) SANITY CHECKS (Item 1)
############################
# Objetivo: evitar erro silencioso (principalmente QTD parse e unidade vs total em VLR_APROVADA)

sanity <- list()

# 3.1.1 Quantidade não-inteira no bruto (antes de arredondar)
san_f_nonint <- 0
if (length(qtd_raw) > 0) {
  san_f_nonint <- mean(nonint_flag, na.rm = TRUE)
}

# 3.1.2 Correlação VLR_UNIT vs QTD (se VLR_APROVADA for total, tende a crescer com QTD)
# (spearman é mais robusto)
spearman_r <- NA_real_
if (nrow(dt) >= 200) {
  suppressWarnings({
    spearman_r <- cor(dt$vlr_unit, dt$qtd, method = "spearman")
  })
}

# 3.1.3 Outros checks simples
f_qtd_gt1 <- if (nrow(dt) > 0) mean(dt$qtd > 1, na.rm = TRUE) else NA_real_
proc_len  <- nchar(trimws(as.character(dt_raw$COD_PROCEDIMENTO)))
proc_len  <- proc_len[is.finite(proc_len)]

sanity$nonint_qtd_fraction_raw <- san_f_nonint
sanity$qtd_gt1_fraction_used   <- f_qtd_gt1
sanity$spearman_unit_qtd       <- spearman_r
sanity$proc_len_summary <- if (length(proc_len)) {
  data.table(
    proc_len_min = min(proc_len),
    proc_len_p50 = as.numeric(quantile(proc_len, 0.50)),
    proc_len_p95 = as.numeric(quantile(proc_len, 0.95)),
    proc_len_max = max(proc_len)
  )
} else {
  data.table(proc_len_min = NA_integer_, proc_len_p50 = NA_real_, proc_len_p95 = NA_real_, proc_len_max = NA_integer_)
}

sanity$unit_summary <- if (nrow(dt) > 0) {
  data.table(
    unit_p01 = as.numeric(quantile(dt$unit, 0.01)),
    unit_p50 = as.numeric(quantile(dt$unit, 0.50)),
    unit_p95 = as.numeric(quantile(dt$unit, 0.95)),
    unit_p999= as.numeric(quantile(dt$unit, 0.999)),
    unit_max = max(dt$unit)
  )
} else {
  data.table(unit_p01 = NA_real_, unit_p50 = NA_real_, unit_p95 = NA_real_, unit_p999 = NA_real_, unit_max = NA_real_)
}

# Heurística de alerta (não prova, mas protege contra erro grosseiro)
# - se muita gente tem qtd>1 E o unitário cresce com qtd, pode ser que VLR_APROVADA seja TOTAL.
sanity$warnings <- character(0)
if (is.finite(san_f_nonint) && san_f_nonint > 0.01) {
  sanity$warnings <- c(sanity$warnings, sprintf("QTD_APROVADA: %.1f%% das linhas tinham valor não-inteiro no bruto (antes de arredondar).", 100*san_f_nonint))
}
if (is.finite(spearman_r) && is.finite(f_qtd_gt1) && f_qtd_gt1 > 0.20 && spearman_r > 0.20) {
  sanity$warnings <- c(sanity$warnings, sprintf("ALERTA: Spearman(unit, qtd)=%.3f e %.1f%% das linhas têm qtd>1. Isso é compatível com VLR_APROVADA ser TOTAL (e não unitário). Verifique essa suposição.", spearman_r, 100*f_qtd_gt1))
}
if (is.finite(sanity$unit_summary$unit_p999) && sanity$unit_summary$unit_p999 > 1e6) {
  sanity$warnings <- c(sanity$warnings, "ALERTA: unitário p99.9 > 1e6. Verifique se há moeda/escala incorreta (centavos, total vs unitário, etc.).")
}

############################
# 4) DETECÇÃO DE PROCEDIMENTOS CAROS/RARES (OUTLIERS) POR g4
############################
# Observação: aqui vlr já é TOTAL monetário (unit*qtd). Então:
# unit_proced = sum(vlr)/sum(qtd) = média ponderada do unitário.
proc_stats_g4 <- dt[, .(
  q = sum(qtd),
  v = sum(vlr),
  unit = sum(vlr) / sum(qtd),
  n_cnes = uniqueN(COD_CNES)
), by = .(g4, proc10)]

cnes_count_g4 <- dt[, .(n_cnes_group = uniqueN(COD_CNES)), by = .(g4)]
proc_stats_g4 <- merge(proc_stats_g4, cnes_count_g4, by = "g4", all.x = TRUE)

proc_stats_g4[, q_share := q / sum(q), by = .(g4)]
proc_stats_g4[, unit_med := weighted_median(unit, q), by = .(g4)]
proc_stats_g4[, unit_ratio := unit / pmax(unit_med, 1e-12)]
proc_stats_g4[, prevalence := n_cnes / pmax(n_cnes_group, 1)]

proc_stats_g4[, is_outlier := (unit_ratio >= outlier_k_ratio) &
                (prevalence <= outlier_prev_max) &
                (q_share <= outlier_qshare_max)]

# Top-K outliers por g4 (Item 5 - parte 1)
outlier_topk_g4 <- proc_stats_g4[is_outlier == TRUE][order(g4, -unit_ratio)]
outlier_topk_g4 <- outlier_topk_g4[, .SD[1:min(.N, outlier_top_k)], by = .(g4)]
outlier_topk_g4[, outlier_rank := seq_len(.N), by = .(g4)]

# Impacto estimado ao remover a cauda (todos outliers) (Item 5 - parte 2)
impact_g4 <- proc_stats_g4[, .(
  q_all = sum(q),
  v_all = sum(v),
  q_tail = sum(q[is_outlier == TRUE]),
  v_tail = sum(v[is_outlier == TRUE]),
  n_outlier_procs = sum(is_outlier == TRUE)
), by = .(g4)]

impact_g4[, price_all := v_all / pmax(q_all, 1e-12)]
impact_g4[, price_core := ifelse((q_all - q_tail) > 0,
                                (v_all - v_tail) / (q_all - q_tail),
                                NA_real_)]
impact_g4[, impact_abs := price_all - price_core]
impact_g4[, impact_rel := impact_abs / pmax(price_all, 1e-12)]
impact_g4[, tail_share_q := q_tail / pmax(q_all, 1e-12)]
impact_g4[, tail_share_v := v_tail / pmax(v_all, 1e-12)]

# Impacto por procedimento (removendo 1 a 1) – útil para ranking explicável
impact_proc_g4 <- merge(
  proc_stats_g4[, .(g4, proc10, q_proc = q, v_proc = v, unit, unit_ratio, prevalence, q_share, is_outlier)],
  impact_g4[, .(g4, q_all, v_all, price_all)],
  by = "g4",
  all.x = TRUE
)
impact_proc_g4[, price_wo_proc := ifelse((q_all - q_proc) > 0,
                                        (v_all - v_proc) / (q_all - q_proc),
                                        NA_real_)]
impact_proc_g4[, impact_abs := price_all - price_wo_proc]
impact_proc_g4[, impact_rel := impact_abs / pmax(price_all, 1e-12)]

############################
# 5) TABELAS POR PRESTADOR E SENSIBILIDADE DO g4 (ESTADO)
############################
prov_state_g4 <- dt[, .(q = sum(qtd), v = sum(vlr)), by = .(ESTADO, g4, COD_CNES)]
prov_state_g4[, cost := v / q]

sens_state_g4 <- prov_state_g4[, {
  n_cnes_local  <- .N
  total_q_local <- sum(q)
  total_v_local <- sum(v)

  mean_price  <- total_v_local / total_q_local
  wmed_price  <- weighted_median(cost, q)
  trim_price  <- weighted_trimmed_mean(cost, q, trim_alpha)
  wins_price  <- weighted_winsor_mean(cost, q, trim_alpha)
  geo_price   <- weighted_geo_mean(cost, q)
  huber_price <- huber_location(cost, q)

  list(
    n_cnes     = n_cnes_local,
    total_q    = total_q_local,
    total_v    = total_v_local,

    price_mean  = mean_price,
    price_wmed  = wmed_price,
    price_trim  = trim_price,
    price_wins  = wins_price,
    price_geo   = geo_price,
    price_huber = huber_price,

    delta_mean_vs_wmed     = (mean_price / pmax(wmed_price, 1e-12)) - 1,
    share_vol_cost_lt_mean = sum(q[cost < mean_price]) / total_q_local,
    p95_cost               = weighted_quantile(cost, q, 0.95)[1],
    max_cost               = max(cost)
  )
}, by = .(ESTADO, g4)]

# Junta info de outliers e impacto
sens_state_g4 <- merge(sens_state_g4, impact_g4[, .(g4, n_outlier_procs, tail_share_q, tail_share_v, price_core, impact_abs, impact_rel)],
                       by = "g4", all.x = TRUE)

# Top-1 outlier (para coluna compacta)
top1 <- outlier_topk_g4[outlier_rank == 1, .(g4, top_outlier_proc = proc10, top_outlier_unit = unit, top_outlier_ratio = unit_ratio,
                                            top_outlier_prev = prevalence, top_outlier_qshare = q_share)]
sens_state_g4 <- merge(sens_state_g4, top1, by = "g4", all.x = TRUE)
sens_state_g4[, has_outlier_proc := is.finite(n_outlier_procs) & n_outlier_procs > 0]

# filtro de estabilidade mínima
sens_state_g4 <- sens_state_g4[total_q >= min_total_q & n_cnes >= 10]

# Seleção dos grupos mais sensíveis para MC detalhado
groups_mc <- sens_state_g4[order(-abs(delta_mean_vs_wmed))][1:min(.N, top_n_groups_mc), g4]

############################
# 6) MONTE CARLO (BOOTSTRAP POR CNES) – MÉTODOS ROBUSTOS
#    Item 2: Avaliação out-of-bag (OOB)
#    Item 3: Otimização (multinomial weights + métricas vetorizadas)
############################
simulate_group_methods <- function(prov_g, B = 300, seed = 123) {
  set.seed(seed)

  prov_g <- prov_g[is.finite(cost) & is.finite(q) & is.finite(v) & q > 0]
  if (nrow(prov_g) < 10) return(NULL)

  x0 <- prov_g$cost
  w0 <- prov_g$q
  v0 <- prov_g$v
  n  <- nrow(prov_g)

  methods <- c("MEAN", "WMED", "TRIM", "WINS", "GEO", "HUBER")
  price_mat <- matrix(NA_real_, nrow = B, ncol = length(methods)); colnames(price_mat) <- methods

  abs_per_unit_oob <- matrix(NA_real_, nrow = B, ncol = length(methods)); colnames(abs_per_unit_oob) <- methods
  overpay_frac_oob <- matrix(NA_real_, nrow = B, ncol = length(methods)); colnames(overpay_frac_oob) <- methods
  underpay_frac_oob<- matrix(NA_real_, nrow = B, ncol = length(methods)); colnames(underpay_frac_oob) <- methods

  oob_n <- integer(B)

  for (b in 1:B) {
    idx <- sample.int(n, size = n, replace = TRUE)
    m   <- tabulate(idx, nbins = n)

    w_boot <- w0 * m
    v_boot <- v0 * m

    # preços (in-bag)
    p_mean  <- sum(v_boot) / pmax(sum(w_boot), 1e-12)
    p_wmed  <- weighted_median(x0, w_boot)
    p_trim  <- weighted_trimmed_mean(x0, w_boot, trim_alpha)
    p_wins  <- weighted_winsor_mean(x0, w_boot, trim_alpha)
    p_geo   <- weighted_geo_mean(x0, w_boot)
    p_huber <- huber_location(x0, w_boot)

    ps <- c(p_mean, p_wmed, p_trim, p_wins, p_geo, p_huber)
    price_mat[b, ] <- ps

    # métricas OOB
    oob <- (m == 0)
    oob_n[b] <- sum(oob)
    if (oob_n[b] > 0) {
      w_o <- w0[oob]
      v_o <- v0[oob]

      pred <- outer(w_o, ps)          # n_oob x n_methods
      err  <- pred - v_o

      denom_q <- sum(w_o)
      denom_v <- sum(v_o)

      abs_per_unit_oob[b, ] <- colSums(abs(err)) / pmax(denom_q, 1e-12)
      overpay_frac_oob[b, ] <- colSums(pmax(err, 0))  / pmax(denom_v, 1e-12)
      underpay_frac_oob[b, ]<- colSums(pmax(-err, 0)) / pmax(denom_v, 1e-12)
    }
  }

  summarize_mat <- function(M) {
    data.table(
      method = colnames(M),
      mean   = colMeans(M, na.rm = TRUE),
      p05    = apply(M, 2, quantile, probs = 0.05, na.rm = TRUE),
      p50    = apply(M, 2, quantile, probs = 0.50, na.rm = TRUE),
      p95    = apply(M, 2, quantile, probs = 0.95, na.rm = TRUE)
    )
  }

  list(
    price = summarize_mat(price_mat),
    abs_per_unit_oob = summarize_mat(abs_per_unit_oob),
    overpay_frac_oob = summarize_mat(overpay_frac_oob),
    underpay_frac_oob= summarize_mat(underpay_frac_oob),
    oob_n_summary = data.table(
      oob_n_mean = mean(oob_n),
      oob_n_p05  = as.numeric(quantile(oob_n, 0.05)),
      oob_n_p50  = as.numeric(quantile(oob_n, 0.50)),
      oob_n_p95  = as.numeric(quantile(oob_n, 0.95))
    )
  )
}

# Split-tail (core + outliers a 10 dígitos separados)
# Avaliação OOB + uso de pesos multinomiais (já era tabulate, mas agora métrica é OOB)
simulate_split_tail <- function(DT_g, outlier_procs, B = 200, seed = 123) {
  set.seed(seed)
  if (!length(outlier_procs)) return(NULL)

  prov_total <- DT_g[, .(q = sum(qtd), v = sum(vlr)), by = .(COD_CNES)]
  prov_total <- prov_total[q > 0]

  DT_g[, is_tail := proc10 %in% outlier_procs]
  prov_core <- DT_g[is_tail == FALSE, .(core_q = sum(qtd), core_v = sum(vlr)), by = .(COD_CNES)]
  prov_tail <- DT_g[is_tail == TRUE,  .(q = sum(qtd), v = sum(vlr)), by = .(COD_CNES, proc10)]

  prov <- merge(prov_total, prov_core, by = "COD_CNES", all.x = TRUE)
  prov[is.na(core_q), core_q := 0]
  prov[is.na(core_v), core_v := 0]

  tail_q_wide <- dcast(prov_tail, COD_CNES ~ proc10, value.var = "q", fill = 0)
  tail_v_wide <- dcast(prov_tail, COD_CNES ~ proc10, value.var = "v", fill = 0)

  tail_proc_cols <- setdiff(names(tail_q_wide), "COD_CNES")

  tail_q_wide <- merge(prov[, .(COD_CNES)], tail_q_wide, by = "COD_CNES", all.x = TRUE)
  tail_v_wide <- merge(prov[, .(COD_CNES)], tail_v_wide, by = "COD_CNES", all.x = TRUE)

  tail_cols <- tail_proc_cols
  if (!length(tail_cols)) return(NULL)

  for (cc in tail_cols) {
    set(tail_q_wide, which(is.na(tail_q_wide[[cc]])), cc, 0)
    set(tail_v_wide, which(is.na(tail_v_wide[[cc]])), cc, 0)
  }

  Q_tail <- as.matrix(tail_q_wide[, ..tail_cols])
  V_tail <- as.matrix(tail_v_wide[, ..tail_cols])

  n <- nrow(prov)
  if (n < 10) return(NULL)

  tail_full <- DT_g[proc10 %in% tail_cols, .(q = sum(qtd), v = sum(vlr)), by = .(proc10)]
  tail_full[, price := v / q]
  tail_full_price <- setNames(tail_full$price, tail_full$proc10)

  default_price <- DT_g[, sum(vlr) / sum(qtd)]
  fallback_vec <- tail_full_price[tail_cols]
  fallback_vec[!is.finite(fallback_vec)] <- default_price

  price_core   <- rep(NA_real_, B)
  abs_per_unit_oob <- rep(NA_real_, B)
  overpay_frac_oob <- rep(NA_real_, B)
  underpay_frac_oob<- rep(NA_real_, B)
  oob_n <- integer(B)

  q0 <- prov$q
  v0 <- prov$v
  core_q0 <- prov$core_q

  for (b in 1:B) {
    idx <- sample.int(n, size = n, replace = TRUE)
    m <- tabulate(idx, nbins = n)

    # preços in-bag
    core_v_s <- sum(prov$core_v * m)
    core_q_s <- sum(prov$core_q * m)
    p_core <- if (core_q_s > 0) core_v_s / core_q_s else 0
    price_core[b] <- p_core

    V_tail_s <- colSums(V_tail * m, na.rm = TRUE)
    Q_tail_s <- colSums(Q_tail * m, na.rm = TRUE)

    p_tail <- V_tail_s / pmax(Q_tail_s, 1e-12)
    miss <- (!is.finite(Q_tail_s)) | (Q_tail_s <= 0)
    if (any(miss)) p_tail[miss] <- fallback_vec[miss]

    # previsão para todos
    pred_core <- p_core * core_q0
    pred_tail <- as.vector(Q_tail %*% p_tail)
    pred_v <- pred_core + pred_tail

    # OOB
    oob <- (m == 0)
    oob_n[b] <- sum(oob)
    if (oob_n[b] > 0) {
      err <- pred_v[oob] - v0[oob]
      abs_per_unit_oob[b] <- sum(abs(err)) / pmax(sum(q0[oob]), 1e-12)

      denom_v <- sum(v0[oob])
      overpay_frac_oob[b] <- sum(pmax(err, 0))  / pmax(denom_v, 1e-12)
      underpay_frac_oob[b]<- sum(pmax(-err, 0)) / pmax(denom_v, 1e-12)
    }
  }

  summarize_vec <- function(x) {
    data.table(
      mean = mean(x, na.rm = TRUE),
      p05  = as.numeric(quantile(x, 0.05, na.rm = TRUE)),
      p50  = as.numeric(quantile(x, 0.50, na.rm = TRUE)),
      p95  = as.numeric(quantile(x, 0.95, na.rm = TRUE))
    )
  }

  list(
    method = "SPLIT_TAIL",
    price_core = summarize_vec(price_core),
    abs_per_unit_oob = summarize_vec(abs_per_unit_oob),
    overpay_frac_oob = summarize_vec(overpay_frac_oob),
    underpay_frac_oob= summarize_vec(underpay_frac_oob),
    oob_n_summary = data.table(
      oob_n_mean = mean(oob_n),
      oob_n_p05  = as.numeric(quantile(oob_n, 0.05)),
      oob_n_p50  = as.numeric(quantile(oob_n, 0.50)),
      oob_n_p95  = as.numeric(quantile(oob_n, 0.95))
    ),
    tail_procs = tail_cols
  )
}

############################
# 7) RODA MONTE CARLO PARA OS GRUPOS MAIS SENSÍVEIS (ESTADO, g4)
############################
mc_results_summary <- data.table()
mc_oob_summary_by_group <- data.table()

for (g in groups_mc) {
  prov_g <- prov_state_g4[g4 == g]

  sim <- simulate_group_methods(
    prov_g,
    B = B_state,
    seed = safe_int_seed(g, base = 1000L)
  )

  out_procs <- proc_stats_g4[g4 == g & is_outlier == TRUE, unique(proc10)]
  DT_g <- dt[g4 == g]
  split_sim <- simulate_split_tail(
    DT_g,
    out_procs,
    B = B_split,
    seed = safe_int_seed(g, base = 2000L)
  )

  if (!is.null(sim)) {
    tmp <- merge(
      sim$price[, .(method, price_mean = mean, price_p05 = p05, price_p50 = p50, price_p95 = p95)],
      sim$abs_per_unit_oob[, .(method, abs_per_unit_mean = mean, abs_per_unit_p95 = p95)],
      by = "method", all = TRUE
    )
    tmp <- merge(tmp, sim$overpay_frac_oob[, .(method, overpay_frac_mean = mean, overpay_frac_p95 = p95)], by = "method", all = TRUE)
    tmp <- merge(tmp, sim$underpay_frac_oob[, .(method, underpay_frac_mean = mean, underpay_frac_p95 = p95)], by = "method", all = TRUE)
    tmp[, g4 := g]
    mc_results_summary <- rbind(mc_results_summary, tmp, fill = TRUE)

    mc_oob_summary_by_group <- rbind(
      mc_oob_summary_by_group,
      data.table(g4 = g, method = "(OOB n)", sim$oob_n_summary),
      fill = TRUE
    )
  }

  if (!is.null(split_sim)) {
    split_row <- data.table(
      g4 = g,
      method = split_sim$method,
      price_mean = split_sim$price_core$mean,
      price_p05  = split_sim$price_core$p05,
      price_p50  = split_sim$price_core$p50,
      price_p95  = split_sim$price_core$p95,
      abs_per_unit_mean = split_sim$abs_per_unit_oob$mean,
      abs_per_unit_p95  = split_sim$abs_per_unit_oob$p95,
      overpay_frac_mean = split_sim$overpay_frac_oob$mean,
      overpay_frac_p95  = split_sim$overpay_frac_oob$p95,
      underpay_frac_mean= split_sim$underpay_frac_oob$mean,
      underpay_frac_p95 = split_sim$underpay_frac_oob$p95
    )
    mc_results_summary <- rbind(mc_results_summary, split_row, fill = TRUE)

    mc_oob_summary_by_group <- rbind(
      mc_oob_summary_by_group,
      data.table(g4 = g, method = "SPLIT_TAIL (OOB n)", split_sim$oob_n_summary),
      fill = TRUE
    )
  }
}

# Ranking global (média ponderada por volume do grupo no estado)
group_weights <- sens_state_g4[g4 %in% groups_mc, .(g4, total_q)]
mc_rank <- merge(mc_results_summary, group_weights, by = "g4", all.x = TRUE)
mc_rank[, w := total_q / sum(total_q, na.rm = TRUE)]

mc_overall <- mc_rank[, .(
  w_abs_per_unit = sum(w * abs_per_unit_mean, na.rm = TRUE),
  w_overpay      = sum(w * overpay_frac_mean, na.rm = TRUE),
  w_underpay     = sum(w * underpay_frac_mean, na.rm = TRUE),
  underpay_p95_median = median(underpay_frac_p95, na.rm = TRUE)
), by = .(method)][order(w_abs_per_unit, underpay_p95_median)]

mc_best_by_group <- mc_rank[
  , .SD[order(abs_per_unit_mean, underpay_frac_mean)][1],
  by = .(g4)
]

############################
# 8) RESULTADOS PARA IMPRESSÃO (NO FINAL)
############################
data_quality <- list(
  n_rows_raw  = nrow(dt_raw),
  n_rows_used = nrow(dt),
  n_cnes      = uniqueN(dt$COD_CNES),
  n_proc10    = uniqueN(dt$proc10),
  n_g4        = uniqueN(dt$g4)
)

sens_state_g4_report <- copy(sens_state_g4)[order(-abs(delta_mean_vs_wmed))]

outlier_proc_report <- proc_stats_g4[is_outlier == TRUE][order(g4, -unit_ratio)]
outlier_topk_report <- outlier_topk_g4[order(g4, outlier_rank)]

impact_report <- impact_g4[order(-abs(impact_rel))]
impact_proc_report <- impact_proc_g4[is_outlier == TRUE][order(g4, -impact_rel, -unit_ratio)]

############################
# 9) IMPRESSÃO FINAL (TUDO NO FIM)
############################
cat("\n============================================================\n")
cat("ROBUST MONTE CARLO – PRECIFICAÇÃO SIGTAP (SÉRIE HISTÓRICA 2025)\n")
cat("============================================================\n")

cat("\n[0] Qualidade e escala dos dados\n")
print(data.table(
  n_rows_raw  = data_quality$n_rows_raw,
  n_rows_used = data_quality$n_rows_used,
  n_cnes      = data_quality$n_cnes,
  n_proc10    = data_quality$n_proc10,
  n_g4        = data_quality$n_g4
))

cat("\n[0.1] Sanity checks (QTD parse + suposição de unitário)\n")
cat("Resumo de comprimentos do COD_PROCEDIMENTO (bruto):\n")
print(sanity$proc_len_summary)

cat("\nResumo do unitário (VLR_APROVADA interpretado como unitário):\n")
print(sanity$unit_summary)

cat("\nFrações (indicadores):\n")
print(data.table(
  nonint_qtd_fraction_raw = sanity$nonint_qtd_fraction_raw,
  qtd_gt1_fraction_used   = sanity$qtd_gt1_fraction_used,
  spearman_unit_qtd       = sanity$spearman_unit_qtd
))

if (length(sanity$warnings)) {
  cat("\nWARNINGS:\n")
  cat(paste0(" - ", sanity$warnings, collapse = "\n"), "\n")
} else {
  cat("\nSem alertas fortes pelos checks heurísticos.\n")
}

cat("\n[1] Alternativas sugeridas (catálogo)\n")
cat(paste0(" - ", alternatives_catalog, collapse = "\n"), "\n")

cat("\n[2] Casos onde a MÉDIA tende a distorcer (Top 30, g4 no ESTADO)\n")
cat("Colunas importantes:\n")
cat("  delta_mean_vs_wmed: (média/mediana_pond - 1)\n")
cat("  share_vol_cost_lt_mean: % do volume com custo do CNES abaixo da média\n")
cat("  impact_rel: queda relativa estimada se remover TODOS os procedimentos outlier (tail)\n\n")
print(sens_state_g4_report[1:min(.N, 30)])

cat("\n[3] Procedimentos caros/raros detectados (candidatos a 'split-tail')\n")
if (nrow(outlier_proc_report) == 0) {
  cat("Nenhum outlier detectado com os thresholds atuais.\n")
  cat("Sugestão: reduzir outlier_k_ratio (ex.: 4) e/ou aumentar outlier_prev_max (ex.: 0.15).\n")
} else {
  cat("\n[3.1] Top-", outlier_top_k, " por g4 (unit_ratio, prevalência, q_share)\n", sep = "")
  print(outlier_topk_report)

  cat("\n[3.2] Impacto ao remover a cauda (todos outliers), por g4\n")
  print(impact_report[1:min(.N, 30), .(g4, n_outlier_procs, tail_share_q, tail_share_v, price_all, price_core, impact_abs, impact_rel)])

  cat("\n[3.3] Ranking de procedimentos outlier por impacto (removendo 1 a 1)\n")
  print(impact_proc_report[1:min(.N, 50), .(g4, proc10, impact_abs, impact_rel, unit_ratio, prevalence, q_share, unit)])
}

cat("\n[4] Monte Carlo (cluster bootstrap por CNES) – grupos mais sensíveis (Top ", top_n_groups_mc, ", g4/ESTADO)\n", sep = "")
cat("Métricas (avaliadas OUT-OF-BAG / OOB):\n")
cat("  abs_per_unit_mean: erro absoluto monetário médio por unidade no OOB (quanto menor, melhor)\n")
cat("  overpay/underpay: frações do valor total OOB com sobre/subpagamento\n")
cat("  Observação: OOB mede generalização entre prestadores, não apenas instabilidade do estimador.\n\n")

if (nrow(mc_results_summary) == 0) {
  cat("MC não gerou resultados (verifique filtros min_total_q e dados).\n")
} else {
  cat("\n[4.1] Tamanho do OOB (diagnóstico):\n")
  print(mc_oob_summary_by_group[order(g4, method)])

  cat("\n[4.2] Ranking global (ponderado pelo volume dos grupos analisados)\n")
  print(mc_overall)

  cat("\n[4.3] Melhor método por grupo (critério: abs_per_unit_mean; desempate: underpay_frac_mean)\n")
  print(
    mc_best_by_group[order(-total_q)][,
                                      .(g4, method, abs_per_unit_mean, underpay_frac_mean, overpay_frac_mean, price_mean, total_q)
    ]
  )

  cat("\n[4.4] Tabela completa (MC / OOB) – para relatório\n")
  print(mc_results_summary[order(g4, method)])
}

cat("\n============================================================\n")
cat("FIM.\n")
cat("Opinião prática:\n")
cat(" - Para governança (e para cortar o 'benefício' do não-ofertante), SPLIT_TAIL costuma ser o argumento mais defensável.\n")
cat(" - WMED/WINS/HUBER ajudam bem, mas ainda são 'uma regra estatística' em cima do mesmo pacote.\n")
cat("============================================================\n")
