#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)
options(pillar.sigfig = 6)

############################################################
# IPCA / SIDRA 1419 -> Stan (cmdstanr)
#
# Padrao do repo:
# - rodar a partir do root do repositorio
# - sem setwd()
# - coleta online via SIDRA (tabelas 1737 e 7060)
# - resultados finais em console por padrao
# - artefatos opcionais em outputs/tables/
#
# Exemplos:
#   Rscript scripts/ipca_sidra_1419/ipca_sidra_1419_cmdstanr.R
#   Rscript scripts/ipca_sidra_1419/ipca_sidra_1419_cmdstanr.R --start_period=202001 --end_period=202602
#   Rscript scripts/ipca_sidra_1419/ipca_sidra_1419_cmdstanr.R --save_report=1 --save_tables=1
############################################################

args <- commandArgs(trailingOnly = TRUE)
AUTO_INSTALL_PKGS <- "--auto-install" %in% args

parse_args <- function(args) {
  out <- list(
    start_period = "202001",
    end_period = "202602",
    seed_model = 20260318L,
    chains = 4L,
    parallel_chains = 4L,
    iter_warmup = 1000L,
    iter_sampling = 1000L,
    max_treedepth = 12L,
    adapt_delta = 0.95,
    init_mode = 0,
    refresh = 200L,
    save_report = 0L,
    save_tables = 0L,
    output_tables_dir = file.path("outputs", "tables", "ipca_sidra_1419"),
    output_prefix = "ipca_sidra_1419"
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
  out$chains <- as.integer(out$chains)
  out$parallel_chains <- as.integer(out$parallel_chains)
  out$iter_warmup <- as.integer(out$iter_warmup)
  out$iter_sampling <- as.integer(out$iter_sampling)
  out$max_treedepth <- as.integer(out$max_treedepth)
  out$adapt_delta <- as.numeric(out$adapt_delta)
  out$init_mode <- as.integer(out$init_mode)
  out$refresh <- as.integer(out$refresh)
  out$save_report <- as.integer(out$save_report)
  out$save_tables <- as.integer(out$save_tables)

  if (!grepl("^(19|20)\\d{2}(0[1-9]|1[0-2])$", out$start_period)) {
    stop("'--start_period' deve estar em YYYYMM.", call. = FALSE)
  }
  if (!grepl("^(19|20)\\d{2}(0[1-9]|1[0-2])$", out$end_period)) {
    stop("'--end_period' deve estar em YYYYMM.", call. = FALSE)
  }
  if (out$start_period > out$end_period) {
    stop("'--start_period' nao pode ser maior que '--end_period'.", call. = FALSE)
  }

  out
}

opts <- parse_args(args)

ensure_repos_for_cmdstanr <- function() {
  repos <- getOption("repos")
  if (is.null(repos) || length(repos) == 0) repos <- c()
  if (identical(repos, "@CRAN@")) repos <- c()
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
        "Pacote '", pkg, "' nao esta instalado.\n\n",
        "Como resolver (recomendado):\n",
        "  Rscript scripts/_setup/install_deps.R --all\n\n",
        "Alternativa (manual, dentro do R):\n",
        "  install.packages('", pkg, "')\n\n",
        "Se voce realmente quer auto-instalar neste script:\n",
        "  Rscript scripts/ipca_sidra_1419/ipca_sidra_1419_cmdstanr.R --auto-install\n",
        call. = FALSE
      )
    }
    message("Instalando pacote ausente: ", pkg)
    ensure_repos_for_cmdstanr()
    install.packages(pkg, dependencies = TRUE)
  }
  invisible(TRUE)
}

pkgs <- c(
  "sidrar", "dplyr", "tidyr", "stringr", "stringi", "lubridate",
  "readr", "purrr", "tibble", "janitor", "cmdstanr", "posterior"
)
invisible(lapply(pkgs, ensure_pkg))

suppressPackageStartupMessages({
  library(sidrar)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(stringi)
  library(lubridate)
  library(readr)
  library(purrr)
  library(tibble)
  library(janitor)
  library(cmdstanr)
  library(posterior)
})

cmdstan_ok <- tryCatch({
  path <- cmdstanr::cmdstan_path()
  is.character(path) && length(path) == 1 && nzchar(path)
}, error = function(e) FALSE)

if (!cmdstan_ok) {
  stop(
    "cmdstanr esta instalado, mas o CmdStan nao foi encontrado.\n",
    "Rode: Rscript scripts/_setup/install_cmdstan.R\n",
    call. = FALSE
  )
}

# -----------------------------
# 1) Parametros
# -----------------------------
start_period <- opts$start_period
end_period   <- opts$end_period
seed_model   <- opts$seed_model
chains       <- opts$chains
parallel_chains <- opts$parallel_chains
iter_warmup  <- opts$iter_warmup
iter_sampling <- opts$iter_sampling
max_treedepth <- opts$max_treedepth
adapt_delta   <- opts$adapt_delta
init_mode     <- opts$init_mode
refresh       <- opts$refresh
save_report   <- opts$save_report
save_tables   <- opts$save_tables
output_tables_dir <- opts$output_tables_dir
output_prefix <- opts$output_prefix

# Codigos corretos do c315 para indice geral + 9 grupos do IPCA
# Fonte cruzada em exemplos publicos da API SIDRA para grupos do IPCA.
group_codes <- c(
  "Indice geral"               = "7169",
  "Alimentacao e bebidas"      = "7170",
  "Habitacao"                  = "7445",
  "Artigos de residencia"      = "7486",
  "Vestuario"                  = "7558",
  "Transportes"                = "7625",
  "Saude e cuidados pessoais"  = "7660",
  "Despesas pessoais"          = "7712",
  "Educacao"                   = "7766",
  "Comunicacao"                = "7786"
)

group_levels <- names(group_codes)
group_only_levels <- setdiff(group_levels, "Indice geral")
# -----------------------------
# 2) Helpers
# -----------------------------
normalize_txt <- function(x) {
  x |>
    as.character() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_lower() |>
    stringr::str_squish()
}

strip_prefix_num <- function(x) {
  x |>
    normalize_txt() |>
    stringr::str_replace("^[0-9]+[. ]*", "") |>
    stringr::str_squish()
}

find_col <- function(df, patterns, label = NULL) {
  nms <- names(df)
  hit <- nms[stringr::str_detect(nms, stringr::regex(paste(patterns, collapse = "|"), ignore_case = TRUE))]
  if (length(hit) == 0) {
    stop("Nao encontrei coluna para: ", ifelse(is.null(label), paste(patterns, collapse = ", "), label))
  }
  hit[1]
}

detect_period_code_col <- function(df) {
  nms <- names(df)

  preferred <- c(
    nms[stringr::str_detect(nms, "^mes_codigo$|^m_es_codigo$|^periodo_codigo$")],
    nms[stringr::str_detect(nms, "mes.*codigo|periodo.*codigo")],
    nms[stringr::str_detect(nms, "^d[0-9]+c$")]
  )
  preferred <- unique(preferred)

  score_one <- function(col) {
    vals <- as.character(df[[col]])
    vals <- vals[!is.na(vals)]
    prop_yyyymm <- if (length(vals) == 0) 0 else mean(stringr::str_detect(vals, "^(19|20)\\d{2}(0[1-9]|1[0-2])$"))
    tibble::tibble(col = col, prop_yyyymm = prop_yyyymm)
  }

  scored <- purrr::map_dfr(preferred, score_one) |>
    dplyr::arrange(dplyr::desc(prop_yyyymm))

  if (nrow(scored) == 0 || scored$prop_yyyymm[[1]] < 0.8) {
    cat("\n[DEBUG] Candidatos de periodo avaliados:\n")
    print(scored)
    cat("\n[DEBUG] Nomes de colunas disponiveis:\n")
    print(nms)
    stop("Nao consegui identificar a coluna do codigo de periodo (YYYYMM).")
  }

  scored$col[[1]]
}

detect_period_label_col <- function(df, period_code_col) {
  nms <- names(df)
  preferred <- unique(c(
    nms[stringr::str_detect(nms, "^mes$|^m_es$|^periodo$")],
    nms[stringr::str_detect(nms, "mes$|periodo$")],
    sub("_codigo$", "", period_code_col),
    sub("codigo$", "", period_code_col),
    sub("_?c$", "_n", period_code_col),
    sub("_?c$", "n", period_code_col)
  ))
  preferred <- preferred[preferred %in% nms]
  if (length(preferred) == 0) return(period_code_col)
  preferred[1]
}

detect_c315_pair <- function(df) {
  nms <- names(df)

  code_candidates <- unique(c(
    nms[stringr::str_detect(nms, "^d[0-9]+_?c$")],
    nms[stringr::str_detect(nms, "c315")],
    nms[stringr::str_detect(nms, "codigo") &
          stringr::str_detect(nms, "geral|grupo|subgrupo|item|subitem|abertura|classific")]
  ))

  if (length(code_candidates) == 0) {
    cat("\n[DEBUG] Nomes de colunas disponiveis:\n")
    print(nms)
    stop("Nao encontrei colunas candidatas de codigo para c315.")
  }

  cand <- purrr::map_dfr(code_candidates, function(cc) {
    possible_name_cols <- unique(c(
      sub("_codigo$", "", cc),
      sub("codigo$", "", cc),
      sub("_?c$", "_n", cc),
      sub("_?c$", "n", cc)
    ))
    possible_name_cols <- possible_name_cols[possible_name_cols %in% nms]
    if (length(possible_name_cols) == 0) return(tibble::tibble())

    purrr::map_dfr(possible_name_cols, function(nn) {
      vals_code <- as.character(df[[cc]])
      vals_name <- strip_prefix_num(df[[nn]])

      tibble::tibble(
        code_col    = cc,
        name_col    = nn,
        hits_codes  = sum(vals_code %in% unname(group_codes), na.rm = TRUE),
        has_indice  = any(stringr::str_detect(vals_name, "^indice geral$"), na.rm = TRUE),
        has_alim    = any(stringr::str_detect(vals_name, "^alimentacao e bebidas$"), na.rm = TRUE),
        has_hab     = any(stringr::str_detect(vals_name, "^habitacao$"), na.rm = TRUE),
        n_codes     = dplyr::n_distinct(vals_code)
      )
    })
  })

  hit <- cand |>
    dplyr::mutate(score = hits_codes + has_indice + has_alim + has_hab) |>
    dplyr::arrange(dplyr::desc(score), dplyr::desc(n_codes)) |>
    dplyr::slice(1)

  if (nrow(hit) == 0 || hit$score[[1]] < 3) {
    cat("\n[DEBUG] Candidatos avaliados para c315:\n")
    print(cand)
    cat("\n[DEBUG] Nomes de colunas disponiveis:\n")
    print(nms)
    stop("Nao consegui identificar automaticamente o par de colunas da classificacao c315.")
  }

  list(code = hit$code_col[[1]], name = hit$name_col[[1]])
}

parse_sidra_value <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  readr::parse_number(as.character(x), locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
}

calc_accum12 <- function(x_pct) {
  n <- length(x_pct)
  out <- rep(NA_real_, n)
  if (n < 12) return(out)
  for (t in 12:n) {
    out[t] <- 100 * (prod(1 + x_pct[(t - 11):t] / 100) - 1)
  }
  out
}

extract_series_summary <- function(draws_df, prefix, months) {
  cols <- names(draws_df)[stringr::str_detect(names(draws_df), paste0("^", prefix, "\\["))]
  tibble::tibble(
    idx = seq_along(cols),
    mes = months,
    media = purrr::map_dbl(cols, ~ mean(draws_df[[.x]], na.rm = TRUE)),
    q05   = purrr::map_dbl(cols, ~ quantile(draws_df[[.x]], 0.05, na.rm = TRUE)),
    q50   = purrr::map_dbl(cols, ~ quantile(draws_df[[.x]], 0.50, na.rm = TRUE)),
    q95   = purrr::map_dbl(cols, ~ quantile(draws_df[[.x]], 0.95, na.rm = TRUE))
  )
}

bfmi_vec <- function(energy_vec) {
  if (length(energy_vec) < 2 || all(is.na(energy_vec))) return(NA_real_)
  num <- mean(diff(energy_vec)^2, na.rm = TRUE)
  den <- stats::var(energy_vec, na.rm = TRUE)
  if (is.na(den) || den == 0) return(NA_real_)
  num / den
}

safe_round_df <- function(df, digits = 4) {
  dplyr::mutate(df, dplyr::across(where(is.numeric), ~ round(.x, digits)))
}

recode_group <- function(code, label) {
  label_std <- strip_prefix_num(label)

  out <- dplyr::case_when(
    as.character(code) == group_codes[["Indice geral"]] ~ "Indice geral",
    as.character(code) == group_codes[["Alimentacao e bebidas"]] ~ "Alimentacao e bebidas",
    as.character(code) == group_codes[["Habitacao"]] ~ "Habitacao",
    as.character(code) == group_codes[["Artigos de residencia"]] ~ "Artigos de residencia",
    as.character(code) == group_codes[["Vestuario"]] ~ "Vestuario",
    as.character(code) == group_codes[["Transportes"]] ~ "Transportes",
    as.character(code) == group_codes[["Saude e cuidados pessoais"]] ~ "Saude e cuidados pessoais",
    as.character(code) == group_codes[["Despesas pessoais"]] ~ "Despesas pessoais",
    as.character(code) == group_codes[["Educacao"]] ~ "Educacao",
    as.character(code) == group_codes[["Comunicacao"]] ~ "Comunicacao",

    label_std == "indice geral" ~ "Indice geral",
    label_std == "alimentacao e bebidas" ~ "Alimentacao e bebidas",
    label_std == "habitacao" ~ "Habitacao",
    label_std == "artigos de residencia" ~ "Artigos de residencia",
    label_std == "vestuario" ~ "Vestuario",
    label_std == "transportes" ~ "Transportes",
    label_std == "saude e cuidados pessoais" ~ "Saude e cuidados pessoais",
    label_std == "despesas pessoais" ~ "Despesas pessoais",
    label_std == "educacao" ~ "Educacao",
    label_std == "comunicacao" ~ "Comunicacao",
    TRUE ~ NA_character_
  )

  out
}

# -----------------------------
# 3) Janela de coleta
# -----------------------------
start_date <- lubridate::ymd(paste0(start_period, "01"))
end_date   <- lubridate::ymd(paste0(end_period, "01"))
fetch_start_date <- start_date %m-% months(12)
fetch_start_period <- format(fetch_start_date, "%Y%m")

if (start_date < lubridate::ymd("2020-01-01")) {
  stop("A tabela 7060 so cobre grupos do IPCA a partir de 2020-01. Ajuste start_period para 202001 ou depois.")
}

cat("\n[1] Coletando IPCA do SIDRA com arquitetura mais robusta...\n")
cat("[1.0] Janela analitica:", start_period, "a", end_period, "\n")
cat("[1.1] Janela de coleta ampliada:", fetch_start_period, "a", end_period, "\n")

first_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  x[[1]]
}

# -----------------------------
# 4) Headline: tabela 1737 (indice geral em nivel)
# -----------------------------
api_headline <- sprintf(
  "/t/1737/n1/all/v/2266/p/%s-%s/d/v2266%%2013",
  fetch_start_period, end_period
)

cat("[2] API headline:", api_headline, "\n")

raw_headline <- sidrar::get_sidra(api = api_headline) |>
  janitor::clean_names()

cat("\n[DEBUG] Colunas headline apos clean_names():\n")
print(names(raw_headline))

col_period_code_hl <- detect_period_code_col(raw_headline)
col_period_lab_hl  <- detect_period_label_col(raw_headline, col_period_code_hl)
col_value_hl       <- find_col(raw_headline, c("^valor$"), "valor headline")

headline_full <- raw_headline |>
  dplyr::transmute(
    periodo_codigo = as.character(.data[[col_period_code_hl]]),
    periodo_rotulo = as.character(.data[[col_period_lab_hl]]),
    indice_nivel   = parse_sidra_value(.data[[col_value_hl]])
  ) |>
  dplyr::mutate(
    mes = lubridate::ymd(paste0(periodo_codigo, "01"))
  ) |>
  dplyr::arrange(mes) |>
  dplyr::mutate(
    variacao_mensal    = 100 * (indice_nivel / dplyr::lag(indice_nivel, 1) - 1),
    acumulado_12m_ibge = 100 * (indice_nivel / dplyr::lag(indice_nivel, 12) - 1)
  )

if (any(is.na(headline_full$mes))) {
  stop("Falha ao converter os periodos da serie headline.")
}

headline <- headline_full |>
  dplyr::filter(mes >= start_date, mes <= end_date) |>
  dplyr::mutate(grupo = "Indice geral") |>
  dplyr::select(mes, grupo, variacao_mensal, acumulado_12m_ibge)

cat("\n[DEBUG] Headline: primeiras linhas apos derivar variacao mensal e 12m:\n")
print(utils::head(headline, 15))

# -----------------------------
# 5) Grupos: tabela 7060 (variacao mensal + peso mensal)
# -----------------------------
fetch_one_group_7060 <- function(group_code, group_name) {
  api_group <- sprintf(
    "/t/7060/n1/all/v/63,66/p/%s-%s/c315/%s/d/v63%%202,v66%%204",
    fetch_start_period, end_period, group_code
  )

  cat("[DOWNLOAD 7060] ", sprintf("%-28s", group_name), " c315=", group_code, "\n", sep = "")
  cat("[API] ", api_group, "\n", sep = "")

  raw_g <- sidrar::get_sidra(api = api_group) |>
    janitor::clean_names()

  col_period_code <- detect_period_code_col(raw_g)
  col_period_lab  <- detect_period_label_col(raw_g, col_period_code)
  col_var_code    <- find_col(raw_g, c("^variavel_codigo$", "^variavel_cod$", "variavel.*codigo", "^v$"), "codigo da variavel")
  col_var_lab     <- find_col(raw_g, c("^variavel$", "^variavel_nome$"), "nome da variavel")
  col_value       <- find_col(raw_g, c("^valor$"), "valor")

  long_g <- raw_g |>
    dplyr::transmute(
      periodo_codigo = as.character(.data[[col_period_code]]),
      periodo_rotulo = as.character(.data[[col_period_lab]]),
      var_code       = as.character(.data[[col_var_code]]),
      variavel       = as.character(.data[[col_var_lab]]),
      valor          = parse_sidra_value(.data[[col_value]]),
      grupo          = group_name,
      grupo_code     = as.character(group_code)
    ) |>
    dplyr::mutate(
      variavel_std = normalize_txt(variavel),
      mes = lubridate::ymd(paste0(periodo_codigo, "01")),
      var_key = dplyr::case_when(
        var_code == "63" ~ "variacao_mensal",
        var_code == "66" ~ "peso_mensal",
        stringr::str_detect(variavel_std, "variacao mensal") ~ "variacao_mensal",
        stringr::str_detect(variavel_std, "peso") ~ "peso_mensal",
        TRUE ~ NA_character_
      )
    )

  if (any(is.na(long_g$mes))) {
    cat("\n[DEBUG] Periodos invalidos em ", group_name, ":\n", sep = "")
    print(dplyr::distinct(long_g, periodo_codigo, periodo_rotulo) |>
            dplyr::filter(is.na(lubridate::ymd(paste0(periodo_codigo, '01')))))
    stop("Falha ao converter periodo_codigo para mes em ", group_name)
  }

  cat("[DEBUG] Variaveis retornadas em ", group_name, ":\n", sep = "")
  print(long_g |>
          dplyr::distinct(var_code, variavel, variavel_std, var_key) |>
          dplyr::arrange(var_code, variavel))

  panel_g <- long_g |>
    dplyr::filter(!is.na(var_key)) |>
    dplyr::group_by(mes, grupo, grupo_code, var_key) |>
    dplyr::summarise(valor = first_non_na(valor), .groups = "drop") |>
    tidyr::pivot_wider(names_from = var_key, values_from = valor) |>
    dplyr::arrange(mes) |>
    dplyr::filter(mes >= start_date, mes <= end_date) |>
    dplyr::select(mes, grupo, grupo_code, variacao_mensal, peso_mensal)

  cat("[COBERTURA 7060] ", group_name, "\n", sep = "")
  print(panel_g |>
          dplyr::summarise(
            n_meses = dplyr::n(),
            n_y_na = sum(is.na(variacao_mensal)),
            n_w_na = sum(is.na(peso_mensal))
          ))

  panel_g
}

panel_groups_raw <- purrr::imap_dfr(group_codes[group_only_levels], fetch_one_group_7060)

cat("\n[DEBUG] Painel bruto dos grupos (amostra):\n")
print(utils::head(panel_groups_raw, 20))

# -----------------------------
# 6) Monta painel conjunto
# -----------------------------
base_panel <- dplyr::bind_rows(
  panel_groups_raw,
  headline |>
    dplyr::mutate(grupo_code = group_codes[["Indice geral"]], peso_mensal = NA_real_)
) |>
  dplyr::arrange(mes, grupo)

coverage_panel <- base_panel |>
  tidyr::pivot_longer(cols = c("variacao_mensal", "peso_mensal", "acumulado_12m_ibge"),
                      names_to = "var_key", values_to = "valor") |>
  dplyr::group_by(var_key, grupo) |>
  dplyr::summarise(
    n_total = dplyr::n(),
    n_na = sum(is.na(valor)),
    n_non_na = sum(!is.na(valor)),
    .groups = "drop"
  ) |>
  dplyr::arrange(var_key, grupo)

cat("\n[DEBUG] Cobertura por variavel e grupo antes do complete():\n")
tibble::as_tibble(coverage_panel) |> print(n = Inf)

missing_y_raw <- base_panel |>
  dplyr::filter(grupo != "Indice geral", is.na(variacao_mensal)) |>
  dplyr::select(mes, grupo)

missing_w_raw <- base_panel |>
  dplyr::filter(grupo != "Indice geral", is.na(peso_mensal)) |>
  dplyr::select(mes, grupo)

missing_total_raw <- base_panel |>
  dplyr::filter(grupo == "Indice geral", is.na(variacao_mensal)) |>
  dplyr::select(mes)

if (nrow(missing_y_raw) > 0) {
  cat("\n[ERRO DE COBERTURA BRUTA] Faltam variacoes mensais antes do complete().\n")
  print(missing_y_raw, n = min(50, nrow(missing_y_raw)))
  stop("A serie de nivel dos grupos nao permitiu derivar variacao_mensal em toda a janela analitica.")
}
if (nrow(missing_w_raw) > 0) {
  cat("\n[ERRO DE COBERTURA BRUTA] Faltam pesos mensais antes do complete().\n")
  print(missing_w_raw, n = min(50, nrow(missing_w_raw)))
  stop("A tabela 7060 veio sem peso_mensal em parte da janela analitica.")
}
if (nrow(missing_total_raw) > 0) {
  cat("\n[ERRO DE COBERTURA BRUTA] Faltam observacoes do indice geral antes do complete().\n")
  print(missing_total_raw, n = min(50, nrow(missing_total_raw)))
  stop("A serie headline nao permitiu derivar variacao_mensal em toda a janela analitica.")
}

# -----------------------------
# 7) Completa grade mensal e monta matrizes
# -----------------------------
all_months <- seq.Date(from = start_date, to = end_date, by = "1 month")

base_panel <- base_panel |>
  dplyr::mutate(grupo = factor(grupo, levels = group_levels)) |>
  tidyr::complete(mes = all_months, grupo = group_levels) |>
  dplyr::arrange(mes, grupo)

base_panel <- base_panel |>
  dplyr::group_by(grupo) |>
  dplyr::arrange(mes, .by_group = TRUE) |>
  dplyr::mutate(acumulado_12m_calc = calc_accum12(variacao_mensal)) |>
  dplyr::ungroup()

panel_groups <- base_panel |>
  dplyr::filter(grupo != "Indice geral")

panel_total <- base_panel |>
  dplyr::filter(grupo == "Indice geral") |>
  dplyr::arrange(mes)

wide_y <- panel_groups |>
  dplyr::select(mes, grupo, variacao_mensal) |>
  tidyr::pivot_wider(names_from = grupo, values_from = variacao_mensal) |>
  dplyr::arrange(mes) |>
  dplyr::select(mes, dplyr::all_of(group_only_levels))

wide_w <- panel_groups |>
  dplyr::select(mes, grupo, peso_mensal) |>
  tidyr::pivot_wider(names_from = grupo, values_from = peso_mensal) |>
  dplyr::arrange(mes) |>
  dplyr::select(mes, dplyr::all_of(group_only_levels))

series_total <- panel_total |>
  dplyr::select(mes, variacao_mensal, acumulado_12m_calc, acumulado_12m_ibge) |>
  dplyr::arrange(mes)

Y <- as.matrix(dplyr::select(wide_y, -mes))
W <- as.matrix(dplyr::select(wide_w, -mes))
y_total <- series_total$variacao_mensal
months_vec <- series_total$mes
accum12_total_calc <- series_total$acumulado_12m_calc
accum12_total_ibge <- series_total$acumulado_12m_ibge

stopifnot(nrow(Y) == nrow(W), length(y_total) == nrow(Y))

# Validacoes de sanidade numerica antes da normalizacao dos pesos
if (any(!is.finite(Y))) stop("Y contem valores nao finitos (Inf/-Inf/NaN).")
if (any(!is.finite(W))) stop("W contem valores nao finitos (Inf/-Inf/NaN) antes da normalizacao.")
if (any(!is.finite(y_total))) stop("y_total contem valores nao finitos (Inf/-Inf/NaN).")
if (any(W < 0, na.rm = TRUE)) stop("W contem pesos negativos, o que nao e compativel com a agregacao.")

missing_y <- panel_groups |>
  dplyr::filter(is.na(variacao_mensal)) |>
  dplyr::select(mes, grupo)
missing_w <- panel_groups |>
  dplyr::filter(is.na(peso_mensal)) |>
  dplyr::select(mes, grupo)
missing_total <- panel_total |>
  dplyr::filter(is.na(variacao_mensal)) |>
  dplyr::select(mes)

if (nrow(missing_y) > 0) {
  cat("\n[ERRO DE GRADE] Faltam variacoes mensais para alguns grupos/meses.\n")
  print(missing_y, n = min(50, nrow(missing_y)))
  stop("Ha NA em Y. Revise a cobertura reportada acima.")
}
if (nrow(missing_w) > 0) {
  cat("\n[ERRO DE GRADE] Faltam pesos mensais para alguns grupos/meses.\n")
  print(missing_w, n = min(50, nrow(missing_w)))
  stop("Ha NA em W. Revise a cobertura reportada acima.")
}
if (nrow(missing_total) > 0) {
  cat("\n[ERRO DE GRADE] Faltam observacoes no indice geral.\n")
  print(missing_total, n = min(50, nrow(missing_total)))
  stop("Ha NA na serie do indice geral.")
}

row_sum_w <- rowSums(W, na.rm = TRUE)
if (any(!is.finite(row_sum_w))) stop("A soma dos pesos possui valores nao finitos.")
if (any(row_sum_w <= 0)) stop("Ha linha de pesos com soma nao positiva.")

W <- W / row_sum_w

if (any(!is.finite(W))) stop("A normalizacao de W gerou valores nao finitos.")

stan_data <- list(
  T = nrow(Y),
  G = ncol(Y),
  y = Y,
  y_total = y_total,
  W = W
)

# -----------------------------
# 8) Validacao dos dados antes do Stan
# -----------------------------
valid_idx_12m <- which(!is.na(accum12_total_calc) & !is.na(accum12_total_ibge))
rmse_12m_data <- sqrt(mean((accum12_total_calc[valid_idx_12m] - accum12_total_ibge[valid_idx_12m])^2))
max_abs_12m_data <- max(abs(accum12_total_calc[valid_idx_12m] - accum12_total_ibge[valid_idx_12m]))

# -----------------------------
# 9) Modelo Stan (embutido; arquivo temporario)
# -----------------------------
stan_code <- "
data {
  int<lower=1> T;
  int<lower=1> G;
  matrix[T, G] y;
  vector[T] y_total;
  matrix[T, G] W;
}

parameters {
  real mu0;
  vector[T - 1] z_mu;
  real<lower=1e-6> sigma_mu;

  vector[G] alpha_g;
  real<lower=-0.80, upper=0.80> phi_eta;
  vector<lower=1e-6>[G] sigma_eta;
  matrix[T, G] z_eta;

  vector<lower=1e-6>[G] sigma_obs_g;
  real<lower=1e-6> sigma_obs_total;
}

transformed parameters {
  vector[T] mu;
  matrix[T, G] eta;
  matrix[T, G] theta;
  vector[T] total_theta;

  mu[1] = mu0;
  for (t in 2:T) {
    mu[t] = mu[t - 1] + sigma_mu * z_mu[t - 1];
  }

  for (g in 1:G) {
    // Inicializacao mais estavel que a versao com divisao por sqrt(1 - phi^2)
    eta[1, g] = alpha_g[g] + sigma_eta[g] * z_eta[1, g];
    for (t in 2:T) {
      eta[t, g] = alpha_g[g] + phi_eta * (eta[t - 1, g] - alpha_g[g]) + sigma_eta[g] * z_eta[t, g];
    }
  }

  for (t in 1:T) {
    for (g in 1:G) {
      theta[t, g] = mu[t] + eta[t, g];
    }
    total_theta[t] = dot_product(to_vector(W[t]), to_vector(theta[t]));
  }
}

model {
  mu0 ~ normal(0.4, 0.5);
  z_mu ~ std_normal();
  sigma_mu ~ normal(0, 0.20);

  alpha_g ~ normal(0, 0.35);
  phi_eta ~ normal(0, 0.30);
  to_vector(z_eta) ~ std_normal();
  sigma_eta ~ normal(0, 0.20);

  sigma_obs_g ~ exponential(3);
  sigma_obs_total ~ exponential(4);

  for (t in 1:T) {
    for (g in 1:G) {
      y[t, g] ~ normal(theta[t, g], sigma_obs_g[g]);
    }
    y_total[t] ~ normal(total_theta[t], sigma_obs_total);
  }
}

generated quantities {
  vector[T] accum12_latent;
  vector[T] effect_base_proxy;
  vector[T] log_lik_total;
  vector[T] y_total_rep;

  for (t in 1:T) {
    log_lik_total[t] = normal_lpdf(y_total[t] | total_theta[t], sigma_obs_total);
    y_total_rep[t] = normal_rng(total_theta[t], sigma_obs_total);
    accum12_latent[t] = negative_infinity();
    effect_base_proxy[t] = negative_infinity();
  }

  for (t in 12:T) {
    real gross = 1.0;
    for (k in (t - 11):t) {
      gross *= (1 + total_theta[k] / 100.0);
    }
    accum12_latent[t] = 100.0 * (gross - 1.0);
  }

  for (t in 13:T) {
    effect_base_proxy[t] = total_theta[t] - total_theta[t - 12];
  }
}
"

stan_file <- cmdstanr::write_stan_file(stan_code)

# -----------------------------
# 10) Ajuste do modelo
# -----------------------------
cat("[2] Compilando e ajustando o modelo Stan...\n")
cat("[2.1] Faixas dos dados enviados ao Stan:\n")
cat("  y_total  : [", round(min(y_total), 6), ", ", round(max(y_total), 6), "]\n", sep = "")
cat("  Y        : [", round(min(Y), 6), ", ", round(max(Y), 6), "]\n", sep = "")
cat("  W normal.: [", round(min(W), 6), ", ", round(max(W), 6), "]\n", sep = "")
mod <- cmdstanr::cmdstan_model(stan_file)

fit <- mod$sample(
  data = stan_data,
  seed = seed_model,
  init = init_mode,
  chains = chains,
  parallel_chains = parallel_chains,
  iter_warmup = iter_warmup,
  iter_sampling = iter_sampling,
  max_treedepth = max_treedepth,
  adapt_delta = adapt_delta,
  refresh = refresh
)

# -----------------------------
# 11) Diagnosticos do ajuste
# -----------------------------
param_core <- c("mu0", "sigma_mu", "phi_eta", "sigma_obs_total")
fit_summary_core <- fit$summary(variables = param_core)
fit_summary_groups <- fit$summary(variables = c("alpha_g", "sigma_eta", "sigma_obs_g"))

rhat_all <- fit$summary()$rhat
ess_bulk_all <- fit$summary()$ess_bulk
ess_tail_all <- fit$summary()$ess_tail

sampler_array <- fit$sampler_diagnostics()
sampler_names <- dimnames(sampler_array)[[3]]

get_sampler_diag <- function(name) {
  idx <- match(name, sampler_names)
  if (is.na(idx)) return(NULL)
  sampler_array[, , idx, drop = FALSE]
}

div_arr <- get_sampler_diag("divergent__")
trd_arr <- get_sampler_diag("treedepth__")
ene_arr <- get_sampler_diag("energy__")

n_divergent <- if (is.null(div_arr)) NA_integer_ else sum(div_arr)
n_max_td <- if (is.null(trd_arr)) NA_integer_ else sum(trd_arr >= max_treedepth)
max_td_seen <- if (is.null(trd_arr)) NA_integer_ else max(trd_arr)

bfmi_by_chain <- if (is.null(ene_arr)) {
  rep(NA_real_, chains)
} else {
  vapply(seq_len(dim(ene_arr)[2]), function(ch) bfmi_vec(ene_arr[, ch, 1]), numeric(1))
}

# -----------------------------
# 12) Resumos posteriores e validacao preditiva
# -----------------------------
post_draws <- fit$draws(variables = c("total_theta", "accum12_latent", "effect_base_proxy", "y_total_rep"))
post_df <- posterior::as_draws_df(post_draws)

sum_total_theta <- extract_series_summary(post_df, "total_theta", months_vec)
sum_accum12     <- extract_series_summary(post_df, "accum12_latent", months_vec)
sum_baseproxy   <- extract_series_summary(post_df, "effect_base_proxy", months_vec)
sum_yrep        <- extract_series_summary(post_df, "y_total_rep", months_vec)

valid_idx_total <- which(!is.na(y_total) & !is.na(sum_total_theta$media))
rmse_total <- sqrt(mean((y_total[valid_idx_total] - sum_total_theta$media[valid_idx_total])^2))
mae_total  <- mean(abs(y_total[valid_idx_total] - sum_total_theta$media[valid_idx_total]))

valid_idx_acc <- which(!is.na(accum12_total_calc) & !is.na(sum_accum12$media) & is.finite(sum_accum12$media))
rmse_acc12 <- sqrt(mean((accum12_total_calc[valid_idx_acc] - sum_accum12$media[valid_idx_acc])^2))
mae_acc12  <- mean(abs(accum12_total_calc[valid_idx_acc] - sum_accum12$media[valid_idx_acc]))

coverage90_total <- mean(y_total[valid_idx_total] >= sum_yrep$q05[valid_idx_total] &
                           y_total[valid_idx_total] <= sum_yrep$q95[valid_idx_total])

# -----------------------------
# 13) Perguntas-alvo da manchete
# -----------------------------
idx_fev_2026 <- which(months_vec == lubridate::ymd("2026-02-01"))
idx_jan_2026 <- which(months_vec == lubridate::ymd("2026-01-01"))
idx_fev_2025 <- which(months_vec == lubridate::ymd("2025-02-01"))

prob_report <- tibble::tibble(
  indicador = c(
    "P(total_theta_fev2026 < total_theta_fev2025)",
    "P(accum12_latent_fev2026 < accum12_latent_jan2026)",
    "P(effect_base_proxy_fev2026 < 0)"
  ),
  valor = c(
    mean(post_df[[paste0("total_theta[", idx_fev_2026, "]")]] < post_df[[paste0("total_theta[", idx_fev_2025, "]")]], na.rm = TRUE),
    mean(post_df[[paste0("accum12_latent[", idx_fev_2026, "]")]] < post_df[[paste0("accum12_latent[", idx_jan_2026, "]")]], na.rm = TRUE),
    mean(post_df[[paste0("effect_base_proxy[", idx_fev_2026, "]")]] < 0, na.rm = TRUE)
  )
)

base_effect_fev2026 <- tibble::tibble(
  metrica = c("media", "q05", "q50", "q95"),
  valor = c(
    mean(post_df[[paste0("effect_base_proxy[", idx_fev_2026, "]")]], na.rm = TRUE),
    quantile(post_df[[paste0("effect_base_proxy[", idx_fev_2026, "]")]], 0.05, na.rm = TRUE),
    quantile(post_df[[paste0("effect_base_proxy[", idx_fev_2026, "]")]], 0.50, na.rm = TRUE),
    quantile(post_df[[paste0("effect_base_proxy[", idx_fev_2026, "]")]], 0.95, na.rm = TRUE)
  )
)

# -----------------------------
# 14) Relatorio final + artefatos opcionais
# -----------------------------
last6_total <-
  safe_round_df(
    dplyr::left_join(
      tibble::tibble(mes = months_vec, observado = y_total),
      dplyr::select(sum_total_theta, mes, latente_media = media, latente_q05 = q05, latente_q95 = q95),
      by = "mes"
    ) |>
      dplyr::slice_tail(n = 6)
  )

last6_accum12 <-
  safe_round_df(
    dplyr::left_join(
      tibble::tibble(mes = months_vec, observado_12m = accum12_total_calc),
      dplyr::select(sum_accum12, mes, latente_media_12m = media, latente_q05_12m = q05, latente_q95_12m = q95),
      by = "mes"
    ) |>
      dplyr::slice_tail(n = 6)
  )

emit_final_report <- function() {
  cat("\n============================================================\n")
  cat("RELATORIO FINAL - IPCA / SIDRA 1419 / STAN\n")
  cat("============================================================\n")

  cat("\n[1] Base analitica\n")
  cat("Linhas da serie headline (tabela 1737) baixada:", nrow(raw_headline), "\n")
  cat("Meses na base modelada:", length(months_vec), "\n")
  cat("Periodo:", format(min(months_vec), "%Y-%m"), "a", format(max(months_vec), "%Y-%m"), "\n")
  cat("Grupos no modelo:", ncol(Y), "\n")
  cat("Nomes dos grupos:", paste(colnames(Y), collapse = ", "), "\n")

  cat("\n[2] Validacao da base antes do ajuste\n")
  cat("RMSE entre acumulado_12m reconstruido e acumulado_12m do IBGE:", round(rmse_12m_data, 6), "\n")
  cat("Maior diferenca absoluta nessa comparacao:", round(max_abs_12m_data, 6), "\n")

  cat("\n[3] Diagnosticos MCMC / HMC\n")
  cat("Divergencias:", n_divergent, "\n")
  cat("Iteracoes no limite de treedepth (>=", max_treedepth, "):", n_max_td, "\n", sep = "")
  cat("Maior treedepth observado:", max_td_seen, "\n")
  cat("Maior Rhat:", round(max(rhat_all, na.rm = TRUE), 4), "\n")
  cat("Menor ESS bulk:", round(min(ess_bulk_all, na.rm = TRUE), 1), "\n")
  cat("Menor ESS tail:", round(min(ess_tail_all, na.rm = TRUE), 1), "\n")
  cat("BFMI por chain:", paste(round(bfmi_by_chain, 4), collapse = ", "), "\n")

  cat("\n[4] Parametros centrais\n")
  print(safe_round_df(dplyr::select(fit_summary_core, variable, mean, median, sd, q5, q95, rhat, ess_bulk, ess_tail)))

  cat("\n[5] Desvios e ruidos por grupo\n")
  print(safe_round_df(dplyr::select(fit_summary_groups, variable, mean, median, sd, q5, q95, rhat, ess_bulk, ess_tail)))

  cat("\n[6] Validacao preditiva\n")
  cat("RMSE do IPCA mensal observado vs media posterior latente:", round(rmse_total, 4), "\n")
  cat("MAE  do IPCA mensal observado vs media posterior latente:", round(mae_total, 4), "\n")
  cat("Cobertura de 90% do posterior preditivo para o IPCA mensal:", round(coverage90_total, 4), "\n")
  cat("RMSE do acumulado em 12m observado vs media posterior latente:", round(rmse_acc12, 4), "\n")
  cat("MAE  do acumulado em 12m observado vs media posterior latente:", round(mae_acc12, 4), "\n")

  cat("\n[7] Perguntas da manchete\n")
  print(safe_round_df(prob_report))

  cat("\n[8] Efeito-base proxy em fevereiro/2026\n")
  print(safe_round_df(base_effect_fev2026))

  cat("\n[9] Ultimos 6 meses - IPCA mensal observado vs latente\n")
  print(last6_total)

  cat("\n[10] Ultimos 6 meses - acumulado em 12m observado vs latente\n")
  print(last6_accum12)

  if (save_report == 0L && save_tables == 0L) {
    cat("\nFim. Nenhum arquivo de saida foi gravado em pastas do projeto.\n")
  } else {
    cat("\nFim. Artefatos opcionais foram gravados em:", output_tables_dir, "\n")
  }
}

emit_final_report()

if (save_report == 1L || save_tables == 1L) {
  dir.create(output_tables_dir, recursive = TRUE, showWarnings = FALSE)
}

if (save_report == 1L) {
  report_path <- file.path(output_tables_dir, paste0(output_prefix, "_report.txt"))
  report_lines <- capture.output(emit_final_report(), type = "output")
  writeLines(report_lines, con = report_path, useBytes = TRUE)
  cat("[ARQUIVO] Relatorio salvo em:", report_path, "\n")
}

if (save_tables == 1L) {
  utils::write.csv(safe_round_df(fit_summary_core), file.path(output_tables_dir, paste0(output_prefix, "_core_params.csv")), row.names = FALSE)
  utils::write.csv(safe_round_df(fit_summary_groups), file.path(output_tables_dir, paste0(output_prefix, "_group_params.csv")), row.names = FALSE)
  utils::write.csv(safe_round_df(prob_report), file.path(output_tables_dir, paste0(output_prefix, "_headline_probs.csv")), row.names = FALSE)
  utils::write.csv(safe_round_df(base_effect_fev2026), file.path(output_tables_dir, paste0(output_prefix, "_effect_base_fev2026.csv")), row.names = FALSE)
  utils::write.csv(last6_total, file.path(output_tables_dir, paste0(output_prefix, "_last6_total.csv")), row.names = FALSE)
  utils::write.csv(last6_accum12, file.path(output_tables_dir, paste0(output_prefix, "_last6_accum12.csv")), row.names = FALSE)
  utils::write.csv(safe_round_df(sum_total_theta), file.path(output_tables_dir, paste0(output_prefix, "_total_theta_summary.csv")), row.names = FALSE)
  utils::write.csv(safe_round_df(sum_accum12), file.path(output_tables_dir, paste0(output_prefix, "_accum12_summary.csv")), row.names = FALSE)
  cat("[ARQUIVO] Tabelas salvas em:", output_tables_dir, "\n")
}
