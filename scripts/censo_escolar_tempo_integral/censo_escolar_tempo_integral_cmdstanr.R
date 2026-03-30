#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)
options(pillar.sigfig = 6)

############################################################
# Censo Escolar 2021-2025 -> tempo integral na rede publica
#
# Padrao do repo:
# - rodar a partir do root do repositorio
# - sem setwd()
# - baixa os ZIPs oficiais do INEP quando necessario
# - usa o dicionario oficial dentro de cada ZIP para validar colunas
# - agrega por ano x UF x dependencia administrativa x etapa
# - ajusta um modelo binomial multinivel via cmdstanr
# - imprime relatorio consolidado no console
#
# Exemplos:
#   Rscript scripts/censo_escolar_tempo_integral/censo_escolar_tempo_integral_cmdstanr.R
#   Rscript scripts/censo_escolar_tempo_integral/censo_escolar_tempo_integral_cmdstanr.R --years=2021:2025
#   Rscript scripts/censo_escolar_tempo_integral/censo_escolar_tempo_integral_cmdstanr.R --force_download=1
#   Rscript scripts/censo_escolar_tempo_integral/censo_escolar_tempo_integral_cmdstanr.R --save_report=1 --save_tables=1
############################################################

args <- commandArgs(trailingOnly = TRUE)
AUTO_INSTALL_PKGS <- "--auto-install" %in% args

parse_args <- function(args) {
  out <- list(
    years = "2021:2025",
    seed = 42L,
    chains = 4L,
    parallel_chains = 4L,
    iter_warmup = 1000L,
    iter_sampling = 1000L,
    adapt_delta = 0.95,
    max_treedepth = 12L,
    refresh = 100L,
    cleanup_temp = 1L,
    keep_base_ana = 0L,
    download_dir = file.path("data", "raw", "censo_escolar"),
    force_download = 0L,
    download_timeout = 7200L,
    save_report = 0L,
    save_tables = 0L,
    output_tables_dir = file.path("outputs", "tables", "censo_escolar_tempo_integral"),
    output_prefix = "censo_escolar_tempo_integral"
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
    "seed", "chains", "parallel_chains", "iter_warmup", "iter_sampling",
    "max_treedepth", "refresh", "cleanup_temp", "keep_base_ana",
    "force_download", "download_timeout", "save_report", "save_tables"
  )
  dbl_fields <- c("adapt_delta")

  for (nm in int_fields) out[[nm]] <- as.integer(out[[nm]])
  for (nm in dbl_fields) out[[nm]] <- as.numeric(out[[nm]])

  if (is.na(out$chains) || out$chains < 1L) stop("'--chains' deve ser >= 1.", call. = FALSE)
  if (is.na(out$parallel_chains) || out$parallel_chains < 1L) stop("'--parallel_chains' deve ser >= 1.", call. = FALSE)
  if (is.na(out$iter_warmup) || out$iter_warmup < 0L) stop("'--iter_warmup' invalido.", call. = FALSE)
  if (is.na(out$iter_sampling) || out$iter_sampling < 1L) stop("'--iter_sampling' invalido.", call. = FALSE)
  if (is.na(out$adapt_delta) || out$adapt_delta <= 0 || out$adapt_delta >= 1) {
    stop("'--adapt_delta' deve estar em (0, 1).", call. = FALSE)
  }
  if (is.na(out$max_treedepth) || out$max_treedepth < 1L) stop("'--max_treedepth' invalido.", call. = FALSE)
  if (is.na(out$download_timeout) || out$download_timeout < 60L) stop("'--download_timeout' deve ser >= 60.", call. = FALSE)

  out
}

parse_years_spec <- function(x) {
  x <- gsub("\\s+", "", as.character(x))
  if (!nzchar(x)) stop("'--years' nao pode ser vazio.", call. = FALSE)

  if (grepl("^[0-9]{4}:[0-9]{4}$", x)) {
    lim <- as.integer(strsplit(x, ":", fixed = TRUE)[[1]])
    if (lim[1] > lim[2]) stop("'--years' em formato intervalo deve ser crescente.", call. = FALSE)
    return(seq.int(lim[1], lim[2]))
  }

  vals <- unlist(strsplit(x, ",", fixed = TRUE), use.names = FALSE)
  yrs <- suppressWarnings(as.integer(vals))
  if (length(yrs) == 0 || any(is.na(yrs))) {
    stop("'--years' deve ser uma lista YYYY separada por virgula ou um intervalo YYYY:YYYY.", call. = FALSE)
  }
  unique(yrs)
}

opts <- parse_args(args)
years <- parse_years_spec(opts$years)

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
        "  Rscript scripts/censo_escolar_tempo_integral/censo_escolar_tempo_integral_cmdstanr.R --auto-install\n",
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
  "data.table", "dplyr", "stringr", "purrr", "tidyr",
  "tibble", "readr", "readxl", "cmdstanr", "posterior"
)
invisible(lapply(pkgs, ensure_pkg))

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tidyr)
  library(tibble)
  library(readr)
  library(readxl)
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

message_line <- function(...) cat(paste0(..., "\n"))

official_landing_page <- "https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-escolar"
official_zip_urls <- c(
  `2021` = "https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2021.zip",
  `2022` = "https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2022.zip",
  `2023` = "https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2023.zip",
  `2024` = "https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2024.zip",
  `2025` = "https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2025_.zip"
)

if (!all(as.character(years) %in% names(official_zip_urls))) {
  stop(
    "Anos fora do mapeamento oficial atual: ",
    paste(setdiff(as.character(years), names(official_zip_urls)), collapse = ", "),
    call. = FALSE
  )
}

strip_accents <- function(x) {
  x <- enc2utf8(as.character(x))
  iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
}

clean_text <- function(x) {
  x |>
    strip_accents() |>
    tolower() |>
    trimws()
}

normalize_name <- function(x) {
  x <- basename(x)
  x <- clean_text(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

parse_flag01 <- function(x) {
  x_chr <- clean_text(x)
  x_num <- suppressWarnings(as.integer(x_chr))
  out <- ifelse(!is.na(x_num), ifelse(x_num == 1L, 1L, 0L), NA_integer_)
  out[is.na(out) & x_chr %in% c("sim", "s", "yes", "y", "true")] <- 1L
  out[is.na(out) & x_chr %in% c("nao", "n", "no", "false")] <- 0L
  out
}

parse_dependencia <- function(x) {
  x_chr <- clean_text(x)
  x_num <- suppressWarnings(as.integer(x_chr))

  out <- rep(NA_character_, length(x_chr))
  out[!is.na(x_num) & x_num == 1L] <- "Federal"
  out[!is.na(x_num) & x_num == 2L] <- "Estadual"
  out[!is.na(x_num) & x_num == 3L] <- "Municipal"
  out[!is.na(x_num) & x_num == 4L] <- "Privada"

  out[is.na(out) & str_detect(x_chr, "federal")] <- "Federal"
  out[is.na(out) & str_detect(x_chr, "estad")] <- "Estadual"
  out[is.na(out) & str_detect(x_chr, "municip")] <- "Municipal"
  out[is.na(out) & str_detect(x_chr, "privad")] <- "Privada"

  out
}

download_one_zip <- function(year, url, destfile, force_download = FALSE, timeout_sec = 7200L) {
  if (file.exists(destfile) && !isTRUE(force_download)) {
    message_line("[download] reutilizando cache do ano ", year, ": ", destfile)
    return(invisible(destfile))
  }

  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(old_timeout, timeout_sec, 60L))

  if (file.exists(destfile) && isTRUE(force_download)) {
    unlink(destfile, force = TRUE)
  }

  message_line("[download] baixando microdados do ano ", year, " ...")
  ok <- tryCatch(
    {
      utils::download.file(
        url = url,
        destfile = destfile,
        mode = "wb",
        method = if (capabilities("libcurl")) "libcurl" else "auto",
        quiet = FALSE
      )
      TRUE
    },
    error = function(e) {
      message_line("[download] falha com metodo padrao para ", year, ": ", conditionMessage(e))
      FALSE
    }
  )

  if (!ok && .Platform$OS.type == "windows") {
    ok <- tryCatch(
      {
        utils::download.file(
          url = url,
          destfile = destfile,
          mode = "wb",
          method = "wininet",
          quiet = FALSE
        )
        TRUE
      },
      error = function(e) {
        message_line("[download] falha com wininet para ", year, ": ", conditionMessage(e))
        FALSE
      }
    )
  }

  if (!ok || !file.exists(destfile) || file.info(destfile)$size <= 0) {
    stop(
      "Falha ao baixar o ZIP oficial do ano ", year, ".\n",
      "URL: ", url, "\n",
      "Verifique conexao de rede, acesso ao dominio do INEP e permissao de escrita em: ", dirname(destfile),
      call. = FALSE
    )
  }

  invisible(destfile)
}

extract_zip_robust <- function(zip_path, exdir) {
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)

  last_error <- NULL

  ok <- tryCatch(
    {
      utils::unzip(zipfile = zip_path, exdir = exdir)
      TRUE
    },
    error = function(e) {
      last_error <<- conditionMessage(e)
      FALSE
    }
  )
  if (ok) return(invisible(TRUE))

  if (requireNamespace("archive", quietly = TRUE)) {
    ok <- tryCatch(
      {
        archive::archive_extract(zip_path, dir = exdir)
        TRUE
      },
      error = function(e) {
        last_error <<- paste(last_error, conditionMessage(e), sep = " | ")
        FALSE
      }
    )
    if (ok) return(invisible(TRUE))
  }

  if (.Platform$OS.type == "windows") {
    zip_win <- normalizePath(zip_path, winslash = "\\", mustWork = TRUE)
    exdir_win <- normalizePath(exdir, winslash = "\\", mustWork = TRUE)

    ps_cmd <- paste0(
      "Expand-Archive -LiteralPath '", gsub("'", "''", zip_win),
      "' -DestinationPath '", gsub("'", "''", exdir_win),
      "' -Force"
    )

    ps_out <- tryCatch(
      system2(
        "powershell",
        c("-NoProfile", "-NonInteractive", "-Command", ps_cmd),
        stdout = TRUE,
        stderr = TRUE
      ),
      error = function(e) {
        structure(conditionMessage(e), status = 1L)
      }
    )

    ps_status <- attr(ps_out, "status")
    if (is.null(ps_status) || identical(ps_status, 0L)) {
      return(invisible(TRUE))
    }

    last_error <- paste(last_error, paste(ps_out, collapse = " "), sep = " | ")
  }

  stop(
    paste(
      "Falha ao extrair o ZIP do Censo Escolar.",
      "Tente instalar o pacote opcional 'archive' ou rode em Windows com PowerShell disponivel.",
      if (!is.null(last_error)) paste("Detalhes:", last_error) else NULL
    ),
    call. = FALSE
  )
}

extract_local_zip <- function(year, zip_path, extract_dir) {
  year_dir <- file.path(extract_dir, paste0("ano_", year))
  dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)

  extract_zip_robust(zip_path, exdir = year_dir)

  files <- list.files(year_dir, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  files <- files[file.info(files)$isdir %in% FALSE]
  Encoding(files) <- "UTF-8"
  files
}

find_file_by_patterns <- function(files, patterns, label, allow_multiple = FALSE) {
  base <- clean_text(basename(files))
  idx <- Reduce(`|`, lapply(patterns, function(p) grepl(p, base, ignore.case = TRUE)))
  hits <- files[idx]

  if (length(hits) == 0) {
    stop("Nao encontrei ", label, " apos extrair o ZIP.", call. = FALSE)
  }

  hits <- hits[order(nchar(hits), hits)]

  if (allow_multiple) return(hits)
  hits[1]
}

read_header_names <- function(path) {
  first_line <- readLines(path, n = 1, warn = FALSE, encoding = "Latin-1")
  if (!length(first_line)) return(character(0))
  hdr <- strsplit(first_line, ";", fixed = TRUE)[[1]]
  trimws(gsub('^"|"$', "", hdr))
}

read_selected_csv <- function(path, cols) {
  data.table::fread(
    path,
    sep = ";",
    encoding = "Latin-1",
    select = cols,
    showProgress = FALSE
  )
}

find_dictionary_file <- function(files) {
  base <- basename(files)
  idx_excel <- grepl("\\.(xlsx|xls)$", base, ignore.case = TRUE) & !grepl("^~\\$", base)
  hits <- files[idx_excel]

  if (length(hits) == 0) {
    stop("Nao encontrei o arquivo Excel do dicionario apos extrair o ZIP.", call. = FALSE)
  }

  norm <- normalize_name(hits)
  preferred <- hits[grepl("dicion|dados", norm, ignore.case = TRUE)]
  if (length(preferred) > 0) hits <- preferred

  hits <- hits[order(nchar(hits), hits)]
  hits[1]
}

coerce_int <- function(x) suppressWarnings(as.integer(as.character(x)))

dictionary_sheet_patterns <- list(
  unified = c("^microdados_unidade_coleta$", "^cadastro_escolas$"),
  escola = c("^tabela_de_escola$", "^tabela_escola$"),
  matricula = c("^tabela_de_matricula$", "^tabela_matricula$")
)

pick_dictionary_sheet <- function(sheet_names, table_key) {
  norm <- clean_text(sheet_names)
  norm <- gsub("[^a-z0-9]+", "_", norm)
  norm <- gsub("^_+|_+$", "", norm)

  patterns <- dictionary_sheet_patterns[[table_key]]
  for (p in patterns) {
    idx <- which(grepl(p, norm, ignore.case = TRUE))
    if (length(idx) > 0) return(sheet_names[idx[1]])
  }

  stop(
    "Nao encontrei a aba do dicionario para a tabela `", table_key, "`.",
    call. = FALSE
  )
}

read_dictionary_varnames <- function(dict_xlsx, table_key) {
  sheets <- readxl::excel_sheets(dict_xlsx)
  sheet <- pick_dictionary_sheet(sheets, table_key)

  raw <- readxl::read_excel(
    path = dict_xlsx,
    sheet = sheet,
    col_names = FALSE
  )

  if (ncol(raw) < 2) {
    stop("O dicionario nao possui a coluna esperada 'Nome da Variavel'.", call. = FALSE)
  }

  vars <- raw[[2]] |>
    as.character() |>
    trimws() |>
    toupper()

  vars <- vars[grepl("^[A-Z0-9_]+$", vars)]
  unique(vars)
}

validate_required_vars <- function(required_vars, dict_vars, header_vars, table_label, year) {
  missing_dict <- setdiff(required_vars, dict_vars)
  if (length(missing_dict) > 0) {
    stop(
      paste0(
        "As seguintes colunas esperadas nao aparecem no dicionario oficial de ",
        table_label, " no ano ", year, ": ",
        paste(missing_dict, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  missing_header <- setdiff(required_vars, header_vars)
  if (length(missing_header) > 0) {
    stop(
      paste0(
        "As seguintes colunas esperadas nao aparecem no arquivo de dados de ",
        table_label, " no ano ", year, ": ",
        paste(missing_header, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

uf_lookup <- tibble::tribble(
  ~co_uf, ~uf,
  11L, "RO", 12L, "AC", 13L, "AM", 14L, "RR", 15L, "PA", 16L, "AP", 17L, "TO",
  21L, "MA", 22L, "PI", 23L, "CE", 24L, "RN", 25L, "PB", 26L, "PE", 27L, "AL", 28L, "SE", 29L, "BA",
  31L, "MG", 32L, "ES", 33L, "RJ", 35L, "SP",
  41L, "PR", 42L, "SC", 43L, "RS",
  50L, "MS", 51L, "MT", 52L, "GO", 53L, "DF"
)

stage_map <- tibble::tribble(
  ~etapa, ~total_var, ~integral_var,
  "Creche", "QT_MAT_INF_CRE", "QT_MAT_INF_CRE_INT",
  "Pre-escola", "QT_MAT_INF_PRE", "QT_MAT_INF_PRE_INT",
  "Ensino fundamental - anos iniciais", "QT_MAT_FUND_AI", "QT_MAT_FUND_AI_INT",
  "Ensino fundamental - anos finais", "QT_MAT_FUND_AF", "QT_MAT_FUND_AF_INT",
  "Ensino medio", "QT_MAT_MED", "QT_MAT_MED_INT"
)

school_required_vars <- c(
  "CO_ENTIDADE", "SG_UF", "CO_UF", "CO_MUNICIPIO", "TP_DEPENDENCIA",
  "IN_MEDIACAO_PRESENCIAL", "IN_MEDIACAO_SEMIPRESENCIAL", "IN_MEDIACAO_EAD",
  "IN_REGULAR"
)

matricula_required_vars <- c(
  "CO_ENTIDADE",
  stage_map$total_var,
  stage_map$integral_var
)

build_school_table <- function(dt_school) {
  tibble(
    school_id = as.character(dt_school$CO_ENTIDADE),
    sg_uf = as.character(dt_school$SG_UF),
    co_uf = coerce_int(dt_school$CO_UF),
    co_municipio = as.character(dt_school$CO_MUNICIPIO),
    dependencia = parse_dependencia(dt_school$TP_DEPENDENCIA),
    med_pres = parse_flag01(dt_school$IN_MEDIACAO_PRESENCIAL),
    med_semi = parse_flag01(dt_school$IN_MEDIACAO_SEMIPRESENCIAL),
    med_ead = parse_flag01(dt_school$IN_MEDIACAO_EAD),
    regular_flag = parse_flag01(dt_school$IN_REGULAR)
  ) |>
    mutate(
      co_uf = if_else(
        is.na(co_uf) & !is.na(co_municipio),
        suppressWarnings(as.integer(substr(stringr::str_pad(co_municipio, width = 7, side = "left", pad = "0"), 1, 2))),
        co_uf
      )
    ) |>
    left_join(uf_lookup, by = "co_uf") |>
    mutate(
      uf = dplyr::coalesce(sg_uf, uf),
      regular_flag = dplyr::coalesce(regular_flag, 1L)
    ) |>
    select(school_id, uf, dependencia, med_pres, med_semi, med_ead, regular_flag) |>
    distinct()
}

build_stage_long <- function(dt_counts) {
  purrr::map_dfr(seq_len(nrow(stage_map)), function(i) {
    tibble(
      school_id = as.character(dt_counts$CO_ENTIDADE),
      etapa = stage_map$etapa[i],
      n_total = coerce_int(dt_counts[[stage_map$total_var[i]]]),
      n_tempo_integral = coerce_int(dt_counts[[stage_map$integral_var[i]]])
    )
  })
}

finalize_year_from_school_counts <- function(year, school_tbl, counts_tbl, keep_base_ana = FALSE) {
  stage_long <- build_stage_long(counts_tbl)

  base <- stage_long |>
    left_join(school_tbl, by = "school_id") |>
    mutate(
      ano = year,
      n_total = dplyr::coalesce(n_total, 0L),
      n_tempo_integral = dplyr::coalesce(n_tempo_integral, 0L),
      n_tempo_integral = pmin(n_tempo_integral, n_total),
      presencial = dplyr::coalesce(med_pres, 1L)
    ) |>
    filter(n_total > 0L)

  base_ana <- base |>
    filter(dependencia %in% c("Federal", "Estadual", "Municipal")) |>
    filter(dplyr::coalesce(presencial, 1L) == 1L) |>
    filter(dplyr::coalesce(regular_flag, 1L) == 1L) |>
    filter(!is.na(etapa)) |>
    filter(!is.na(uf)) |>
    filter(n_total > 0L)

  panel_year <- base_ana |>
    group_by(ano, uf, dependencia, etapa) |>
    summarise(
      n_total = sum(n_total, na.rm = TRUE),
      n_tempo_integral = sum(n_tempo_integral, na.rm = TRUE),
      prop_tempo_integral = n_tempo_integral / n_total,
      .groups = "drop"
    )

  national_year <- base_ana |>
    summarise(
      ano = year,
      n_total = sum(n_total, na.rm = TRUE),
      n_tempo_integral = sum(n_tempo_integral, na.rm = TRUE),
      pct_tempo_integral = 100 * n_tempo_integral / n_total
    )

  stage_br_year <- base_ana |>
    group_by(ano, etapa) |>
    summarise(
      n_total = sum(n_total, na.rm = TRUE),
      n_tempo_integral = sum(n_tempo_integral, na.rm = TRUE),
      pct_tempo_integral = 100 * n_tempo_integral / n_total,
      .groups = "drop"
    )

  list(
    base_ana = if (isTRUE(keep_base_ana)) base_ana else NULL,
    base_ana_n = nrow(base_ana),
    panel_year = panel_year,
    national_year = national_year,
    stage_br_year = stage_br_year
  )
}

process_year <- function(year, zip_paths, extract_dir, keep_base_ana = FALSE) {
  files <- extract_local_zip(year, zip_paths[[as.character(year)]], extract_dir)

  dict_xlsx <- find_dictionary_file(files)

  if (year <= 2024) {
    data_file <- find_file_by_patterns(
      files = files,
      patterns = c(paste0("microdados_ed_basica_", year, "\\.csv$")),
      label = paste0("arquivo principal de microdados de ", year)
    )

    dict_vars <- read_dictionary_varnames(dict_xlsx, table_key = "unified")
    header_vars <- read_header_names(data_file)
    required_vars <- unique(c(school_required_vars, matricula_required_vars))

    validate_required_vars(
      required_vars = required_vars,
      dict_vars = dict_vars,
      header_vars = header_vars,
      table_label = "microdados_unidade_coleta",
      year = year
    )

    dt <- read_selected_csv(data_file, cols = required_vars)

    school_tbl <- build_school_table(dt[, ..school_required_vars])
    counts_tbl <- dt[, ..matricula_required_vars]

    return(finalize_year_from_school_counts(
      year = year,
      school_tbl = school_tbl,
      counts_tbl = counts_tbl,
      keep_base_ana = keep_base_ana
    ))
  }

  escola_file <- find_file_by_patterns(
    files = files,
    patterns = c("tabela_escola_2025\\.csv$", "tabela_de_escola_2025\\.csv$"),
    label = "Tabela_Escola_2025"
  )

  matricula_file <- find_file_by_patterns(
    files = files,
    patterns = c("tabela_matricula_2025\\.csv$", "tabela_de_matricula_2025\\.csv$"),
    label = "Tabela_Matricula_2025"
  )

  dict_vars_escola <- read_dictionary_varnames(dict_xlsx, table_key = "escola")
  dict_vars_matricula <- read_dictionary_varnames(dict_xlsx, table_key = "matricula")

  header_escola <- read_header_names(escola_file)
  header_matricula <- read_header_names(matricula_file)

  validate_required_vars(
    required_vars = school_required_vars,
    dict_vars = dict_vars_escola,
    header_vars = header_escola,
    table_label = "Tabela_Escola",
    year = year
  )

  validate_required_vars(
    required_vars = matricula_required_vars,
    dict_vars = dict_vars_matricula,
    header_vars = header_matricula,
    table_label = "Tabela_Matricula",
    year = year
  )

  dt_school <- read_selected_csv(escola_file, cols = school_required_vars)
  dt_counts <- read_selected_csv(matricula_file, cols = matricula_required_vars)

  school_tbl <- build_school_table(dt_school)
  finalize_year_from_school_counts(
    year = year,
    school_tbl = school_tbl,
    counts_tbl = dt_counts,
    keep_base_ana = keep_base_ana
  )
}

dir.create(opts$download_dir, recursive = TRUE, showWarnings = FALSE)

zip_paths <- stats::setNames(
  file.path(opts$download_dir, paste0("microdados_censo_escolar_", years, ".zip")),
  years
)

for (yr in years) {
  expected_path <- zip_paths[[as.character(yr)]]
  url <- official_zip_urls[[as.character(yr)]]

  if (yr == 2025L) {
    expected_path <- file.path(opts$download_dir, "microdados_censo_escolar_2025_.zip")
    zip_paths[[as.character(yr)]] <- expected_path
  }

  download_one_zip(
    year = yr,
    url = url,
    destfile = expected_path,
    force_download = isTRUE(opts$force_download == 1L),
    timeout_sec = opts$download_timeout
  )
}

work_dir <- file.path(
  tempdir(),
  paste0("censo_escolar_tempo_integral_", format(Sys.time(), "%Y%m%d_%H%M%S"))
)
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

if (isTRUE(opts$cleanup_temp == 1L)) {
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)
}

extract_dir <- file.path(work_dir, "extract")
dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)

all_years <- purrr::map(
  years,
  process_year,
  zip_paths = zip_paths,
  extract_dir = extract_dir,
  keep_base_ana = isTRUE(opts$keep_base_ana == 1L)
)

base_ana_all <- if (isTRUE(opts$keep_base_ana == 1L)) {
  bind_rows(purrr::map(all_years, "base_ana"))
} else {
  NULL
}
base_ana_all_n <- sum(purrr::map_int(all_years, "base_ana_n"))

panel <- bind_rows(purrr::map(all_years, "panel_year"))
res_national <- bind_rows(purrr::map(all_years, "national_year"))
res_stage_br <- bind_rows(purrr::map(all_years, "stage_br_year"))

etapa_levels <- c(
  "Creche",
  "Pre-escola",
  "Educacao infantil - unificada",
  "Ensino fundamental - anos iniciais",
  "Ensino fundamental - anos finais",
  "Ensino medio",
  "Outras etapas regulares"
)
etapa_levels <- etapa_levels[etapa_levels %in% unique(panel$etapa)]

dep_levels <- c("Federal", "Estadual", "Municipal")
year_levels <- sort(unique(panel$ano))
uf_levels <- sort(unique(panel$uf))

panel_stan <- panel |>
  mutate(
    etapa = factor(etapa, levels = etapa_levels),
    dependencia = factor(dependencia, levels = dep_levels),
    ano = as.integer(ano),
    uf = factor(uf, levels = uf_levels)
  ) |>
  arrange(ano, uf, dependencia, etapa) |>
  mutate(
    uf_id = as.integer(uf),
    dep_id = as.integer(dependencia),
    etapa_id = as.integer(etapa),
    ano_id = match(ano, year_levels)
  )

invalid_cells <- panel_stan |>
  mutate(
    problema = dplyr::case_when(
      is.na(uf_id) ~ "uf_id ausente",
      is.na(dep_id) ~ "dep_id ausente",
      is.na(etapa_id) ~ "etapa_id ausente",
      is.na(ano_id) ~ "ano_id ausente",
      is.na(n_total) ~ "n_total ausente",
      is.na(n_tempo_integral) ~ "n_tempo_integral ausente",
      n_total <= 0 ~ "n_total <= 0",
      n_tempo_integral < 0 ~ "n_tempo_integral < 0",
      n_tempo_integral > n_total ~ "n_tempo_integral > n_total",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(problema))

if (nrow(invalid_cells) > 0) {
  message_line("\n[ERRO DE CONSISTENCIA] Ha celulas invalidas no painel que impedem o ajuste do Stan.")
  print(invalid_cells |> select(ano, uf, dependencia, etapa, n_total, n_tempo_integral, problema))
  stop("Corrija as celulas invalidas antes de rodar o modelo Stan.", call. = FALSE)
}

prop_global_obs <- sum(panel_stan$n_tempo_integral) / sum(panel_stan$n_total)
prop_global_obs <- min(max(prop_global_obs, 1e-6), 1 - 1e-6)
alpha_init <- qlogis(prop_global_obs)

stan_data <- list(
  N = nrow(panel_stan),
  K_uf = length(uf_levels),
  K_dep = length(dep_levels),
  K_etapa = length(etapa_levels),
  K_ano = length(year_levels),
  y = as.integer(panel_stan$n_tempo_integral),
  n = as.integer(panel_stan$n_total),
  uf = as.integer(panel_stan$uf_id),
  dep = as.integer(panel_stan$dep_id),
  etapa = as.integer(panel_stan$etapa_id),
  ano = as.integer(panel_stan$ano_id),
  alpha_prior_mean = as.numeric(alpha_init)
)

stopifnot(
  is.finite(stan_data$N),
  is.finite(stan_data$K_uf),
  is.finite(stan_data$K_dep),
  is.finite(stan_data$K_etapa),
  is.finite(stan_data$K_ano),
  all(is.finite(stan_data$y)),
  all(is.finite(stan_data$n)),
  all(is.finite(stan_data$uf)),
  all(is.finite(stan_data$dep)),
  all(is.finite(stan_data$etapa)),
  all(is.finite(stan_data$ano)),
  all(stan_data$y >= 0L),
  all(stan_data$n > 0L),
  all(stan_data$y <= stan_data$n),
  all(stan_data$uf >= 1L & stan_data$uf <= stan_data$K_uf),
  all(stan_data$dep >= 1L & stan_data$dep <= stan_data$K_dep),
  all(stan_data$etapa >= 1L & stan_data$etapa <= stan_data$K_etapa),
  all(stan_data$ano >= 1L & stan_data$ano <= stan_data$K_ano),
  is.finite(stan_data$alpha_prior_mean)
)

stan_code <- "
data {
  int<lower=1> N;
  int<lower=1> K_uf;
  int<lower=1> K_dep;
  int<lower=1> K_etapa;
  int<lower=1> K_ano;
  array[N] int<lower=0> y;
  array[N] int<lower=1> n;
  array[N] int<lower=1, upper=K_uf> uf;
  array[N] int<lower=1, upper=K_dep> dep;
  array[N] int<lower=1, upper=K_etapa> etapa;
  array[N] int<lower=1, upper=K_ano> ano;
  real alpha_prior_mean;
}
parameters {
  real alpha;
  vector[K_uf] b_uf;
  vector[K_dep] b_dep;
  vector[K_etapa] b_etapa;
  vector[K_ano] b_ano;
  real<lower=1e-6> sigma_uf;
  real<lower=1e-6> sigma_dep;
  real<lower=1e-6> sigma_etapa;
  real<lower=1e-6> sigma_ano;
}
model {
  vector[N] eta;

  alpha ~ normal(alpha_prior_mean, 0.75);

  b_uf ~ normal(0, sigma_uf);
  b_dep ~ normal(0, sigma_dep);
  b_etapa ~ normal(0, sigma_etapa);
  b_ano ~ normal(0, sigma_ano);

  sigma_uf ~ normal(0, 0.25);
  sigma_dep ~ normal(0, 0.25);
  sigma_etapa ~ normal(0, 0.25);
  sigma_ano ~ normal(0, 0.25);

  for (i in 1:N) {
    eta[i] = alpha + b_uf[uf[i]] + b_dep[dep[i]] + b_etapa[etapa[i]] + b_ano[ano[i]];
  }

  y ~ binomial_logit(n, eta);
}
generated quantities {
  vector[N] p;
  vector[K_ano] br_prop;

  for (i in 1:N) {
    p[i] = inv_logit(alpha + b_uf[uf[i]] + b_dep[dep[i]] + b_etapa[etapa[i]] + b_ano[ano[i]]);
  }

  for (t in 1:K_ano) {
    real num = 0;
    real den = 0;
    for (i in 1:N) {
      if (ano[i] == t) {
        num += n[i] * p[i];
        den += n[i];
      }
    }
    br_prop[t] = num / den;
  }
}
"

init_fun <- function() {
  list(
    alpha = as.numeric(alpha_init),
    b_uf = rep(0, stan_data$K_uf),
    b_dep = rep(0, stan_data$K_dep),
    b_etapa = rep(0, stan_data$K_etapa),
    b_ano = rep(0, stan_data$K_ano),
    sigma_uf = 0.10,
    sigma_dep = 0.10,
    sigma_etapa = 0.10,
    sigma_ano = 0.10
  )
}

stan_file <- file.path(work_dir, "censo_escolar_tempo_integral.stan")
writeLines(stan_code, con = stan_file, useBytes = TRUE)

parallel_chains <- min(
  opts$parallel_chains,
  opts$chains,
  max(1L, parallel::detectCores(logical = FALSE))
)

mod <- cmdstanr::cmdstan_model(
  stan_file = stan_file,
  compile = TRUE,
  quiet = FALSE,
  pedantic = FALSE
)

fit <- mod$sample(
  data = stan_data,
  seed = opts$seed,
  init = init_fun,
  chains = opts$chains,
  parallel_chains = parallel_chains,
  iter_warmup = opts$iter_warmup,
  iter_sampling = opts$iter_sampling,
  adapt_delta = opts$adapt_delta,
  max_treedepth = opts$max_treedepth,
  refresh = opts$refresh,
  show_messages = TRUE,
  show_exceptions = TRUE,
  diagnostics = c("divergences", "treedepth", "ebfmi"),
  output_dir = NULL,
  save_latent_dynamics = FALSE
)

all_summary <- fit$summary()
core_summary <- fit$summary(variables = c("alpha", "sigma_uf", "sigma_dep", "sigma_etapa", "sigma_ano", "br_prop"))

diagnostic_summary <- tryCatch(
  fit$diagnostic_summary(),
  error = function(e) NULL
)

extract_sampler_metric <- function(sdiag, var_name) {
  dn <- dimnames(sdiag)
  if (is.null(dn) || length(dn) < 3) return(NULL)
  var_dim <- dn[[length(dn)]]
  if (is.null(var_dim) || !(var_name %in% var_dim)) return(NULL)
  idx <- match(var_name, var_dim)
  arr <- sdiag[, , idx, drop = TRUE]
  if (length(dim(arr)) == 1) {
    arr <- matrix(arr, ncol = 1)
  }
  arr
}

sdiag <- fit$sampler_diagnostics()

n_divergent <- {
  x <- extract_sampler_metric(sdiag, "divergent__")
  if (is.null(x)) NA_integer_ else sum(x > 0, na.rm = TRUE)
}

n_max_treedepth <- {
  td <- extract_sampler_metric(sdiag, "treedepth__")
  if (is.null(td)) {
    NA_integer_
  } else {
    sum(td >= opts$max_treedepth, na.rm = TRUE)
  }
}

energy_matrix <- extract_sampler_metric(sdiag, "energy__")
chain_ebfmi <- if (is.null(energy_matrix)) {
  rep(NA_real_, opts$chains)
} else {
  apply(energy_matrix, 2, function(e) {
    e <- as.numeric(e)
    if (length(e) < 2 || is.na(stats::var(e)) || stats::var(e) == 0) return(NA_real_)
    mean(diff(e)^2) / stats::var(e)
  })
}

max_rhat <- suppressWarnings(max(all_summary$rhat, na.rm = TRUE))
min_ess_bulk <- suppressWarnings(min(all_summary$ess_bulk, na.rm = TRUE))
min_ess_tail <- suppressWarnings(min(all_summary$ess_tail, na.rm = TRUE))

br_prop_summary <- fit$summary(
  variables = "br_prop"
) |>
  mutate(
    ano = year_levels,
    pct_media = 100 * mean,
    pct_mediana = 100 * median,
    pct_q5 = 100 * q5,
    pct_q95 = 100 * q95
  ) |>
  select(ano, pct_media, pct_mediana, pct_q5, pct_q95, rhat, ess_bulk, ess_tail)

observed_vs_fitted <- fit$summary(
  variables = "p"
) |>
  bind_cols(
    panel_stan |>
      transmute(
        ano, uf, dependencia, etapa,
        n_total,
        n_tempo_integral,
        prop_observada = n_tempo_integral / n_total
      )
  ) |>
  mutate(
    prop_ajustada = mean,
    erro = prop_observada - prop_ajustada,
    erro_abs = abs(erro),
    erro_quad = erro^2,
    peso = n_total / sum(n_total)
  )

fit_quality <- tibble(
  rmse_naive = sqrt(mean(observed_vs_fitted$erro_quad)),
  rmse_ponderado = sqrt(sum(observed_vs_fitted$peso * observed_vs_fitted$erro_quad)),
  mae_naive = mean(observed_vs_fitted$erro_abs),
  mae_ponderado = sum(observed_vs_fitted$peso * observed_vs_fitted$erro_abs),
  correlacao = suppressWarnings(stats::cor(observed_vs_fitted$prop_observada, observed_vs_fitted$prop_ajustada))
)

br_draws <- fit$draws(variables = "br_prop", format = "draws_df")
prob_br_2025_gt_25 <- {
  idx_2025 <- match(2025L, year_levels)
  col_2025 <- if (is.na(idx_2025)) NA_character_ else paste0("br_prop[", idx_2025, "]")
  if (!is.na(col_2025) && col_2025 %in% names(br_draws)) {
    mean(br_draws[[col_2025]] > 0.25)
  } else {
    NA_real_
  }
}

save_tables <- isTRUE(opts$save_tables == 1L)
save_report <- isTRUE(opts$save_report == 1L)

if (save_tables || save_report) {
  dir.create(opts$output_tables_dir, recursive = TRUE, showWarnings = FALSE)
}

if (save_tables) {
  readr::write_csv(res_national, file.path(opts$output_tables_dir, paste0(opts$output_prefix, "_res_national.csv")))
  readr::write_csv(res_stage_br, file.path(opts$output_tables_dir, paste0(opts$output_prefix, "_res_stage_br.csv")))
  readr::write_csv(panel, file.path(opts$output_tables_dir, paste0(opts$output_prefix, "_panel.csv")))
  readr::write_csv(br_prop_summary, file.path(opts$output_tables_dir, paste0(opts$output_prefix, "_br_prop_summary.csv")))
  readr::write_csv(core_summary, file.path(opts$output_tables_dir, paste0(opts$output_prefix, "_core_summary.csv")))
  readr::write_csv(fit_quality, file.path(opts$output_tables_dir, paste0(opts$output_prefix, "_fit_quality.csv")))
  readr::write_csv(observed_vs_fitted, file.path(opts$output_tables_dir, paste0(opts$output_prefix, "_observed_vs_fitted.csv")))
}

print_report <- function() {
  message_line("\n============================================================")
  message_line("RELATORIO FINAL - CENSO ESCOLAR / TEMPO INTEGRAL / STAN")
  message_line("============================================================")

  message_line("\n[0] Fontes oficiais")
  message_line("Pagina do INEP: ", official_landing_page)
  for (yr in years) {
    message_line("  - ", yr, ": ", official_zip_urls[[as.character(yr)]])
  }

  message_line("\n[1] Base analitica")
  message_line("Linhas analiticas (base_ana): ", format(base_ana_all_n, big.mark = ".", decimal.mark = ","))
  message_line("Linhas no painel: ", format(nrow(panel_stan), big.mark = ".", decimal.mark = ","))
  message_line("UFs: ", length(uf_levels))
  message_line("Dependencias publicas: ", length(dep_levels), " -> ", paste(dep_levels, collapse = ", "))
  message_line("Etapas: ", length(etapa_levels), " -> ", paste(etapa_levels, collapse = ", "))
  message_line("Anos: ", paste(year_levels, collapse = ", "))
  message_line("Base analitica detalhada mantida em memoria: ", ifelse(isTRUE(opts$keep_base_ana == 1L), "SIM", "NAO"))

  message_line("\n[2] Replica observada da manchete")
  print(res_national)

  message_line("\n[3] Observado por etapa (Brasil)")
  print(res_stage_br |> arrange(ano, factor(etapa, levels = etapa_levels)))

  message_line("\n[4] Resumo do ajuste")
  message_line("Cadeias: ", opts$chains)
  message_line("Parallel chains: ", parallel_chains)
  message_line("Iter warmup por cadeia: ", opts$iter_warmup)
  message_line("Iter sampling por cadeia: ", opts$iter_sampling)
  message_line("adapt_delta: ", opts$adapt_delta)
  message_line("max_treedepth: ", opts$max_treedepth)

  message_line("\n[5] Diagnosticos essenciais")
  message_line("Maximo R-hat: ", format(round(max_rhat, 4), nsmall = 4))
  message_line("Minimo ESS bulk: ", format(round(min_ess_bulk, 1), nsmall = 1))
  message_line("Minimo ESS tail: ", format(round(min_ess_tail, 1), nsmall = 1))
  message_line("Divergencias: ", ifelse(is.na(n_divergent), "NA", format(n_divergent, big.mark = ".")))
  message_line("Hits de treedepth maximo: ", ifelse(is.na(n_max_treedepth), "NA", format(n_max_treedepth, big.mark = ".")))
  message_line("E-BFMI por cadeia: ", paste(format(round(chain_ebfmi, 3), nsmall = 3), collapse = ", "))

  if (!is.null(diagnostic_summary)) {
    message_line("\n[5.1] diagnostic_summary() do cmdstanr")
    print(diagnostic_summary)
  }

  message_line("\n[6] Resumo dos hiperparametros e proporcoes Brasil por ano")
  print(core_summary)

  message_line("\n[7] Percentual latente do Brasil por ano (posterior)")
  print(br_prop_summary)

  message_line("\n[8] Probabilidade posterior de o Brasil superar 25% em 2025")
  message_line(
    "P(brasil_2025 > 25%) = ",
    ifelse(is.na(prob_br_2025_gt_25), "NA", format(round(prob_br_2025_gt_25, 4), nsmall = 4))
  )

  message_line("\n[9] Qualidade de ajuste no nivel das celulas agregadas")
  print(fit_quality)

  message_line("\n[10] Top 20 celulas com maior erro absoluto")
  print(
    observed_vs_fitted |>
      select(ano, uf, dependencia, etapa, n_total, prop_observada, prop_ajustada, erro_abs) |>
      arrange(desc(erro_abs)) |>
      slice_head(n = 20)
  )

  message_line("\n[11] Saidas em memoria")
  message_line("- base_ana_all: ", ifelse(is.null(base_ana_all), "NULL (use --keep_base_ana=1 para manter)", "objeto mantido"))
  message_line("- panel")
  message_line("- panel_stan")
  message_line("- stan_data")
  message_line("- stan_code")
  message_line("- fit_quality")
  message_line("- core_summary")
  message_line("- br_prop_summary")
  message_line("- observed_vs_fitted")

  message_line("\n[12] Arquivos e cache")
  message_line("Diretorio de download/cache: ", normalizePath(opts$download_dir, winslash = "/", mustWork = FALSE))
  message_line("Diretorio temporario de trabalho: ", work_dir)
  message_line("Remocao automatica do temporario ao final: ", ifelse(isTRUE(opts$cleanup_temp == 1L), "SIM", "NAO"))

  if (save_tables || save_report) {
    message_line("Diretorio de saida: ", normalizePath(opts$output_tables_dir, winslash = "/", mustWork = FALSE))
  }
}

print_report()

if (save_report) {
  report_path <- file.path(opts$output_tables_dir, paste0(opts$output_prefix, "_report.txt"))
  utils::capture.output(print_report(), file = report_path)
  message_line("\n[13] Relatorio salvo em: ", report_path)
}

if (save_tables) {
  message_line("[14] Tabelas salvas em: ", opts$output_tables_dir)
}

results <- list(
  years = years,
  official_landing_page = official_landing_page,
  official_zip_urls = official_zip_urls[as.character(years)],
  base_ana_all = base_ana_all,
  base_ana_all_n = base_ana_all_n,
  res_national = res_national,
  res_stage_br = res_stage_br,
  panel = panel,
  panel_stan = panel_stan,
  stan_data = stan_data,
  stan_code = stan_code,
  core_summary = core_summary,
  br_prop_summary = br_prop_summary,
  diagnostic_summary = diagnostic_summary,
  fit_quality = fit_quality,
  observed_vs_fitted = observed_vs_fitted,
  prob_br_2025_gt_25 = prob_br_2025_gt_25,
  chain_ebfmi = chain_ebfmi,
  n_divergent = n_divergent,
  n_max_treedepth = n_max_treedepth,
  max_rhat = max_rhat,
  min_ess_bulk = min_ess_bulk,
  min_ess_tail = min_ess_tail
)

rm(fit, mod, sdiag, br_draws, all_summary)
gc(verbose = FALSE)

invisible(results)
