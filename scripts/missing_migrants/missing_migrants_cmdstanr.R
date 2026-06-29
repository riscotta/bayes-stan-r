#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)
options(pillar.sigfig = 6)

############################################################
# IOM Missing Migrants Project — modelo NB2 hierárquico
# rota-mês com offset de exposição em dias
#
# Padrão do repo:
# - rodar a partir da raiz do repositório
# - sem setwd()
# - dados versionados em data/raw/missing_migrants/
# - modelo Stan ao lado deste script
# - saídas regeneráveis em outputs/missing_migrants/
#
# Execução padrão, sem reamostrar Stan:
#   Rscript scripts/missing_migrants/missing_migrants_cmdstanr.R
#
# Reexecução da amostragem Stan/cmdstanr:
#   Rscript scripts/missing_migrants/missing_migrants_cmdstanr.R --run_stan=1
#   RUN_STAN=true Rscript scripts/missing_migrants/missing_migrants_cmdstanr.R
############################################################

args <- commandArgs(trailingOnly = TRUE)

parse_args <- function(args) {
  out <- list(
    root = NA_character_,
    run_stan = NA_character_,
    seed = 20260626L,
    chains = 4L,
    parallel_chains = 4L,
    iter_warmup = 1000L,
    iter_sampling = 1000L,
    adapt_delta = 0.99,
    max_treedepth = 14L,
    refresh = 100L,
    output_dir = file.path("outputs", "missing_migrants")
  )

  if (length(args) == 0) return(out)

  for (a in args) {
    if (!startsWith(a, "--")) next
    a2 <- sub("^--", "", a)
    if (!grepl("=", a2, fixed = TRUE)) next
    key <- sub("=.*$", "", a2)
    val <- sub("^[^=]*=", "", a2)
    if (key %in% names(out)) out[[key]] <- val
  }

  int_fields <- c("seed", "chains", "parallel_chains", "iter_warmup", "iter_sampling", "max_treedepth", "refresh")
  dbl_fields <- c("adapt_delta")
  for (nm in int_fields) out[[nm]] <- as.integer(out[[nm]])
  for (nm in dbl_fields) out[[nm]] <- as.numeric(out[[nm]])

  out
}

opts <- parse_args(args)

script_path <- function() {
  cmd <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd, value = TRUE)
  if (length(file_arg) == 0) return(NA_character_)
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = FALSE)
}

has_repo_markers <- function(path) {
  file.exists(file.path(path, "README.md")) &&
    dir.exists(file.path(path, "scripts")) &&
    dir.exists(file.path(path, "data", "raw"))
}

find_repo_root <- function(start) {
  if (is.na(start) || !nzchar(start)) return(NA_character_)
  cur <- normalizePath(start, mustWork = FALSE)
  if (file.exists(cur) && !dir.exists(cur)) cur <- dirname(cur)
  repeat {
    if (has_repo_markers(cur)) return(cur)
    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }
  NA_character_
}

repo_root_candidates <- unique(c(opts$root, getwd(), dirname(script_path())))
repo_root <- NA_character_
for (candidate in repo_root_candidates) {
  found <- find_repo_root(candidate)
  if (!is.na(found)) {
    repo_root <- found
    break
  }
}
if (is.na(repo_root)) {
  stop("Não foi possível localizar a raiz do repositório. Rode a partir da raiz ou use --root=/caminho/do/repo.", call. = FALSE)
}

as_flag <- function(x, default = FALSE) {
  if (is.na(x) || !nzchar(x)) return(default)
  tolower(x) %in% c("1", "true", "t", "sim", "s", "yes", "y")
}

run_stan <- as_flag(opts$run_stan, default = as_flag(Sys.getenv("RUN_STAN", "false"), FALSE))
study_id <- "missing_migrants"

data_dir <- file.path(repo_root, "data", "raw", study_id)
model_input_dir <- file.path(data_dir, "model_inputs")
audited_dir <- file.path(data_dir, "resultados_auditados")
stan_dir <- file.path(repo_root, "scripts", study_id)
out_dir <- file.path(repo_root, opts$output_dir)
out_tables <- file.path(out_dir, "tables")
out_figures <- file.path(out_dir, "figures")
out_logs <- file.path(out_dir, "logs")
out_cmdstan <- file.path(out_dir, "cmdstan_csv")
out_data_stan <- file.path(out_dir, "data_stan")
for (d in c(out_tables, out_figures, out_logs, out_cmdstan, out_data_stan)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(out_logs, "missing_migrants_execucao.log")
log_msg <- function(...) {
  txt <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " - ", paste(..., collapse = ""))
  cat(txt, "\n")
  cat(txt, "\n", file = log_file, append = TRUE)
}

require_pkg <- function(pkg, optional = FALSE) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- paste0("Pacote requerido não instalado: ", pkg)
    if (optional) return(FALSE)
    stop(msg, call. = FALSE)
  }
  TRUE
}

for (pkg in c("readr", "dplyr", "tidyr", "lubridate", "stringr", "jsonlite", "ggplot2", "tibble")) require_pkg(pkg)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(stringr)
  library(jsonlite)
  library(ggplot2)
})

first_existing <- function(paths) {
  hits <- paths[file.exists(paths)]
  if (length(hits) == 0) return(NA_character_)
  hits[[1]]
}

require_file <- function(path) {
  if (!file.exists(path)) stop("Arquivo obrigatório não encontrado: ", path, call. = FALSE)
  invisible(path)
}

read_optional_csv <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  readr::read_csv(path, show_col_types = FALSE)
}

copy_if_exists <- function(src, dst_dir = out_tables) {
  if (!is.na(src) && file.exists(src)) {
    file.copy(src, file.path(dst_dir, basename(src)), overwrite = TRUE)
    return(file.path(dst_dir, basename(src)))
  }
  NA_character_
}

clean_text <- function(x) {
  x <- stringr::str_replace_all(x, "\\ufeff", "")
  x <- stringr::str_squish(x)
  dplyr::na_if(x, "")
}

std_category <- function(x, missing_label = "Unknown/Not reported") {
  x <- clean_text(x)
  z <- stringr::str_to_lower(stringr::str_trim(x))
  dplyr::if_else(
    is.na(x) | z %in% c("unknown", "unknown (unknown)", "na", "n/a", "not available", "not reported", ""),
    missing_label,
    x
  )
}

as_num <- function(x) suppressWarnings(as.numeric(x))

raw_path <- require_file(file.path(data_dir, "Missing_Migrants_Global_Figures_allData.csv"))
stan_model_path <- require_file(file.path(stan_dir, "modelo_nb_hierarquico_rota_mes_estavel.stan"))

log_msg("Início da execução. Repo root: ", repo_root)

# -----------------------------
# 1. Leitura e preparação da base
# -----------------------------
raw <- readr::read_csv(raw_path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), clean_text))

required_raw_cols <- c(
  "Main ID", "Incident ID", "Incident Type", "Region of Incident", "Incident Date", "Incident Year", "Month",
  "Number of Dead", "Minimum Estimated Number of Missing", "Total Number of Dead and Missing",
  "Country of Origin", "Region of Origin", "Cause of Death", "Country of Incident", "Migration Route",
  "Location of Incident", "Coordinates", "UNSD Geographical Grouping", "Information Source", "URL", "Source Quality"
)
missing_cols <- setdiff(required_raw_cols, names(raw))
if (length(missing_cols) > 0) {
  stop("Colunas ausentes no CSV bruto: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

base <- raw |>
  dplyr::rename(
    main_id = `Main ID`,
    incident_id = `Incident ID`,
    incident_type_raw = `Incident Type`,
    region_incident = `Region of Incident`,
    incident_date_raw = `Incident Date`,
    incident_year_raw = `Incident Year`,
    incident_month_name_raw = Month,
    number_dead_raw = `Number of Dead`,
    minimum_missing_raw = `Minimum Estimated Number of Missing`,
    total_dead_missing_raw = `Total Number of Dead and Missing`,
    country_origin_raw = `Country of Origin`,
    region_origin_raw = `Region of Origin`,
    cause_death_raw = `Cause of Death`,
    country_incident = `Country of Incident`,
    migration_route_raw = `Migration Route`,
    location_incident = `Location of Incident`,
    coordinates_raw = Coordinates,
    unsd_geographical_grouping = `UNSD Geographical Grouping`,
    information_source_raw = `Information Source`,
    url_raw = URL,
    source_quality_raw = `Source Quality`
  ) |>
  dplyr::mutate(
    incident_date = lubridate::ymd(incident_date_raw),
    incident_year = as.integer(incident_year_raw),
    number_dead_clean = pmax(as_num(number_dead_raw), 0, na.rm = TRUE),
    minimum_missing_clean = pmax(as_num(minimum_missing_raw), 0, na.rm = TRUE),
    total_dead_missing_clean = pmax(as_num(total_dead_missing_raw), 0, na.rm = TRUE),
    dead_missing_response = total_dead_missing_clean,
    migration_route = std_category(migration_route_raw),
    region_incident_std = std_category(region_incident),
    cause_death = std_category(cause_death_raw, "Mixed or unknown"),
    source_quality_primary_numeric = suppressWarnings(as.numeric(stringr::str_extract(source_quality_raw, "[1-5]"))),
    is_simple_incident = incident_type_raw == "Incident",
    ready_for_incident_level_modeling_flag = is_simple_incident & !is.na(incident_date) & !is.na(dead_missing_response),
    is_extreme_total_ge_80 = dead_missing_response >= 80,
    requires_manual_review_flag = is_extreme_total_ge_80 | stringr::str_detect(stringr::str_to_lower(incident_type_raw), "cumulative|split")
  )

readr::write_csv(base, file.path(out_tables, "base_analitica_recriada_minima.csv"))

resumo_base_recriada <- tibble::tibble(
  metrica = c(
    "registros_base_completa",
    "mortos_desaparecidos_base_completa",
    "incidentes_simples_modelaveis",
    "mortos_desaparecidos_base_modelada",
    "rotas_distintas_modeladas",
    "eventos_extremos_ge_80"
  ),
  valor = c(
    nrow(base),
    sum(base$dead_missing_response, na.rm = TRUE),
    sum(base$ready_for_incident_level_modeling_flag, na.rm = TRUE),
    sum(base$dead_missing_response[base$ready_for_incident_level_modeling_flag], na.rm = TRUE),
    dplyr::n_distinct(base$migration_route[base$ready_for_incident_level_modeling_flag]),
    sum(base$is_extreme_total_ge_80, na.rm = TRUE)
  )
)
readr::write_csv(resumo_base_recriada, file.path(out_tables, "resumo_base_recriada.csv"))

# -----------------------------
# 2. Agregação rota-mês para Stan
# -----------------------------
preparar_rota_mes <- function(dados) {
  dados <- dados |>
    dplyr::filter(ready_for_incident_level_modeling_flag) |>
    dplyr::mutate(incident_date = as.Date(incident_date))

  data_min <- lubridate::floor_date(min(dados$incident_date, na.rm = TRUE), "month")
  data_max <- max(dados$incident_date, na.rm = TRUE)
  mes_max <- lubridate::floor_date(data_max, "month")

  meses <- tibble::tibble(month_start = seq.Date(data_min, mes_max, by = "month")) |>
    dplyr::mutate(
      incident_year = lubridate::year(month_start),
      incident_month_number = lubridate::month(month_start),
      days_in_month = lubridate::days_in_month(month_start),
      days_observed_in_cell = dplyr::if_else(month_start == mes_max, lubridate::day(data_max), as.integer(days_in_month)),
      month_index = dplyr::dense_rank(month_start),
      t_years_centered = (month_index - mean(unique(month_index))) / 12
    )

  rotas <- dados |>
    dplyr::distinct(migration_route) |>
    dplyr::arrange(migration_route) |>
    dplyr::mutate(group_id = dplyr::row_number())

  agregado <- dados |>
    dplyr::mutate(month_start = lubridate::floor_date(incident_date, "month")) |>
    dplyr::group_by(month_start, migration_route) |>
    dplyr::summarise(y = sum(dead_missing_response, na.rm = TRUE), .groups = "drop")

  dados_modelo <- tidyr::crossing(meses, rotas) |>
    dplyr::left_join(agregado, by = c("month_start", "migration_route")) |>
    dplyr::mutate(y = tidyr::replace_na(y, 0L)) |>
    dplyr::arrange(group_id, month_start)

  stan_data <- list(
    N = nrow(dados_modelo),
    G = nrow(rotas),
    y = as.integer(dados_modelo$y),
    group_id = as.integer(dados_modelo$group_id),
    month_id = as.integer(dados_modelo$incident_month_number),
    t_years = dados_modelo$t_years_centered,
    exposure_days = as.numeric(dados_modelo$days_observed_in_cell)
  )

  list(dados_modelo = dados_modelo, mapa_rotas = rotas, stan_data = stan_data)
}

prep <- preparar_rota_mes(base)
readr::write_csv(prep$dados_modelo, file.path(out_data_stan, "dados_modelo_rota_mes_recriado.csv"))
readr::write_csv(prep$mapa_rotas, file.path(out_data_stan, "mapa_rotas_recriado.csv"))
jsonlite::write_json(prep$stan_data, file.path(out_data_stan, "stan_data_rota_mes_recriado.json"), pretty = TRUE, auto_unbox = TRUE)

# Mantém cópia das entradas congeladas da E06 quando presentes, para comparação rápida.
for (f in c("E06_dados_modelo_rota_mes.csv", "E06_mapa_rotas.csv", "E06_stan_data_rota_mes.json")) {
  copy_if_exists(file.path(model_input_dir, f), out_data_stan)
}

# -----------------------------
# 3. Modelo Stan e ajuste opcional
# -----------------------------
if (run_stan) {
  require_pkg("cmdstanr")
  require_pkg("posterior")

  log_msg("Reamostragem Stan iniciada.")
  fit <- cmdstanr::cmdstan_model(stan_model_path, force_recompile = TRUE)$sample(
    data = prep$stan_data,
    seed = opts$seed,
    chains = opts$chains,
    parallel_chains = opts$parallel_chains,
    iter_warmup = opts$iter_warmup,
    iter_sampling = opts$iter_sampling,
    adapt_delta = opts$adapt_delta,
    max_treedepth = opts$max_treedepth,
    output_dir = out_cmdstan,
    refresh = opts$refresh
  )

  readr::write_csv(fit$summary(), file.path(out_tables, "resumo_posterior_cmdstan_reestimado.csv"))
  readr::write_csv(fit$sampler_diagnostics(format = "df"), file.path(out_tables, "sampler_diagnostics_cmdstan_reestimado.csv"))
  log_msg("Reamostragem Stan concluída.")
}

# -----------------------------
# 4. Tabelas finais a partir dos artefatos auditados
# -----------------------------
resumo_posterior_path <- first_existing(c(
  file.path(audited_dir, "resumo_posterior_cmdstan.csv"),
  file.path(out_tables, "resumo_posterior_cmdstan_reestimado.csv")
))
diag_path <- first_existing(c(
  file.path(audited_dir, "diagnosticos_mcmc.csv"),
  file.path(out_tables, "resumo_posterior_cmdstan_reestimado.csv")
))
ppc_path <- first_existing(c(file.path(audited_dir, "E08_checagem_preditiva_cmdstan_rota.csv")))
rotas_path <- first_existing(c(file.path(audited_dir, "E09_sintese_posterior_rotas_cmdstan.csv")))
sazonalidade_path <- first_existing(c(file.path(audited_dir, "E09_sintese_sazonalidade_cmdstan.csv")))
tendencia_path <- first_existing(c(file.path(audited_dir, "E09_sintese_tendencia_global_cmdstan.csv")))
resumo_diag_path <- first_existing(c(file.path(audited_dir, "E08_resumo_diagnostico_mcmc.csv")))

for (f in c(
  "E10_resultados_chave.csv", "E10_top_rotas_posterior.csv", "E10_reprodutibilidade.csv",
  "E08_checagem_preditiva_cmdstan_rota.csv", "E08_resumo_diagnostico_mcmc.csv",
  "E09_sintese_posterior_rotas_cmdstan.csv", "E09_sintese_sazonalidade_cmdstan.csv",
  "E09_sintese_tendencia_global_cmdstan.csv", "configuracao_amostragem.json",
  "resumo_posterior_parametros_principais_cmdstan.csv", "sampler_diagnostics_cmdstan.csv"
)) {
  copy_if_exists(file.path(audited_dir, f), if (tools::file_ext(f) == "json") out_logs else out_tables)
}

rotas <- read_optional_csv(rotas_path)
if (!is.null(rotas)) {
  top_rotas <- rotas |>
    dplyr::arrange(dplyr::desc(taxa_diaria_mediana)) |>
    dplyr::slice_head(n = 10)
  readr::write_csv(top_rotas, file.path(out_tables, "top10_rotas_taxa_diaria.csv"))

  p_rotas <- top_rotas |>
    dplyr::mutate(rota = reorder(rota, taxa_diaria_mediana)) |>
    ggplot(aes(x = rota, y = taxa_diaria_mediana, ymin = taxa_diaria_q05, ymax = taxa_diaria_q95)) +
    geom_pointrange() +
    coord_flip() +
    labs(
      title = "Missing Migrants: rotas com maior taxa diária posterior",
      subtitle = "Mediana posterior e intervalo 5%-95% por rota",
      x = NULL,
      y = "Mortos e desaparecidos por dia"
    ) +
    theme_minimal(base_size = 11)
  ggsave(file.path(out_figures, "grafico_top10_rotas_taxa_diaria.png"), p_rotas, width = 9, height = 5, dpi = 160)
}

ppc <- read_optional_csv(ppc_path)
if (!is.null(ppc)) {
  p_ppc <- ppc |>
    dplyr::select(estatistica, observado, y_rep_mediana) |>
    tidyr::pivot_longer(cols = c(observado, y_rep_mediana), names_to = "serie", values_to = "valor") |>
    ggplot(aes(x = estatistica, y = valor, fill = serie)) +
    geom_col(position = "dodge") +
    coord_flip() +
    labs(
      title = "Checagem preditiva posterior",
      subtitle = "Comparação entre estatísticas observadas e mediana de y_rep",
      x = NULL,
      y = "Valor"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.title = element_blank())
  ggsave(file.path(out_figures, "grafico_ppc_observado_vs_yrep.png"), p_ppc, width = 9, height = 5, dpi = 160)
}

sazonalidade <- read_optional_csv(sazonalidade_path)
if (!is.null(sazonalidade)) {
  readr::write_csv(
    sazonalidade |>
      dplyr::arrange(mes),
    file.path(out_tables, "sintese_sazonalidade_cmdstan.csv")
  )
}

if (!is.na(tendencia_path) && !is.na(ppc_path) && !is.na(diag_path)) {
  tendencia <- readr::read_csv(tendencia_path, show_col_types = FALSE)
  ppc <- readr::read_csv(ppc_path, show_col_types = FALSE)
  diag <- readr::read_csv(diag_path, show_col_types = FALSE)
  resumo_diag <- read_optional_csv(resumo_diag_path)

  divergencias <- 0
  if (!is.null(resumo_diag) && all(c("metrica", "valor") %in% names(resumo_diag))) {
    val <- resumo_diag$valor[resumo_diag$metrica == "divergencias_total"]
    if (length(val) > 0) divergencias <- suppressWarnings(as.numeric(val[[1]]))
  }

  fechamento <- tibble::tibble(
    item = c(
      "RR anual global mediano",
      "RR anual global q05",
      "RR anual global q95",
      "Probabilidade posterior RR > 1",
      "R-hat máximo",
      "ESS bulk mínimo",
      "Divergências",
      "PPC com falha"
    ),
    valor = c(
      tendencia$mediana[1],
      tendencia$q05[1],
      tendencia$q95[1],
      tendencia$prob_maior_1[1],
      max(diag$rhat, na.rm = TRUE),
      min(diag$ess_bulk, na.rm = TRUE),
      divergencias,
      paste(ppc$estatistica[ppc$avaliacao == "falha_ppc"], collapse = "; ")
    )
  )
  readr::write_csv(fechamento, file.path(out_tables, "fechamento_resultados.csv"))
}

indice_saida <- tibble::tibble(
  arquivo = list.files(out_dir, recursive = TRUE, full.names = FALSE, all.files = FALSE),
  tamanho_bytes = file.info(list.files(out_dir, recursive = TRUE, full.names = TRUE, all.files = FALSE))$size
) |>
  dplyr::arrange(arquivo)
readr::write_csv(indice_saida, file.path(out_tables, "indice_saidas_geradas.csv"))

log_msg("Script final concluído. Saídas em: ", out_dir)
