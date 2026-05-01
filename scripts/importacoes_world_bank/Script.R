#!/usr/bin/env Rscript

# ==============================================================================
# SCRIPT.R FINAL - Importacoes anuais por pais
# Projeto: analise estatistica das importacoes anuais por pais
# Data de fechamento: 2026-05-01
#
# Uso:
#   Rscript scripts/importacoes_world_bank/Script.R
#
# A raiz do projeto deve conter, no minimo:
#   data/raw/importacoes_world_bank/world_bank_Import_Usd_enriched.csv
#   scripts/importacoes_world_bank/modelo_principal_hierarquico_student_t.stan
#
# Observacao: a etapa STAN exige R, cmdstanr e CmdStan funcionais.
# Este script foi consolidado para usar caminhos relativos e nao gerar .rds por padrao.
# ==============================================================================


# =============================
# E04 - Preparacao da base
# =============================
# Etapa 04 — Preparação e construção da base analítica
# Projeto: importações anuais por país (World Bank enriquecido)
# Objetivo: limpar, padronizar, enriquecer e exportar a base analítica
# Requisitos: apenas base R
# Uso sugerido:
#   Rscript script_etapa_04_preparacao.R "/caminho/da/pasta_do_projeto"
# Se nenhum argumento for informado, o script usa o diretório de trabalho atual.

args <- commandArgs(trailingOnly = TRUE)
project_dir <- if (length(args) >= 1) normalizePath(args[1], winslash = "/", mustWork = FALSE) else getwd()

input_csv <- file.path(project_dir, "data", "raw", "importacoes_world_bank", "world_bank_Import_Usd_enriched.csv")
output_dir <- file.path(project_dir, "outputs", "tables", "importacoes_world_bank", "E04_Preparacao_Base_Analitica")

if (!file.exists(input_csv)) {
  stop("Arquivo de entrada não encontrado: ", input_csv)
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

clean_names <- function(x) {
  x <- trimws(x)
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

message_line <- function(...) cat(paste0(..., "\n"))

message_line("Lendo arquivo bruto: ", input_csv)
raw <- read.csv(input_csv, stringsAsFactors = FALSE, check.names = FALSE)

names(raw) <- clean_names(names(raw))

# trim em colunas texto
for (j in seq_along(raw)) {
  if (is.character(raw[[j]])) {
    raw[[j]] <- trimws(raw[[j]])
    raw[[j]][raw[[j]] == ""] <- NA_character_
  }
}

# conversões principais
raw$year <- suppressWarnings(as.integer(raw$year))
raw$imports_current_usd <- suppressWarnings(as.numeric(gsub(",", "", raw$imports_current_usd, fixed = TRUE)))

# ordenação do painel
ord <- order(raw$country, raw$year)
raw <- raw[ord, , drop = FALSE]

# country_id estável
country_levels <- sort(unique(raw$country))
country_lookup <- data.frame(
  country = country_levels,
  country_id = seq_along(country_levels),
  stringsAsFactors = FALSE
)

base <- merge(raw, country_lookup, by = "country", all.x = TRUE, sort = FALSE)
base <- base[order(base$country_id, base$year), , drop = FALSE]

year_min <- min(base$year, na.rm = TRUE)
year_max <- max(base$year, na.rm = TRUE)

split_country <- split(base, base$country)

profile_list <- lapply(split_country, function(g) {
  obs_years <- g$year[!is.na(g$imports_current_usd)]
  n_total <- nrow(g)
  n_obs <- length(obs_years)

  has_any <- n_obs > 0
  complete_missing <- n_obs == 0
  complete_series <- n_obs == n_total

  if (has_any) {
    first_obs <- min(obs_years)
    last_obs <- max(obs_years)
    internal_gap_years <- length(setdiff(seq.int(first_obs, last_obs), obs_years))
    has_initial_missing <- first_obs > year_min
    has_final_missing <- last_obs < year_max
  } else {
    first_obs <- NA_integer_
    last_obs <- NA_integer_
    internal_gap_years <- 0L
    has_initial_missing <- FALSE
    has_final_missing <- FALSE
  }

  pattern <- if (complete_missing) {
    "complete_missing"
  } else if (complete_series) {
    "complete_series"
  } else if (internal_gap_years > 0) {
    "internal_gaps"
  } else if (has_initial_missing && !has_final_missing) {
    "only_initial_missing"
  } else if (has_final_missing && !has_initial_missing) {
    "only_final_missing"
  } else {
    "mixed_partial"
  }

  data.frame(
    country = g$country[1],
    country_has_any_import = has_any,
    country_complete_missing_flag = complete_missing,
    country_complete_series_flag = complete_series,
    country_has_initial_missing = has_initial_missing,
    country_has_final_missing = has_final_missing,
    country_has_internal_gap = internal_gap_years > 0,
    country_internal_gap_years = internal_gap_years,
    country_n_years_observed = n_obs,
    country_pct_years_observed = round(100 * n_obs / n_total, 2),
    first_observed_year = first_obs,
    last_observed_year = last_obs,
    country_missing_pattern = pattern,
    stringsAsFactors = FALSE
  )
})

profiles <- do.call(rbind, profile_list)

base <- merge(base, profiles, by = "country", all.x = TRUE, sort = FALSE)
base <- base[order(base$country_id, base$year), , drop = FALSE]

# variáveis derivadas
base$observed_import_flag <- !is.na(base$imports_current_usd)
base$imports_usd_billion <- base$imports_current_usd / 1e9
base$log_import <- ifelse(!is.na(base$imports_current_usd) & base$imports_current_usd > 0,
                          log(base$imports_current_usd), NA_real_)
base$year_index <- base$year - year_min
base$decade <- floor(base$year / 10) * 10
base$period_5y <- floor((base$year - year_min) / 5) * 5 + year_min
base$geo_cluster_rebuilt <- paste(base$continent, base$income_group, sep = "_")
base$geo_cluster_matches_rebuilt <- base$geo_cluster == base$geo_cluster_rebuilt
base$classification_static_assumption_flag <- TRUE
base$flag_window_1994_2023 <- base$year >= 1994 & base$year <= 2023
base$flag_window_2000_2023 <- base$year >= 2000 & base$year <= 2023

full_cols <- c(
  "country_id","country","year","year_index","decade","period_5y",
  "imports_current_usd","imports_usd_billion","log_import","observed_import_flag",
  "continent","subregion","income_group","development_status","geo_cluster",
  "geo_cluster_rebuilt","geo_cluster_matches_rebuilt","classification_static_assumption_flag",
  "country_has_any_import","country_complete_missing_flag","country_complete_series_flag",
  "country_has_initial_missing","country_has_final_missing","country_has_internal_gap",
  "country_internal_gap_years","country_n_years_observed","country_pct_years_observed",
  "first_observed_year","last_observed_year","country_missing_pattern",
  "flag_window_1994_2023","flag_window_2000_2023"
)

base_analitica_ampla <- base[, full_cols]

# base modelável: apenas observações válidas da resposta
drop_cols_model <- c("geo_cluster", "geo_cluster_rebuilt", "geo_cluster_matches_rebuilt", "country_complete_missing_flag")
keep_model <- base_analitica_ampla$observed_import_flag & !is.na(base_analitica_ampla$imports_current_usd) & base_analitica_ampla$imports_current_usd > 0
base_analitica_modelavel <- base_analitica_ampla[keep_model, setdiff(names(base_analitica_ampla), drop_cols_model), drop = FALSE]

# dicionário
desc <- c(
  country_id = "Identificador numérico estável do país, ordenado alfabeticamente por country.",
  country = "Nome textual do país conforme arquivo recebido.",
  year = "Ano da observação.",
  year_index = "Índice temporal iniciado em 0 no ano mínimo observado (1974).",
  decade = "Década calendário do ano.",
  period_5y = "Bloco quinquenal iniciado no ano mínimo da série.",
  imports_current_usd = "Importações anuais em USD corrente, convertidas para numérico.",
  imports_usd_billion = "Importações anuais em bilhões de USD corrente.",
  log_import = "Log natural de imports_current_usd para valores positivos.",
  observed_import_flag = "Indicador de observação não ausente da variável resposta.",
  continent = "Continente do país.",
  subregion = "Sub-região do país.",
  income_group = "Grupo de renda do país, tratado como classificação estática até validação externa.",
  development_status = "Status de desenvolvimento do país, tratado como classificação estática até validação externa.",
  geo_cluster = "Variável derivada recebida no arquivo original.",
  geo_cluster_rebuilt = "Reconstrução técnica de Continent + \"_\" + Income_Group.",
  geo_cluster_matches_rebuilt = "Indicador de redundância entre geo_cluster e a reconstrução técnica.",
  classification_static_assumption_flag = "Premissa operacional de classificações estáticas no tempo.",
  country_has_any_import = "País possui ao menos uma observação de importação no período.",
  country_complete_missing_flag = "País sem qualquer observação de importação em todo o período.",
  country_complete_series_flag = "País com observação de importação em todos os anos do painel.",
  country_has_initial_missing = "Há ausências apenas ou também no início da série do país.",
  country_has_final_missing = "Há ausências apenas ou também no final da série do país.",
  country_has_internal_gap = "Há lacunas internas entre o primeiro e o último ano observado.",
  country_internal_gap_years = "Número de anos ausentes entre o primeiro e o último ano observado.",
  country_n_years_observed = "Número de anos com importação observada para o país.",
  country_pct_years_observed = "Percentual de anos observados para o país no painel bruto.",
  first_observed_year = "Primeiro ano com importação observada para o país.",
  last_observed_year = "Último ano com importação observada para o país.",
  country_missing_pattern = "Classificação operacional do padrão de ausência do país.",
  flag_window_1994_2023 = "Indicador de janela com ausência anual abaixo de 30% e exclusão do último ano ainda mais incompleto.",
  flag_window_2000_2023 = "Indicador de janela mais conservadora com ausência anual abaixo de 25% em quase toda a faixa."
)

infer_tipo <- function(x) class(x)[1]
dicionario <- data.frame(
  variavel = names(desc),
  tipo = vapply(names(desc), function(nm) {
    if (nm %in% names(base_analitica_ampla)) infer_tipo(base_analitica_ampla[[nm]]) else NA_character_
  }, character(1)),
  descricao = unname(desc),
  stringsAsFactors = FALSE
)

# log de transformações
log_transformacoes <- data.frame(
  ordem = 1:11,
  transformacao = c(
    "Leitura do CSV bruto",
    "Padronização de nomes de colunas",
    "Tratamento de strings",
    "Conversão de year",
    "Conversão de imports_current_usd",
    "Perfil de ausências por país",
    "Criação de variáveis derivadas",
    "Tratamento de Geo_Cluster",
    "Regra para países totalmente ausentes",
    "Regra para ausências parciais",
    "Premissa sobre classificações"
  ),
  regra_aplicada = c(
    "Arquivo lido preservando conteúdo textual original, inclusive cabeçalhos com espaços.",
    "Remoção de espaços laterais, conversão para minúsculas e snake_case.",
    "Trim em variáveis textuais.",
    "Conversão para inteiro com tolerância a erro.",
    "Remoção de vírgulas de milhar e conversão para numérico.",
    "Cálculo de anos observados, primeira/última observação e lacunas internas.",
    "country_id, year_index, imports_usd_billion, log_import, decade, period_5y e flags operacionais.",
    "Geo_Cluster mantido apenas na base ampla e removido da base modelável por redundância com Continent + Income_Group.",
    "Países sem qualquer importação observada são preservados na base ampla, mas não entram na base modelável observada.",
    "Ausências parciais não foram imputadas na Etapa 04; foram apenas sinalizadas por flags para decisão posterior na EDA/modelagem.",
    "Income_Group e Development_Status tratados como classificações estáticas até validação externa."
  ),
  justificativa = c(
    "Conferência inicial do material recebido.",
    "Evitar erro sintático e permitir script reproduzível.",
    "Eliminar espaços residuais nos valores categóricos.",
    "Garantir ordenação temporal e controles de faixa.",
    "Transformar a resposta em variável analítica utilizável.",
    "Registrar padrão de cobertura que afeta EDA e modelagem.",
    "Preparar base para exploração e modelagem sem repetir transformações.",
    "Evitar multicolinearidade lógica.",
    "Separar rastreabilidade de usabilidade analítica.",
    "Evitar imputação prematura sem critério substantivo.",
    "Responder à condição explícita da Etapa 03."
  ),
  stringsAsFactors = FALSE
)

resumo <- data.frame(
  indicador = c(
    "linhas_painel_bruto",
    "linhas_base_analitica_ampla",
    "linhas_base_modelavel_observada",
    "colunas_base_analitica_ampla",
    "colunas_base_modelavel_observada",
    "paises_totais",
    "paises_com_alguma_observacao",
    "paises_totalmente_ausentes",
    "paises_com_serie_completa",
    "paises_com_lacunas_internas",
    "anos_min",
    "anos_max",
    "ausencias_imports_painel_bruto",
    "pct_ausencias_imports_painel_bruto",
    "observacoes_validas_modelagem"
  ),
  valor = c(
    nrow(base),
    nrow(base_analitica_ampla),
    nrow(base_analitica_modelavel),
    ncol(base_analitica_ampla),
    ncol(base_analitica_modelavel),
    length(unique(base$country)),
    sum(profiles$country_has_any_import),
    sum(profiles$country_complete_missing_flag),
    sum(profiles$country_complete_series_flag),
    sum(profiles$country_has_internal_gap),
    year_min,
    year_max,
    sum(is.na(base$imports_current_usd)),
    round(mean(is.na(base$imports_current_usd)) * 100, 2),
    nrow(base_analitica_modelavel)
  ),
  stringsAsFactors = FALSE
)

cobertura_pais <- profiles
cobertura_pais <- merge(country_lookup, cobertura_pais, by = "country", all.x = TRUE, sort = FALSE)
cobertura_pais <- cobertura_pais[, c(
  "country_id","country","country_n_years_observed","country_pct_years_observed",
  "first_observed_year","last_observed_year","country_has_initial_missing",
  "country_has_final_missing","country_has_internal_gap","country_internal_gap_years","country_missing_pattern"
)]

# exportação
write.csv(base_analitica_ampla, file.path(output_dir, "base_analitica_ampla.csv"), row.names = FALSE, na = "")
write.csv(base_analitica_modelavel, file.path(output_dir, "base_analitica_modelavel.csv"), row.names = FALSE, na = "")
write.csv(dicionario, file.path(output_dir, "dicionario_base_analitica.csv"), row.names = FALSE, na = "")
write.csv(log_transformacoes, file.path(output_dir, "log_transformacoes_etapa_04.csv"), row.names = FALSE, na = "")
write.csv(resumo, file.path(output_dir, "resumo_base_analitica.csv"), row.names = FALSE, na = "")
write.csv(cobertura_pais, file.path(output_dir, "cobertura_pais_base_analitica.csv"), row.names = FALSE, na = "")

# RDS

# log de execução
log_file <- file.path(output_dir, "log_execucao_etapa_04.txt")
zz <- file(log_file, open = "wt", encoding = "UTF-8")
sink(zz, split = TRUE)
message_line("Etapa 04 — Preparação e construção da base analítica")
message_line("Projeto dir: ", project_dir)
message_line("Arquivo de entrada: ", input_csv)
message_line("Linhas/colunas do bruto: ", nrow(base), " / ", ncol(raw))
message_line("Linhas/colunas da base analítica ampla: ", nrow(base_analitica_ampla), " / ", ncol(base_analitica_ampla))
message_line("Linhas/colunas da base modelável observada: ", nrow(base_analitica_modelavel), " / ", ncol(base_analitica_modelavel))
message_line("Países totais: ", length(unique(base$country)))
message_line("Países com alguma observação: ", sum(profiles$country_has_any_import))
message_line("Países totalmente ausentes: ", sum(profiles$country_complete_missing_flag))
message_line("Países com lacunas internas: ", sum(profiles$country_has_internal_gap))
message_line("Etapa 04 concluída com exportação dos artefatos.")
sink()
close(zz)

message_line("Arquivos gerados em: ", output_dir)



# =============================
# E05 - Analise exploratoria
# =============================
# Etapa 05 — Análise exploratória e leitura inicial dos dados
# Projeto: importações anuais por país (World Bank enriquecido)
# Objetivo: produzir estatísticas descritivas, tabelas, gráficos e leituras exploratórias
# Requisitos: apenas base R
# Uso sugerido:
#   Rscript script_etapa_05_eda.R "/caminho/da/pasta_do_projeto"
# Se nenhum argumento for informado, o script usa o diretório de trabalho atual.

args <- commandArgs(trailingOnly = TRUE)
project_dir <- if (length(args) >= 1) normalizePath(args[1], winslash = "/", mustWork = FALSE) else getwd()

input_model <- file.path(project_dir, "outputs", "tables", "importacoes_world_bank", "E04_Preparacao_Base_Analitica", "base_analitica_modelavel.csv")
input_full <- file.path(project_dir, "outputs", "tables", "importacoes_world_bank", "E04_Preparacao_Base_Analitica", "base_analitica_ampla.csv")
output_dir <- file.path(project_dir, "outputs", "tables", "importacoes_world_bank", "E05_EDA")
fig_dir <- file.path(project_dir, "outputs", "figures", "importacoes_world_bank", "E05_EDA")
tab_dir <- output_dir

if (!file.exists(input_model)) stop("Base modelável não encontrada: ", input_model)
if (!file.exists(input_full)) stop("Base ampla não encontrada: ", input_full)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(tab_dir)) dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

model <- read.csv(input_model, stringsAsFactors = FALSE)
full <- read.csv(input_full, stringsAsFactors = FALSE)

model$year <- as.integer(model$year)
full$year <- as.integer(full$year)
model$imports_usd_billion <- model$imports_current_usd / 1e9
full$imports_usd_billion <- full$imports_current_usd / 1e9

summarize_numeric <- function(x) {
  data.frame(
    n = sum(!is.na(x)),
    media = mean(x, na.rm = TRUE),
    desvio_padrao = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    p01 = as.numeric(quantile(x, 0.01, na.rm = TRUE)),
    p05 = as.numeric(quantile(x, 0.05, na.rm = TRUE)),
    p25 = as.numeric(quantile(x, 0.25, na.rm = TRUE)),
    mediana = median(x, na.rm = TRUE),
    p75 = as.numeric(quantile(x, 0.75, na.rm = TRUE)),
    p95 = as.numeric(quantile(x, 0.95, na.rm = TRUE)),
    p99 = as.numeric(quantile(x, 0.99, na.rm = TRUE)),
    max = max(x, na.rm = TRUE)
  )
}

n_countries_total <- length(unique(full$country))
country_once <- model[!duplicated(model$country), ]

overview <- data.frame(
  indicador = c(
    "Observações válidas na base modelável",
    "Países com ao menos uma observação",
    "Anos cobertos",
    "Ano inicial",
    "Ano final",
    "Continentes",
    "Sub-regiões",
    "Grupos de renda",
    "Status de desenvolvimento",
    "Países com série completa",
    "Países com lacunas internas",
    "Observações no painel amplo",
    "Ausências no painel amplo",
    "% ausente no painel amplo"
  ),
  valor = c(
    nrow(model),
    length(unique(model$country)),
    length(unique(model$year)),
    min(model$year),
    max(model$year),
    length(unique(model$continent)),
    length(unique(model$subregion)),
    length(unique(model$income_group)),
    length(unique(model$development_status)),
    sum(country_once$country_complete_series_flag),
    sum(country_once$country_has_internal_gap),
    nrow(full),
    sum(is.na(full$imports_current_usd)),
    round(100 * mean(is.na(full$imports_current_usd)), 2)
  )
)

dist <- rbind(
  cbind(variavel = "imports_current_usd", summarize_numeric(model$imports_current_usd)),
  cbind(variavel = "imports_usd_billion", summarize_numeric(model$imports_usd_billion)),
  cbind(variavel = "log_import", summarize_numeric(model$log_import))
)

annual_base <- aggregate(
  cbind(imports_usd_billion, log_import) ~ year,
  data = model,
  FUN = function(x) c(total = sum(x), media = mean(x), mediana = median(x))
)

annual <- data.frame(
  year = annual_base$year,
  total_import_usd_billion = annual_base$imports_usd_billion[, "total"],
  mean_import_usd_billion = annual_base$imports_usd_billion[, "media"],
  median_import_usd_billion = annual_base$imports_usd_billion[, "mediana"],
  mean_log_import = annual_base$log_import[, "media"],
  median_log_import = annual_base$log_import[, "mediana"]
)

n_obs_year <- aggregate(country ~ year, data = model, FUN = function(x) length(unique(x)))
names(n_obs_year)[2] <- "n_obs"
annual <- merge(n_obs_year, annual, by = "year", all.x = TRUE)
annual$coverage_pct_total_countries <- 100 * annual$n_obs / n_countries_total
missing_year <- aggregate(imports_current_usd ~ year, data = full, FUN = function(x) mean(is.na(x)) * 100, na.action = NULL)
names(missing_year)[2] <- "missing_pct_panel_amplo"
annual <- merge(annual, missing_year, by = "year", all.x = TRUE)

group_summary <- function(data, group_col) {
  groups <- unique(data[[group_col]])
  out <- lapply(groups, function(g) {
    x <- data[data[[group_col]] == g, ]
    data.frame(
      grupo = g,
      n_obs = nrow(x),
      n_countries = length(unique(x$country)),
      mean_import_usd_billion = mean(x$imports_usd_billion, na.rm = TRUE),
      median_import_usd_billion = median(x$imports_usd_billion, na.rm = TRUE),
      p75_import_usd_billion = as.numeric(quantile(x$imports_usd_billion, 0.75, na.rm = TRUE)),
      mean_log_import = mean(x$log_import, na.rm = TRUE),
      median_log_import = median(x$log_import, na.rm = TRUE),
      sd_log_import = sd(x$log_import, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, out)
  out[order(-out$median_log_import), ]
}

income_summary <- group_summary(model, "income_group")
names(income_summary)[1] <- "income_group"
status_summary <- group_summary(model, "development_status")
names(status_summary)[1] <- "development_status"
continent_summary <- group_summary(model, "continent")
names(continent_summary)[1] <- "continent"

decade_base <- aggregate(
  cbind(imports_usd_billion, log_import) ~ decade,
  data = model,
  FUN = function(x) c(total = sum(x), media = mean(x), mediana = median(x))
)
decade <- data.frame(
  decade = decade_base$decade,
  total_import_usd_billion = decade_base$imports_usd_billion[, "total"],
  mean_import_usd_billion = decade_base$imports_usd_billion[, "media"],
  median_import_usd_billion = decade_base$imports_usd_billion[, "mediana"],
  mean_log_import = decade_base$log_import[, "media"],
  median_log_import = decade_base$log_import[, "mediana"]
)
n_obs_decade <- aggregate(country ~ decade, data = model, FUN = length)
names(n_obs_decade)[2] <- "n_obs"
n_countries_decade <- aggregate(country ~ decade, data = model, FUN = function(x) length(unique(x)))
names(n_countries_decade)[2] <- "n_countries"
decade <- merge(merge(n_obs_decade, n_countries_decade, by = "decade"), decade, by = "decade")

window_summary <- function(label, idx) {
  x <- model[idx, ]
  data.frame(
    janela = label,
    n_obs = nrow(x),
    n_paises = length(unique(x$country)),
    anos = length(unique(x$year)),
    mediana_import_usd_billion = median(x$imports_usd_billion, na.rm = TRUE),
    media_log_import = mean(x$log_import, na.rm = TRUE),
    mediana_log_import = median(x$log_import, na.rm = TRUE),
    pais_ano_por_pais_medio = nrow(x) / length(unique(x$country))
  )
}

windows <- rbind(
  window_summary("1974-2024", rep(TRUE, nrow(model))),
  window_summary("1994-2023", model$year >= 1994 & model$year <= 2023),
  window_summary("2000-2023", model$year >= 2000 & model$year <= 2023)
)

top_values <- model[order(-model$imports_current_usd), c("country", "year", "continent", "income_group", "development_status", "imports_usd_billion", "log_import")]
top_values <- top_values[seq_len(min(20, nrow(top_values))), ]

country_groups <- split(model, model$country)
country_summary <- do.call(rbind, lapply(country_groups, function(x) {
  data.frame(
    country = x$country[1],
    continent = x$continent[1],
    income_group = x$income_group[1],
    development_status = x$development_status[1],
    n_years = length(unique(x$year)),
    first_year = min(x$year),
    last_year = max(x$year),
    total_import_usd_billion = sum(x$imports_usd_billion, na.rm = TRUE),
    mean_import_usd_billion = mean(x$imports_usd_billion, na.rm = TRUE),
    median_import_usd_billion = median(x$imports_usd_billion, na.rm = TRUE),
    mean_log_import = mean(x$log_import, na.rm = TRUE),
    median_log_import = median(x$log_import, na.rm = TRUE),
    sd_log_import = sd(x$log_import, na.rm = TRUE)
  )
}))
country_summary <- country_summary[order(-country_summary$total_import_usd_billion), ]

slope_rows <- lapply(country_groups, function(x) {
  if (length(unique(x$year)) < 10) return(NULL)
  fit <- lm(log_import ~ year, data = x)
  slope <- coef(fit)[["year"]]
  data.frame(
    country = x$country[1],
    continent = x$continent[1],
    income_group = x$income_group[1],
    development_status = x$development_status[1],
    n_years = length(unique(x$year)),
    first_year = min(x$year),
    last_year = max(x$year),
    slope_log_per_year = slope,
    approx_growth_pct_per_year = (exp(slope) - 1) * 100,
    r_squared = summary(fit)$r.squared
  )
})
slopes <- do.call(rbind, slope_rows)
slopes <- slopes[order(-slopes$slope_log_per_year), ]

num_cols <- c("year", "year_index", "imports_usd_billion", "log_import", "country_n_years_observed", "country_pct_years_observed", "country_internal_gap_years")
corr <- cor(model[, num_cols], use = "pairwise.complete.obs")

write.csv(overview, file.path(tab_dir, "tabela_01_visao_geral.csv"), row.names = FALSE)
write.csv(dist, file.path(tab_dir, "tabela_02_distribuicao_variaveis.csv"), row.names = FALSE)
write.csv(annual, file.path(tab_dir, "tabela_03_resumo_anual.csv"), row.names = FALSE)
write.csv(decade, file.path(tab_dir, "tabela_04_resumo_decadas.csv"), row.names = FALSE)
write.csv(income_summary, file.path(tab_dir, "tabela_05_resumo_grupo_renda.csv"), row.names = FALSE)
write.csv(status_summary, file.path(tab_dir, "tabela_06_resumo_status_desenvolvimento.csv"), row.names = FALSE)
write.csv(continent_summary, file.path(tab_dir, "tabela_07_resumo_continente.csv"), row.names = FALSE)
write.csv(windows, file.path(tab_dir, "tabela_08_janelas_temporais.csv"), row.names = FALSE)
write.csv(top_values, file.path(tab_dir, "tabela_09_top_pais_ano_importacoes.csv"), row.names = FALSE)
write.csv(head(country_summary, 15), file.path(tab_dir, "tabela_10_top_paises_total_importacoes.csv"), row.names = FALSE)
write.csv(slopes, file.path(tab_dir, "tabela_11_tendencias_pais.csv"), row.names = FALSE)
write.csv(head(slopes, 15), file.path(tab_dir, "tabela_12_top_crescimento_exploratorio.csv"), row.names = FALSE)
write.csv(tail(slopes, 15), file.path(tab_dir, "tabela_13_menor_crescimento_exploratorio.csv"), row.names = FALSE)
write.csv(data.frame(variavel = rownames(corr), corr, row.names = NULL), file.path(tab_dir, "tabela_14_correlacoes_variaveis_numericas.csv"), row.names = FALSE)

png(file.path(fig_dir, "grafico_01_cobertura_anual.png"), width = 1600, height = 900, res = 180)
plot(annual$year, annual$coverage_pct_total_countries, type = "l",
     xlab = "Ano", ylab = "% de países com importação observada",
     main = "Cobertura anual da variável de importação no painel amplo")
grid()
dev.off()

png(file.path(fig_dir, "grafico_02_distribuicao_log_import.png"), width = 1600, height = 900, res = 180)
hist(model$log_import, breaks = 40,
     xlab = "log(importações em USD corrente)", ylab = "Frequência",
     main = "Distribuição da variável resposta em escala logarítmica")
grid()
dev.off()

png(file.path(fig_dir, "grafico_03_tendencia_total_mediana.png"), width = 1600, height = 900, res = 180)
plot(annual$year, annual$total_import_usd_billion, type = "l",
     xlab = "Ano", ylab = "Total anual observado (US$ bilhões)",
     main = "Evolução agregada e mediana das importações observadas")
par(new = TRUE)
plot(annual$year, annual$median_import_usd_billion, type = "l", lty = 2, axes = FALSE, xlab = "", ylab = "")
axis(side = 4)
mtext("Mediana por país (US$ bilhões)", side = 4, line = 3)
legend("topleft", legend = c("Total", "Mediana país-ano"), lty = c(1, 2), bty = "n")
grid()
dev.off()

png(file.path(fig_dir, "grafico_04_mediana_log_por_renda.png"), width = 1600, height = 900, res = 180)
income_year <- aggregate(log_import ~ year + income_group, data = model, FUN = median)
plot(range(income_year$year), range(income_year$log_import), type = "n",
     xlab = "Ano", ylab = "Mediana de log(importações)",
     main = "Trajetória mediana por grupo de renda")
for (grp in unique(income_year$income_group)) {
  g <- income_year[income_year$income_group == grp, ]
  lines(g$year, g$log_import)
}
legend("topleft", legend = unique(income_year$income_group), lty = 1, bty = "n", cex = 0.8)
grid()
dev.off()

png(file.path(fig_dir, "grafico_05_boxplot_log_por_renda.png"), width = 1600, height = 900, res = 180)
boxplot(log_import ~ income_group, data = model, horizontal = TRUE, outline = FALSE,
        xlab = "log(importações em USD corrente)",
        main = "Distribuição de log(importações) por grupo de renda")
grid()
dev.off()

png(file.path(fig_dir, "grafico_06_boxplot_log_por_continente.png"), width = 1600, height = 900, res = 180)
boxplot(log_import ~ continent, data = model, horizontal = TRUE, outline = FALSE,
        xlab = "log(importações em USD corrente)",
        main = "Distribuição de log(importações) por continente")
grid()
dev.off()

png(file.path(fig_dir, "grafico_07_distribuicao_tendencias_pais.png"), width = 1600, height = 900, res = 180)
hist(slopes$approx_growth_pct_per_year, breaks = 35,
     xlab = "Crescimento anual aproximado da tendência log-linear (%)",
     ylab = "Número de países",
     main = "Distribuição exploratória das tendências por país")
grid()
dev.off()

coverage_country <- country_once[, c("country", "country_pct_years_observed")]
country_summary2 <- merge(country_summary, coverage_country, by = "country", all.x = TRUE)
png(file.path(fig_dir, "grafico_08_cobertura_vs_nivel_pais.png"), width = 1600, height = 900, res = 180)
plot(country_summary2$country_pct_years_observed, country_summary2$median_log_import,
     xlab = "% de anos observados por país", ylab = "Mediana de log(importações)",
     main = "Cobertura temporal por país versus nível mediano de importações")
grid()
dev.off()

writeLines(c(
  "Etapa 05 executada.",
  paste0("Data/hora: ", Sys.time()),
  paste0("Base modelável: ", input_model),
  paste0("Base ampla: ", input_full),
  paste0("Observações modeláveis: ", nrow(model)),
  paste0("Países com observação: ", length(unique(model$country))),
  paste0("Tabelas geradas: ", length(list.files(tab_dir, pattern = '\\\\.csv$'))),
  paste0("Gráficos gerados: ", length(list.files(fig_dir, pattern = '\\\\.png$'))),
  "Observação: as tendências por país são exploratórias e não substituem modelagem formal."
), con = file.path(output_dir, "log_execucao_etapa_05.txt"))



# =============================
# E07 - Modelagem STAN
# =============================

# ------------------------------------------------------------------------------
# Etapa 07 - Implementacao, estimacao e processamento principal
# Versao consolidada E10: caminhos relativos e sem geracao padrao de .rds.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(stringr)
  library(posterior)
  library(cmdstanr)
  library(loo)
  library(bayesplot)
  library(ggplot2)
  library(jsonlite)
  library(purrr)
})

set.seed(20260429)

args <- commandArgs(trailingOnly = TRUE)
project_dir <- if (length(args) >= 1) normalizePath(args[1], winslash = "/", mustWork = FALSE) else getwd()

dir_base <- file.path(project_dir, "outputs", "tables", "importacoes_world_bank", "E04_Preparacao_Base_Analitica")
dir_etapa <- file.path(project_dir, "outputs", "tables", "importacoes_world_bank", "E07_Implementacao_Estimacao")
dir_tabelas <- file.path(dir_etapa, "tabelas")
dir_figuras <- file.path(project_dir, "outputs", "figures", "importacoes_world_bank", "E07_Implementacao_Estimacao")
dir_stan <- file.path(project_dir, "scripts", "importacoes_world_bank")
dir_cmdstan_csv <- file.path(project_dir, "outputs", "models", "importacoes_world_bank", "cmdstan_csv")
dir_logs <- file.path(dir_etapa, "logs")
dir_config <- file.path(dir_etapa, "configuracoes")
dir_dados_stan <- file.path(dir_etapa, "dados_stan")

for (d in c(dir_tabelas, dir_figuras, dir_stan, dir_cmdstan_csv, dir_logs, dir_config, dir_dados_stan)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

arquivo_base <- file.path(dir_base, "base_analitica_modelavel.csv")
arquivo_stan <- file.path(dir_stan, "modelo_principal_hierarquico_student_t.stan")

if (!file.exists(arquivo_base)) stop("Base modelavel nao encontrada: ", arquivo_base)
if (!file.exists(arquivo_stan)) stop("Arquivo STAN nao encontrado: ", arquivo_stan)

cmdstanr::check_cmdstan_toolchain(fix = FALSE, quiet = FALSE)
if (is.null(cmdstanr::cmdstan_path()) || !dir.exists(cmdstanr::cmdstan_path())) {
  stop("CmdStan nao esta configurado. Use cmdstanr::install_cmdstan() ou cmdstanr::set_cmdstan_path().")
}

base_modelavel <- readr::read_csv(arquivo_base, show_col_types = FALSE) %>%
  mutate(
    year10_c = (year - 2000) / 10,
    income_group = factor(income_group),
    continent = factor(continent),
    development_status = factor(development_status),
    country = factor(country),
    subregion = factor(subregion)
  )

formula_fixa <- ~ year10_c * income_group + continent

preparar_dados_janela <- function(dados, ano_min, ano_max) {
  dados_janela <- dados %>%
    filter(year >= ano_min, year <= ano_max) %>%
    arrange(country, year) %>%
    mutate(
      country = droplevels(factor(country)),
      income_group = droplevels(factor(income_group)),
      continent = droplevels(factor(continent))
    )

  stopifnot(nrow(dados_janela) > 0)
  stopifnot(!anyNA(dados_janela$log_import))

  X <- model.matrix(formula_fixa, data = dados_janela)

  stan_data <- list(
    N = nrow(dados_janela),
    P = ncol(X),
    J_country = nlevels(dados_janela$country),
    y = as.numeric(dados_janela$log_import),
    X = X,
    year10_c = as.numeric(dados_janela$year10_c),
    country_id = as.integer(dados_janela$country),
    y_mean = mean(dados_janela$log_import, na.rm = TRUE)
  )

  mapa_x <- tibble(
    indice_beta = seq_len(ncol(X)),
    variavel_stan = paste0("beta[", seq_len(ncol(X)), "]"),
    termo_modelo = colnames(X)
  )

  list(dados = dados_janela, X = X, stan_data = stan_data, mapa_x = mapa_x)
}

salvar_preprocessamento_janela <- function(obj, codigo_janela) {
  readr::write_csv(
    tibble(
      item = c("N", "P", "J_country", "y_mean"),
      valor = c(obj$stan_data$N, obj$stan_data$P, obj$stan_data$J_country, obj$stan_data$y_mean)
    ),
    file.path(dir_tabelas, paste0("preprocessamento_", codigo_janela, ".csv"))
  )
  readr::write_csv(obj$mapa_x, file.path(dir_tabelas, paste0("mapa_colunas_X_", codigo_janela, ".csv")))
  jsonlite::write_json(
    obj$stan_data,
    file.path(dir_dados_stan, paste0("stan_data_", codigo_janela, ".json")),
    digits = NA,
    auto_unbox = TRUE,
    pretty = TRUE
  )
}

resumir_beta <- function(fit, mapa_x, codigo_janela) {
  resumo <- posterior::summarise_draws(fit$draws(variables = "beta")) %>%
    as_tibble() %>%
    rename(variavel_stan = variable) %>%
    left_join(mapa_x, by = "variavel_stan") %>%
    mutate(
      codigo_janela = codigo_janela,
      razao_multiplicativa_media = exp(mean),
      variacao_percentual_media = 100 * (exp(mean) - 1),
      crescimento_anual_percentual_media = if_else(str_detect(termo_modelo, "year10_c"), 100 * (exp(mean / 10) - 1), NA_real_)
    ) %>%
    select(codigo_janela, indice_beta, variavel_stan, termo_modelo, mean, median, sd, q5, q95, rhat, ess_bulk, ess_tail, razao_multiplicativa_media, variacao_percentual_media, crescimento_anual_percentual_media)

  readr::write_csv(resumo, file.path(dir_tabelas, paste0("resumo_beta_", codigo_janela, ".csv")))
  resumo
}

resumir_diagnosticos <- function(fit, codigo_janela) {
  resumo <- fit$summary() %>% as_tibble()
  readr::write_csv(resumo, file.path(dir_tabelas, paste0("resumo_posterior_completo_", codigo_janela, ".csv")))

  diagnostico_cmdstan <- capture.output(fit$cmdstan_diagnose())
  writeLines(diagnostico_cmdstan, file.path(dir_logs, paste0("cmdstan_diagnose_", codigo_janela, ".txt")))

  diag_basico <- resumo %>%
    summarise(
      codigo_janela = codigo_janela,
      parametros = n(),
      max_rhat = max(rhat, na.rm = TRUE),
      min_ess_bulk = min(ess_bulk, na.rm = TRUE),
      min_ess_tail = min(ess_tail, na.rm = TRUE),
      parametros_rhat_maior_1_01 = sum(rhat > 1.01, na.rm = TRUE),
      parametros_ess_bulk_menor_400 = sum(ess_bulk < 400, na.rm = TRUE),
      parametros_ess_tail_menor_400 = sum(ess_tail < 400, na.rm = TRUE)
    )

  readr::write_csv(diag_basico, file.path(dir_tabelas, paste0("diagnostico_basico_", codigo_janela, ".csv")))
  diag_basico
}

resumir_sampler_diagnostics <- function(fit, codigo_janela, max_treedepth_config) {
  sd <- fit$sampler_diagnostics(format = "draws_df") %>% as_tibble()
  readr::write_csv(sd, file.path(dir_tabelas, paste0("sampler_diagnostics_", codigo_janela, ".csv")))

  ebfmi <- sd %>%
    select(.chain, .iteration, energy__) %>%
    arrange(.chain, .iteration) %>%
    group_by(.chain) %>%
    summarise(
      ebfmi = mean(diff(energy__)^2, na.rm = TRUE) / var(energy__, na.rm = TRUE),
      .groups = "drop"
    )
  readr::write_csv(ebfmi, file.path(dir_tabelas, paste0("ebfmi_", codigo_janela, ".csv")))

  resumo <- tibble(
    codigo_janela = codigo_janela,
    divergencias = sum(sd$divergent__, na.rm = TRUE),
    max_treedepth_observado = max(sd$treedepth__, na.rm = TRUE),
    saturacoes_treedepth = sum(sd$treedepth__ >= max_treedepth_config, na.rm = TRUE),
    ebfmi_min = min(ebfmi$ebfmi, na.rm = TRUE),
    ebfmi_max = max(ebfmi$ebfmi, na.rm = TRUE)
  )
  readr::write_csv(resumo, file.path(dir_tabelas, paste0("diagnosticos_mcmc_", codigo_janela, ".csv")))
  resumo
}

calcular_loo <- function(fit, codigo_janela) {
  log_lik <- posterior::as_draws_matrix(fit$draws(variables = "log_lik"))
  loo_obj <- loo::loo(log_lik)

  writeLines(capture.output(print(loo_obj)), file.path(dir_logs, paste0("loo_", codigo_janela, ".txt")))

  pareto <- tibble(observacao = seq_along(loo::pareto_k_values(loo_obj)), pareto_k = loo::pareto_k_values(loo_obj))
  readr::write_csv(pareto, file.path(dir_tabelas, paste0("pareto_k_", codigo_janela, ".csv")))

  tibble(
    codigo_janela = codigo_janela,
    elpd_loo = loo_obj$estimates["elpd_loo", "Estimate"],
    se_elpd_loo = loo_obj$estimates["elpd_loo", "SE"],
    p_loo = loo_obj$estimates["p_loo", "Estimate"],
    looic = loo_obj$estimates["looic", "Estimate"],
    max_pareto_k = max(pareto$pareto_k, na.rm = TRUE),
    n_pareto_k_maior_0_7 = sum(pareto$pareto_k > 0.7, na.rm = TRUE)
  )
}

gerar_ppc <- function(fit, y, codigo_janela) {
  y_rep <- posterior::as_draws_matrix(fit$draws(variables = "y_rep"))
  n_draws <- min(100, nrow(y_rep))
  set.seed(20260429)
  idx <- sample(seq_len(nrow(y_rep)), size = n_draws)

  png(file.path(dir_figuras, paste0("ppc_dens_overlay_", codigo_janela, ".png")), width = 1600, height = 1000, res = 160)
  print(bayesplot::ppc_dens_overlay(y, y_rep[idx, , drop = FALSE]))
  dev.off()

  png(file.path(dir_figuras, paste0("ppc_stat_mediana_", codigo_janela, ".png")), width = 1400, height = 900, res = 160)
  print(bayesplot::ppc_stat(y, y_rep[idx, , drop = FALSE], stat = "median"))
  dev.off()

  invisible(TRUE)
}

ajustar_modelo_stan <- function(obj, codigo_janela, iter_warmup, iter_sampling, chains, parallel_chains, adapt_delta, max_treedepth) {
  salvar_preprocessamento_janela(obj, codigo_janela)

  configuracao <- list(
    codigo_janela = codigo_janela,
    seed = 20260429,
    chains = chains,
    parallel_chains = parallel_chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    modelo_stan = arquivo_stan
  )
  jsonlite::write_json(configuracao, file.path(dir_config, paste0("configuracao_amostragem_", codigo_janela, ".json")), pretty = TRUE, auto_unbox = TRUE)

  y_sd <- stats::sd(obj$stan_data$y, na.rm = TRUE)
  init_fun <- function(chain_id = 1) {
    beta_init <- rep(0, obj$stan_data$P)
    beta_init[1] <- obj$stan_data$y_mean
    list(
      beta = beta_init,
      z_country_intercept = rep(0, obj$stan_data$J_country),
      z_country_slope = rep(0, obj$stan_data$J_country),
      tau_country_intercept = max(y_sd / 4, 0.10),
      tau_country_slope = 0.05,
      sigma = max(y_sd, 0.10),
      nu = 10
    )
  }

  mod <- cmdstanr::cmdstan_model(stan_file = arquivo_stan, force_recompile = FALSE)

  fit <- mod$sample(
    data = obj$stan_data,
    seed = 20260429,
    chains = chains,
    parallel_chains = parallel_chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    init = init_fun,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    refresh = 100,
    output_dir = dir_cmdstan_csv
  )

  # Nao salvar objeto fit em .rds. A reproducibilidade e auditoria ficam nos CSVs do CmdStan, tabelas, logs e configuracoes.
  beta <- resumir_beta(fit, obj$mapa_x, codigo_janela)
  diag <- resumir_diagnosticos(fit, codigo_janela)
  mcmc <- resumir_sampler_diagnostics(fit, codigo_janela, max_treedepth)
  loo <- calcular_loo(fit, codigo_janela)
  gerar_ppc(fit, obj$stan_data$y, codigo_janela)

  list(beta = beta, diag = diag, mcmc = mcmc, loo = loo)
}

janelas <- list(
  principal_2000_2023 = preparar_dados_janela(base_modelavel, 2000, 2023),
  sensibilidade_1994_2023 = preparar_dados_janela(base_modelavel, 1994, 2023),
  sensibilidade_1974_2024 = preparar_dados_janela(base_modelavel, 1974, 2024)
)

resumo_janelas <- purrr::imap_dfr(janelas, function(obj, nm) {
  tibble(
    codigo_janela = nm,
    observacoes = nrow(obj$dados),
    paises = n_distinct(obj$dados$country),
    ano_min = min(obj$dados$year),
    ano_max = max(obj$dados$year),
    media_log_import = mean(obj$dados$log_import),
    mediana_log_import = median(obj$dados$log_import),
    media_usd_bi = mean(obj$dados$imports_usd_billion),
    mediana_usd_bi = median(obj$dados$imports_usd_billion)
  )
})
readr::write_csv(resumo_janelas, file.path(dir_tabelas, "resumo_janelas_stan.csv"))

config_principal <- list(iter_warmup = 1000, iter_sampling = 2000, chains = 4, parallel_chains = 4, adapt_delta = 0.99, max_treedepth = 14)
config_sensibilidade <- list(iter_warmup = 750, iter_sampling = 1000, chains = 4, parallel_chains = 4, adapt_delta = 0.99, max_treedepth = 14)

resultado_principal <- ajustar_modelo_stan(janelas$principal_2000_2023, "principal_2000_2023", config_principal$iter_warmup, config_principal$iter_sampling, config_principal$chains, config_principal$parallel_chains, config_principal$adapt_delta, config_principal$max_treedepth)
resultado_sens_1994_2023 <- ajustar_modelo_stan(janelas$sensibilidade_1994_2023, "sensibilidade_1994_2023", config_sensibilidade$iter_warmup, config_sensibilidade$iter_sampling, config_sensibilidade$chains, config_sensibilidade$parallel_chains, config_sensibilidade$adapt_delta, config_sensibilidade$max_treedepth)
resultado_sens_1974_2024 <- ajustar_modelo_stan(janelas$sensibilidade_1974_2024, "sensibilidade_1974_2024", config_sensibilidade$iter_warmup, config_sensibilidade$iter_sampling, config_sensibilidade$chains, config_sensibilidade$parallel_chains, config_sensibilidade$adapt_delta, config_sensibilidade$max_treedepth)

comparativo_loo <- bind_rows(resultado_principal$loo, resultado_sens_1994_2023$loo, resultado_sens_1974_2024$loo)
readr::write_csv(comparativo_loo, file.path(dir_tabelas, "comparativo_loo_janelas.csv"))

comparativo_diagnostico <- bind_rows(resultado_principal$diag, resultado_sens_1994_2023$diag, resultado_sens_1974_2024$diag)
readr::write_csv(comparativo_diagnostico, file.path(dir_tabelas, "comparativo_diagnostico_janelas.csv"))

comparativo_mcmc <- bind_rows(resultado_principal$mcmc, resultado_sens_1994_2023$mcmc, resultado_sens_1974_2024$mcmc)
readr::write_csv(comparativo_mcmc, file.path(dir_tabelas, "diagnosticos_mcmc_janelas.csv"))

writeLines(
  c(
    "E07 - execucao R + STAN concluida.",
    paste("Data/hora:", Sys.time()),
    paste("CmdStan:", cmdstanr::cmdstan_version()),
    paste("Modelo STAN:", arquivo_stan),
    paste("Base:", arquivo_base),
    "Arquivos principais: CSVs do CmdStan, resumo_beta_*.csv, resumo_posterior_completo_*.csv, sampler_diagnostics_*.csv, diagnosticos_mcmc_*.csv, comparativo_loo_janelas.csv, comparativo_diagnostico_janelas.csv.",
    "Nao foram gerados objetos .rds por padrao."
  ),
  file.path(dir_etapa, "log_execucao_stan_etapa_07.txt")
)



# =============================
# E08 - Diagnostico e sensibilidade
# =============================
# E08 — Diagnóstico, validação e análise de sensibilidade
# Projeto: análise estatística das importações anuais por país
# Entrada: saídas tabulares da E07.
# Saída: tabelas e gráficos da E08 em E08_Diagnostico_Validacao/.
#
# Como usar:
# 1) Execute este script a partir da raiz do projeto.
# 2) As saidas da E07 sao geradas automaticamente em outputs/tables/importacoes_world_bank/E07_Implementacao_Estimacao/.
# 3) O script não reestima o modelo STAN; ele diagnostica os resultados já produzidos na E07.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(ggplot2)
})

dir_e07 <- file.path(project_dir, "outputs", "tables", "importacoes_world_bank", "E07_Implementacao_Estimacao")
dir_e08 <- file.path(project_dir, "outputs", "tables", "importacoes_world_bank", "E08_Diagnostico_Validacao")
dir_tabelas_e07 <- file.path(dir_e07, "tabelas")
dir_tabelas_e08 <- file.path(dir_e08, "tabelas")
dir_figuras_e08 <- file.path(project_dir, "outputs", "figures", "importacoes_world_bank", "E08_Diagnostico_Validacao")

if (!dir.exists(dir_tabelas_e07)) {
  stop("Pasta de tabelas da E07 nao encontrada: ", dir_tabelas_e07)
}

dir.create(dir_tabelas_e08, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_figuras_e08, recursive = TRUE, showWarnings = FALSE)

ordem_janelas <- c("principal_2000_2023", "sensibilidade_1994_2023", "sensibilidade_1974_2024")
rotulo_janela <- c(
  principal_2000_2023 = "Principal 2000-2023",
  sensibilidade_1994_2023 = "Sens. 1994-2023",
  sensibilidade_1974_2024 = "Sens. 1974-2024"
)

diagnostico <- read_csv(file.path(dir_tabelas_e07, "comparativo_diagnostico_janelas.csv"), show_col_types = FALSE)
loo <- read_csv(file.path(dir_tabelas_e07, "comparativo_loo_janelas.csv"), show_col_types = FALSE)
janelas <- read_csv(file.path(dir_tabelas_e07, "resumo_janelas_stan.csv"), show_col_types = FALSE)

tabela_01 <- diagnostico %>%
  left_join(loo, by = "codigo_janela") %>%
  left_join(select(janelas, codigo_janela, observacoes, paises, ano_min, ano_max), by = "codigo_janela") %>%
  mutate(
    perc_rhat_maior_1_01 = 100 * parametros_rhat_maior_1_01 / parametros,
    perc_ess_bulk_menor_400 = 100 * parametros_ess_bulk_menor_400 / parametros,
    perc_ess_tail_menor_400 = 100 * parametros_ess_tail_menor_400 / parametros,
    classificacao_diagnostica = case_when(
      max_rhat <= 1.01 & n_pareto_k_maior_0_7 == 0 &
        parametros_ess_bulk_menor_400 == 0 & parametros_ess_tail_menor_400 == 0 ~ "forte",
      max_rhat <= 1.015 & n_pareto_k_maior_0_7 <= 1 &
        parametros_ess_bulk_menor_400 == 0 & parametros_ess_tail_menor_400 == 0 ~ "aceitável com alerta",
      max_rhat <= 1.05 ~ "sensibilidade com restrição",
      TRUE ~ "frágil"
    ),
    rotulo_janela = unname(rotulo_janela[codigo_janela])
  )

write_csv(tabela_01, file.path(dir_tabelas_e08, "tabela_01_diagnostico_computacional.csv"))

pareto_resumo <- map_dfr(ordem_janelas, function(cod) {
  p <- read_csv(file.path(dir_tabelas_e07, paste0("pareto_k_", cod, ".csv")), show_col_types = FALSE)
  tibble(
    codigo_janela = cod,
    rotulo_janela = unname(rotulo_janela[cod]),
    n_observacoes_loo = nrow(p),
    min_pareto_k = min(p$pareto_k, na.rm = TRUE),
    p50_pareto_k = median(p$pareto_k, na.rm = TRUE),
    p90_pareto_k = quantile(p$pareto_k, .90, na.rm = TRUE),
    p95_pareto_k = quantile(p$pareto_k, .95, na.rm = TRUE),
    p99_pareto_k = quantile(p$pareto_k, .99, na.rm = TRUE),
    max_pareto_k = max(p$pareto_k, na.rm = TRUE),
    n_pareto_k_maior_0_5 = sum(p$pareto_k > 0.5, na.rm = TRUE),
    n_pareto_k_maior_0_7 = sum(p$pareto_k > 0.7, na.rm = TRUE),
    n_pareto_k_maior_1_0 = sum(p$pareto_k > 1.0, na.rm = TRUE)
  )
})

write_csv(pareto_resumo, file.path(dir_tabelas_e08, "tabela_02_pareto_k_resumo.csv"))

arquivos_beta <- list.files(dir_tabelas_e07, pattern = "^resumo_beta_.*\\.csv$", full.names = TRUE)
betas <- map_dfr(arquivos_beta, ~ read_csv(.x, show_col_types = FALSE)) %>%
  mutate(
    sinal_intervalo_90 = case_when(
      q5 > 0 & q95 > 0 ~ "positivo",
      q5 < 0 & q95 < 0 ~ "negativo",
      TRUE ~ "incerto"
    ),
    rotulo_janela = unname(rotulo_janela[codigo_janela])
  )

write_csv(betas, file.path(dir_tabelas_e08, "tabela_03_sensibilidade_coeficientes.csv"))

classificacao <- betas %>%
  group_by(termo_modelo) %>%
  summarise(
    sinais = paste(unique(sinal_intervalo_90), collapse = "; "),
    max_rhat_termo = max(rhat, na.rm = TRUE),
    min_ess_bulk_termo = min(ess_bulk, na.rm = TRUE),
    min_ess_tail_termo = min(ess_tail, na.rm = TRUE),
    classificacao_robustez = case_when(
      n_distinct(sinal_intervalo_90) == 1 & !any(sinal_intervalo_90 == "incerto") ~ "robusto substantivamente",
      n_distinct(sign(mean)) == 1 & any(sinal_intervalo_90 == "incerto") ~ "condicional",
      TRUE ~ "frágil"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    classificacao_robustez = if_else(
      max_rhat_termo > 1.03 | min_ess_bulk_termo < 400 | min_ess_tail_termo < 400,
      paste(classificacao_robustez, "com alerta computacional"),
      classificacao_robustez
    )
  )

write_csv(classificacao, file.path(dir_tabelas_e08, "tabela_04_classificacao_robustez.csv"))

grafico_01 <- tabela_01 %>%
  select(rotulo_janela, perc_rhat_maior_1_01, perc_ess_bulk_menor_400, perc_ess_tail_menor_400) %>%
  pivot_longer(-rotulo_janela, names_to = "metrica", values_to = "valor") %>%
  ggplot(aes(x = rotulo_janela, y = valor, fill = metrica)) +
  geom_col(position = "dodge") +
  labs(title = "Diagnóstico computacional por janela", x = NULL, y = "Percentual de parâmetros") +
  theme_minimal()

ggsave(file.path(dir_figuras_e08, "grafico_01_diagnostico_computacional.png"), grafico_01, width = 8, height = 4.8, dpi = 180)

grafico_02 <- pareto_resumo %>%
  ggplot(aes(x = rotulo_janela, y = n_pareto_k_maior_0_7)) +
  geom_col() +
  labs(title = "Observações com Pareto-k > 0,7", x = NULL, y = "Número de observações") +
  theme_minimal()

ggsave(file.path(dir_figuras_e08, "grafico_02_pareto_k_maior_07.png"), grafico_02, width = 8, height = 4.8, dpi = 180)

writeLines(
  c(
    "E08 — diagnostico, validacao e sensibilidade concluida.",
    paste("Data/hora:", Sys.time()),
    paste("Pasta E07 usada:", dir_e07),
    "Arquivos principais: tabela_01_diagnostico_computacional.csv; tabela_02_pareto_k_resumo.csv; tabela_03_sensibilidade_coeficientes.csv; tabela_04_classificacao_robustez.csv; graficos de diagnostico."
  ),
  file.path(dir_e08, "log_execucao_etapa_08.txt")
)


# Fim do script final reprodutivel.
