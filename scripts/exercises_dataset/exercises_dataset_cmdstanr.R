#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)
options(pillar.sigfig = 6)

############################################################
# Exercises Dataset — cobertura, dificuldade e modelagem ordinal
#
# Padrão do repo:
# - rodar a partir da raiz do repositório
# - sem setwd()
# - dados versionados em data/raw/exercises_dataset/
# - modelos Stan ao lado deste script
# - saídas regeneráveis em outputs/exercises_dataset/
# - sem salvar objetos .rds
#
# Execução padrão, sem reamostrar Stan:
#   Rscript scripts/exercises_dataset/exercises_dataset_cmdstanr.R
#
# Reexecução da amostragem Stan/cmdstanr:
#   Rscript scripts/exercises_dataset/exercises_dataset_cmdstanr.R --run_stan=1
#   RUN_STAN=true Rscript scripts/exercises_dataset/exercises_dataset_cmdstanr.R
############################################################

args <- commandArgs(trailingOnly = TRUE)

parse_args <- function(args) {
  out <- list(
    root = NA_character_,
    input_csv = file.path("data", "raw", "exercises_dataset", "final_exercise_dataset.csv"),
    run_stan = NA_character_,
    seed = 20260518L,
    chains = 4L,
    parallel_chains = 4L,
    iter_warmup = 1000L,
    iter_sampling = 1000L,
    adapt_delta = 0.95,
    max_treedepth = 12L,
    refresh = 100L,
    output_dir = file.path("outputs", "exercises_dataset")
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

repo_root_candidates <- unique(c(
  opts$root,
  getwd(),
  dirname(script_path())
))
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
study_id <- "exercises_dataset"

input_csv <- if (file.exists(opts$input_csv)) opts$input_csv else file.path(repo_root, opts$input_csv)
input_csv <- normalizePath(input_csv, mustWork = FALSE)

script_dir <- file.path(repo_root, "scripts", study_id)
out_dir <- file.path(repo_root, opts$output_dir)
out_tables <- file.path(out_dir, "tables")
out_figures <- file.path(out_dir, "figures")
out_logs <- file.path(out_dir, "logs")
out_cmdstan <- file.path(out_dir, "cmdstan_csv")
out_data_stan <- file.path(out_dir, "data_stan")
for (d in c(out_tables, out_figures, out_logs, out_cmdstan, out_data_stan)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

log_file <- file.path(out_logs, "exercises_dataset_execucao.log")
log_msg <- function(...) {
  txt <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste0(..., collapse = ""))
  message(txt)
  cat(txt, "\n", file = log_file, append = TRUE)
}

require_pkg <- function(pkg, optional = FALSE) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- paste0(
      "Pacote requerido não instalado: ", pkg,
      "\nRode: Rscript scripts/_setup/install_deps.R --all"
    )
    if (optional) return(FALSE)
    stop(msg, call. = FALSE)
  }
  TRUE
}

for (pkg in c("readr", "dplyr", "tidyr", "stringr", "purrr", "tibble", "digest", "jsonlite")) {
  require_pkg(pkg)
}
if (run_stan) {
  require_pkg("cmdstanr")
  require_pkg("posterior")
}

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(jsonlite)
})

fail <- function(...) stop(paste0(...), call. = FALSE)

safe_parse_list <- function(x) {
  if (is.na(x) || !nzchar(x)) return(character(0))
  y <- x
  y <- gsub("\\\\", "\\\\\\\\", y)
  y <- gsub('"', '\\"', y)
  y <- gsub("'", '"', y)
  out <- tryCatch(jsonlite::fromJSON(y), error = function(e) character(0))
  stringr::str_squish(as.character(out))
}

is_effectively_missing <- function(x) {
  y <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  is.na(x) | !nzchar(y) | y %in% c("na", "nan", "null", "none")
}

std_score <- function(x) {
  x <- as.numeric(x)
  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  as.numeric((x - mean(x, na.rm = TRUE)) / s)
}

freq_tbl <- function(data, var) {
  data |>
    count({{ var }}, name = "n") |>
    mutate(prop = n / sum(n)) |>
    arrange(desc(n))
}

write_csv2 <- function(x, path) {
  readr::write_csv(x, path, na = "")
  invisible(path)
}

read_required_stan_csv <- function(path, chain_id) {
  x <- readr::read_csv(path, comment = "#", show_col_types = FALSE)
  if (nrow(x) == 0) return(NULL)
  x$chain <- chain_id
  x$draw <- seq_len(nrow(x))
  x
}

ebfmi <- function(energy) {
  if (length(energy) < 2 || stats::var(energy) == 0) return(NA_real_)
  mean(diff(energy)^2) / stats::var(energy)
}

log_msg("Iniciando estudo exercises_dataset.")
if (!file.exists(input_csv)) fail("Arquivo de entrada não encontrado: ", input_csv)

# ------------------------------------------------------------------------------
# 1. Leitura e preparação da base analítica
# ------------------------------------------------------------------------------
raw <- readr::read_csv(input_csv, show_col_types = FALSE) |>
  mutate(.linha_arquivo = row_number() + 1L)

essential <- c(
  "bodyPart", "equipment", "id", "name", "target", "secondaryMuscles",
  "instructions", "description", "difficulty", "category"
)
missing_essential_cols <- setdiff(essential, names(raw))
if (length(missing_essential_cols) > 0) {
  fail("Colunas obrigatórias ausentes no CSV: ", paste(missing_essential_cols, collapse = ", "))
}

mask_message <- if ("message" %in% names(raw)) !is_effectively_missing(raw$message) else rep(FALSE, nrow(raw))
mask_essential_missing <- raw |>
  select(all_of(essential)) |>
  mutate(across(everything(), is_effectively_missing)) |>
  as.data.frame() |>
  apply(1, any)
mask_quarantine <- mask_message | mask_essential_missing

quarantine <- raw[mask_quarantine, , drop = FALSE] |>
  mutate(motivo_quarentena = case_when(
    mask_message[mask_quarantine] ~ "message preenchida / linha técnica de API",
    mask_essential_missing[mask_quarantine] ~ "campo essencial ausente",
    TRUE ~ "critério de quarentena"
  ))

valid <- raw[!mask_quarantine, , drop = FALSE] |>
  mutate(
    id_original = as.character(id),
    id = as.integer(id),
    exercise_id = sprintf("EX%04d", id)
  ) |>
  mutate(across(c(bodyPart, equipment, name, target, description, difficulty, category), str_squish)) |>
  mutate(
    bodyPart_std = str_to_lower(bodyPart),
    equipment_std = str_to_lower(equipment),
    name_std = str_to_lower(name),
    target_std = str_to_lower(target),
    difficulty_std = str_to_lower(difficulty),
    category_std = str_to_lower(category),
    secondary_muscles_list = map(secondaryMuscles, safe_parse_list),
    instructions_list = map(instructions, safe_parse_list),
    n_secondary_muscles = map_int(secondary_muscles_list, length),
    n_instruction_steps = map_int(instructions_list, length),
    instructions_hash = vapply(instructions, digest::digest, character(1), algo = "sha1", serialize = FALSE),
    instructions_hash = substr(instructions_hash, 1, 12),
    name_key = name_std
  ) |>
  group_by(name_key) |>
  mutate(n_registros_mesmo_nome = n(), flag_nome_duplicado = n() > 1) |>
  ungroup() |>
  group_by(instructions_hash) |>
  mutate(n_registros_mesma_instrucao = n(), flag_instrucoes_duplicadas = n() > 1) |>
  ungroup()

valid_export <- valid |>
  mutate(
    secondary_muscles_list = map_chr(secondary_muscles_list, ~ paste(.x, collapse = "|")),
    instructions_list = map_chr(instructions_list, ~ paste(.x, collapse = "|"))
  )
write_csv2(valid_export, file.path(out_tables, "base_analitica.csv"))
write_csv2(quarantine, file.path(out_tables, "linhas_quarentenadas.csv"))

# ------------------------------------------------------------------------------
# 2. Exploração descritiva principal
# ------------------------------------------------------------------------------
write_csv2(freq_tbl(valid, category_std), file.path(out_tables, "freq_category.csv"))
write_csv2(freq_tbl(valid, difficulty_std), file.path(out_tables, "freq_difficulty.csv"))
write_csv2(freq_tbl(valid, bodyPart_std), file.path(out_tables, "freq_bodyPart.csv"))
write_csv2(freq_tbl(valid, equipment_std), file.path(out_tables, "freq_equipment.csv"))
write_csv2(freq_tbl(valid, target_std), file.path(out_tables, "freq_target.csv"))

resumo_geral <- tibble(
  indicador = c(
    "linhas_base_analitica", "linhas_quarentenadas", "colunas_base_analitica", "ids_unicos", "nomes_unicos",
    "body_parts", "equipamentos", "targets", "categorias", "dificuldades",
    "media_musculos_secundarios", "mediana_musculos_secundarios", "max_musculos_secundarios",
    "media_passos_instrucao", "mediana_passos_instrucao", "max_passos_instrucao",
    "exercicios_nome_duplicado", "exercicios_instrucao_duplicada"
  ),
  valor = c(
    nrow(valid), nrow(quarantine), ncol(valid_export), n_distinct(valid$id), n_distinct(valid$name_std),
    n_distinct(valid$bodyPart_std), n_distinct(valid$equipment_std), n_distinct(valid$target_std),
    n_distinct(valid$category_std), n_distinct(valid$difficulty_std),
    mean(valid$n_secondary_muscles), median(valid$n_secondary_muscles), max(valid$n_secondary_muscles),
    mean(valid$n_instruction_steps), median(valid$n_instruction_steps), max(valid$n_instruction_steps),
    sum(valid$flag_nome_duplicado), sum(valid$flag_instrucoes_duplicadas)
  )
)
write_csv2(resumo_geral, file.path(out_tables, "resumo_geral.csv"))

# ------------------------------------------------------------------------------
# 3. Base de modelagem e dados Stan
# ------------------------------------------------------------------------------
difficulty_levels <- c("beginner", "intermediate", "advanced")
model_df <- valid |>
  mutate(
    difficulty_id = match(difficulty_std, difficulty_levels),
    body_fct = factor(bodyPart_std),
    body_id = as.integer(body_fct),
    category_group = if_else(category_std == "strength", "strength", "other_categories"),
    category_fct = factor(category_group),
    category_id = as.integer(category_fct),
    equipment_group = case_when(
      equipment_std %in% c("body weight", "dumbbell", "barbell", "cable", "leverage machine", "kettlebell", "smith machine", "weighted") ~ equipment_std,
      TRUE ~ "other_equipment"
    ),
    equipment_fct = factor(equipment_group),
    equipment_id = as.integer(equipment_fct),
    z_instruction_steps = std_score(n_instruction_steps),
    z_secondary_muscles = std_score(n_secondary_muscles)
  ) |>
  filter(!is.na(difficulty_id))

base_modelagem_export <- model_df |>
  select(
    exercise_id, id, name, bodyPart_std, equipment_std, target_std, category_std,
    difficulty_std, difficulty_id, body_id, category_group, category_id,
    equipment_group, equipment_id, n_instruction_steps, n_secondary_muscles,
    z_instruction_steps, z_secondary_muscles
  )
write_csv2(base_modelagem_export, file.path(out_tables, "base_modelagem.csv"))

codificacao <- bind_rows(
  model_df |> distinct(tipo = "body", codigo = body_id, valor = bodyPart_std),
  model_df |> distinct(tipo = "category_group", codigo = category_id, valor = category_group),
  model_df |> distinct(tipo = "equipment_group", codigo = equipment_id, valor = equipment_group)
) |>
  arrange(tipo, codigo)
write_csv2(codificacao, file.path(out_tables, "codificacao_variaveis.csv"))

stan_data_m2 <- list(
  N = nrow(model_df),
  K_y = length(difficulty_levels),
  y = model_df$difficulty_id,
  K_body = n_distinct(model_df$body_id),
  body = model_df$body_id,
  K_cat = n_distinct(model_df$category_id),
  category_group = model_df$category_id,
  z_instruction_steps = model_df$z_instruction_steps,
  z_secondary_muscles = model_df$z_secondary_muscles
)
stan_data_m2b <- c(stan_data_m2, list(
  K_equipment = n_distinct(model_df$equipment_id),
  equipment_group = model_df$equipment_id
))
X_softmax <- stats::model.matrix(
  ~ 0 + bodyPart_std + category_group + z_instruction_steps + z_secondary_muscles,
  data = model_df
)
stan_data_softmax <- list(
  N = nrow(model_df),
  K_y = length(difficulty_levels),
  P = ncol(X_softmax),
  X = unname(X_softmax),
  y = model_df$difficulty_id
)
jsonlite::write_json(stan_data_m2, file.path(out_data_stan, "dados_ordinal_sem_equipamento.json"), auto_unbox = TRUE, pretty = TRUE)
jsonlite::write_json(stan_data_m2b, file.path(out_data_stan, "dados_ordinal_com_equipamento.json"), auto_unbox = TRUE, pretty = TRUE)
jsonlite::write_json(stan_data_softmax, file.path(out_data_stan, "dados_softmax_dificuldade_sensibilidade.json"), auto_unbox = TRUE, pretty = TRUE)

# ------------------------------------------------------------------------------
# 4. Estimação CmdStanR opcional
# ------------------------------------------------------------------------------
cfg <- list(
  seed = opts$seed,
  chains = opts$chains,
  parallel_chains = opts$parallel_chains,
  iter_warmup = opts$iter_warmup,
  iter_sampling = opts$iter_sampling,
  adapt_delta = opts$adapt_delta,
  max_treedepth = opts$max_treedepth,
  refresh = opts$refresh
)
jsonlite::write_json(cfg, file.path(out_logs, "configuracao_amostragem_cmdstanr.json"), auto_unbox = TRUE, pretty = TRUE)

fit_one <- function(nome, stan_file, data_file) {
  if (!file.exists(stan_file)) fail("Arquivo Stan não encontrado: ", stan_file)
  if (!file.exists(data_file)) fail("Arquivo de dados Stan não encontrado: ", data_file)
  log_msg("Amostrando modelo: ", nome)
  mod <- cmdstanr::cmdstan_model(stan_file)
  fit <- mod$sample(
    data = data_file,
    seed = cfg$seed,
    chains = cfg$chains,
    parallel_chains = cfg$parallel_chains,
    iter_warmup = cfg$iter_warmup,
    iter_sampling = cfg$iter_sampling,
    adapt_delta = cfg$adapt_delta,
    max_treedepth = cfg$max_treedepth,
    refresh = cfg$refresh,
    output_dir = out_cmdstan
  )
  readr::write_csv(fit$summary(), file.path(out_tables, paste0("resumo_posterior_", nome, ".csv")))
  writeLines(capture.output(print(fit$diagnostic_summary())), file.path(out_logs, paste0("diagnosticos_", nome, ".txt")))
  invisible(fit)
}

if (run_stan) {
  suppressPackageStartupMessages({
    library(cmdstanr)
    library(posterior)
  })
  fit_one(
    "M2_ordinal_sem_equipamento",
    file.path(script_dir, "modelo_ordinal_dificuldade_sem_equipamento.stan"),
    file.path(out_data_stan, "dados_ordinal_sem_equipamento.json")
  )
  fit_one(
    "M2b_ordinal_com_equipamento_diagnostico",
    file.path(script_dir, "modelo_ordinal_dificuldade_com_equipamento_diagnostico.stan"),
    file.path(out_data_stan, "dados_ordinal_com_equipamento.json")
  )
} else {
  log_msg("Amostragem Stan não executada. Use --run_stan=1 para reestimar os modelos.")
}

# ------------------------------------------------------------------------------
# 5. Auditoria mínima de diagnósticos a partir dos CSVs CmdStan disponíveis
# ------------------------------------------------------------------------------
audit_model <- function(pattern, modelo, max_treedepth = 12L) {
  files <- sort(list.files(out_cmdstan, pattern = pattern, full.names = TRUE))
  if (length(files) == 0) return(tibble())
  draws <- bind_rows(lapply(seq_along(files), function(i) read_required_stan_csv(files[i], i)))
  required_diag <- c("divergent__", "treedepth__", "energy__", "accept_stat__")
  if (!all(required_diag %in% names(draws))) return(tibble())
  draws |>
    group_by(chain) |>
    summarise(
      modelo = modelo,
      n_draws_pos_warmup = n(),
      divergencias = sum(.data$divergent__),
      treedepth_saturado = sum(.data$treedepth__ >= max_treedepth),
      ebfmi = ebfmi(.data$energy__),
      accept_stat_medio = mean(.data$accept_stat__),
      treedepth_max = max(.data$treedepth__),
      .groups = "drop"
    ) |>
    select(modelo, chain, everything())
}

diag <- bind_rows(
  audit_model("modelo_ordinal_dificuldade_sem_equipamento.*csv$", "M2_ordinal_sem_equipamento", cfg$max_treedepth),
  audit_model("modelo_ordinal_dificuldade_com_equipamento_diagnostico.*csv$", "M2b_ordinal_com_equipamento_diagnostico", cfg$max_treedepth)
)
if (nrow(diag) > 0) {
  write_csv2(diag, file.path(out_tables, "diagnosticos_por_cadeia.csv"))
}

# ------------------------------------------------------------------------------
# 6. Entrega final regenerável
# ------------------------------------------------------------------------------
coverage_final <- bind_rows(
  freq_tbl(valid, category_std) |>
    mutate(dimensao = "category", leitura = category_std) |>
    select(dimensao, leitura, n, prop),
  freq_tbl(valid, difficulty_std) |>
    mutate(dimensao = "difficulty", leitura = difficulty_std) |>
    select(dimensao, leitura, n, prop),
  freq_tbl(valid, bodyPart_std) |>
    slice_head(n = 10) |>
    mutate(dimensao = "bodyPart", leitura = bodyPart_std) |>
    select(dimensao, leitura, n, prop),
  freq_tbl(valid, equipment_std) |>
    slice_head(n = 10) |>
    mutate(dimensao = "equipment", leitura = equipment_std) |>
    select(dimensao, leitura, n, prop),
  freq_tbl(valid, target_std) |>
    slice_head(n = 10) |>
    mutate(dimensao = "target", leitura = target_std) |>
    select(dimensao, leitura, n, prop)
) |>
  mutate(prop_pct = round(100 * prop, 2))
write_csv2(coverage_final, file.path(out_tables, "sintese_cobertura_catalogo.csv"))

recomendacoes <- tibble(
  prioridade = c("Alta", "Alta", "Média", "Média"),
  recomendacao = c(
    "Usar a distribuição de difficulty como eixo central de comunicação do catálogo.",
    "Evitar interpretar o efeito de equipment como causal; tratar como diagnóstico de composição do catálogo.",
    "Revisar grupos corporais e equipamentos de baixa frequência antes de usar o catálogo para recomendação automatizada.",
    "Reexecutar Stan com --run_stan=1 quando for necessário atualizar inferências posteriores a partir de nova versão dos dados."
  )
)
write_csv2(recomendacoes, file.path(out_tables, "recomendacoes_acionaveis.csv"))

confiabilidade <- tibble(
  item = c("base_analitica", "dados_stan", "amostragem_cmdstanr", "diagnosticos_cmdstanr"),
  status = c(
    "gerada",
    "gerados",
    if (run_stan) "executada" else "não executada por padrão",
    if (nrow(diag) > 0) "auditados a partir dos CSVs" else "sem CSVs CmdStan disponíveis"
  )
)
write_csv2(confiabilidade, file.path(out_tables, "confiabilidade_reprodutibilidade.csv"))

log_msg("Fluxo concluído. Saídas em: ", out_dir)
