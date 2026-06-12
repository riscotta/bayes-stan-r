#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)
options(pillar.sigfig = 6)

############################################################
# Sherlock Holmes — frequências de termos/personagens
# com modelo Poisson-lognormal hierárquico
#
# Padrão do repo:
# - rodar a partir da raiz do repositório
# - sem setwd()
# - dados versionados em data/raw/sherlock_holmes_terms/
# - modelo Stan ao lado deste script
# - saídas regeneráveis em outputs/
#
# Execução padrão, sem reamostrar Stan:
#   Rscript scripts/sherlock_holmes_terms/sherlock_holmes_terms_cmdstanr.R
#
# Reexecução da amostragem Stan/cmdstanr:
#   Rscript scripts/sherlock_holmes_terms/sherlock_holmes_terms_cmdstanr.R --run_stan=1
#   RUN_STAN=true Rscript scripts/sherlock_holmes_terms/sherlock_holmes_terms_cmdstanr.R
############################################################

args <- commandArgs(trailingOnly = TRUE)

parse_args <- function(args) {
  out <- list(
    root = NA_character_,
    run_stan = NA_character_,
    seed = 20260612L,
    chains = 4L,
    parallel_chains = 4L,
    iter_warmup = 1000L,
    iter_sampling = 1000L,
    adapt_delta = 0.95,
    max_treedepth = 12L,
    refresh = 100L,
    output_dir = file.path("outputs", "sherlock_holmes_terms")
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
study_id <- "sherlock_holmes_terms"

data_dir <- file.path(repo_root, "data", "raw", study_id)
tables_in_dir <- file.path(data_dir, "tabelas_finais_auditadas")
model_input_dir <- file.path(data_dir, "model_inputs")
stan_dir <- file.path(repo_root, "scripts", study_id)
out_dir <- file.path(repo_root, opts$output_dir)
out_tables <- file.path(out_dir, "tables")
out_figures <- file.path(out_dir, "figures")
out_logs <- file.path(out_dir, "logs")
out_cmdstan <- file.path(out_dir, "cmdstan_csv")
for (d in c(out_tables, out_figures, out_logs, out_cmdstan)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

log_msg <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", ..., "\n")

require_pkg <- function(pkg, optional = FALSE) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- paste0("Pacote requerido não instalado: ", pkg)
    if (optional) return(FALSE)
    stop(msg, call. = FALSE)
  }
  TRUE
}

for (pkg in c("readr", "dplyr", "tidyr", "stringr", "ggplot2", "jsonlite")) require_pkg(pkg)

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(jsonlite)

read_required_csv <- function(path) {
  if (!file.exists(path)) stop("Arquivo não encontrado: ", path, call. = FALSE)
  readr::read_csv(path, show_col_types = FALSE)
}

require_file <- function(path) {
  if (!file.exists(path)) stop("Arquivo obrigatório não encontrado: ", path, call. = FALSE)
  invisible(path)
}

copy_audited_table <- function(filename) {
  src <- require_file(file.path(tables_in_dir, filename))
  dst <- file.path(out_tables, filename)
  file.copy(src, dst, overwrite = TRUE)
  dst
}

raw_path <- require_file(file.path(data_dir, "12_Sherlock_Holmes_Texts.csv"))
raw <- read_required_csv(raw_path)
required_raw_cols <- c("book", "text", "line_num")
missing_raw_cols <- setdiff(required_raw_cols, names(raw))
if (length(missing_raw_cols) > 0) {
  stop("Colunas ausentes no CSV bruto: ", paste(missing_raw_cols, collapse = ", "), call. = FALSE)
}

raw_audit <- raw |>
  mutate(text = dplyr::coalesce(as.character(text), ""), text_trim = stringr::str_squish(text)) |>
  summarise(
    obras_no_csv_bruto = dplyr::n_distinct(book),
    linhas_totais = dplyr::n(),
    linhas_com_texto = sum(nzchar(text_trim)),
    primeira_linha = min(line_num, na.rm = TRUE),
    ultima_linha = max(line_num, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(everything(), names_to = "metrica", values_to = "valor") |>
  mutate(valor = as.character(valor))
readr::write_csv(raw_audit, file.path(out_tables, "auditoria_csv_bruto_sherlock_holmes.csv"))

model_data <- read_required_csv(file.path(model_input_dir, "dados_modelo_obra_termo_e07.csv"))
required_model_cols <- c("book_canonical", "termo_canonico", "ocorrencias", "tokens_exposicao", "exposure_10k")
missing_model_cols <- setdiff(required_model_cols, names(model_data))
if (length(missing_model_cols) > 0) {
  stop("Colunas ausentes na base obra-termo: ", paste(missing_model_cols, collapse = ", "), call. = FALSE)
}

model_audit <- model_data |>
  summarise(
    obras_modeladas = dplyr::n_distinct(book_canonical),
    termos_modelados = dplyr::n_distinct(termo_canonico),
    celulas_obra_termo = dplyr::n(),
    ocorrencias_modeladas = sum(ocorrencias),
    tokens_literarios = sum(dplyr::distinct(model_data, book_canonical, tokens_exposicao)$tokens_exposicao),
    celulas_zeradas = sum(ocorrencias == 0),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(everything(), names_to = "metrica", values_to = "valor") |>
  mutate(valor = as.character(valor))
readr::write_csv(model_audit, file.path(out_tables, "auditoria_base_modelo_obra_termo.csv"))

final_tables <- c(
  "tabela_final_conclusoes_acionaveis.csv",
  "tabela_final_contrastes_prioritarios.csv",
  "tabela_final_diagnostico_validacao.csv",
  "tabela_final_efeitos_tipo_temporal.csv",
  "tabela_final_mensagens_chave.csv",
  "tabela_final_ppc_global.csv",
  "tabela_final_recomendacoes.csv",
  "tabela_final_resultados_nao_comunicar_isoladamente.csv",
  "tabela_final_sampler_cmdstan.csv",
  "tabela_final_sintese_bayesiana_auditavel.csv",
  "tabela_final_taxas_globais_termos.csv"
)

copied_tables <- vapply(final_tables, copy_audited_table, character(1))

# Figuras finais regeneráveis a partir das tabelas auditadas versionadas.
taxas <- read_required_csv(file.path(tables_in_dir, "tabela_final_taxas_globais_termos.csv"))
p_taxas <- taxas |>
  mutate(termo_canonico = reorder(termo_canonico, taxa_posterior_media_pond_10k)) |>
  ggplot(aes(x = termo_canonico, y = taxa_posterior_media_pond_10k)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Taxas posteriores globais por termo",
    subtitle = "Taxa posterior média ponderada por 10 mil tokens",
    x = NULL,
    y = "Taxa por 10 mil tokens"
  ) +
  theme_minimal(base_size = 11)
ggsave(file.path(out_figures, "grafico_taxas_globais_termos.png"), p_taxas, width = 9, height = 5, dpi = 160)

contrastes <- read_required_csv(file.path(tables_in_dir, "tabela_final_contrastes_prioritarios.csv"))
p_contrastes <- contrastes |>
  mutate(termo_canonico = reorder(termo_canonico, rate_ratio_median)) |>
  ggplot(aes(x = termo_canonico, y = rate_ratio_median, ymin = rate_ratio_hdi89_low, ymax = rate_ratio_hdi89_high)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_pointrange() +
  coord_flip() +
  labs(
    title = "Contrastes prioritários top1/top2 por termo",
    subtitle = "Razão de taxas posterior com intervalo HDI 89%",
    x = NULL,
    y = "Razão de taxas"
  ) +
  theme_minimal(base_size = 11)
ggsave(file.path(out_figures, "grafico_contrastes_top1_top2.png"), p_contrastes, width = 9, height = 5, dpi = 160)

ppc <- read_required_csv(file.path(tables_in_dir, "tabela_final_ppc_global.csv"))
p_ppc <- ppc |>
  select(estatistica, observado, yrep_median) |>
  pivot_longer(cols = c(observado, yrep_median), names_to = "serie", values_to = "valor") |>
  ggplot(aes(x = estatistica, y = valor, fill = serie)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Checagem preditiva posterior global",
    subtitle = "Estatísticas observadas versus mediana preditiva posterior",
    x = NULL,
    y = "Valor"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.title = element_blank())
ggsave(file.path(out_figures, "grafico_ppc_global.png"), p_ppc, width = 9, height = 5, dpi = 160)

sampler <- read_required_csv(file.path(tables_in_dir, "tabela_final_sampler_cmdstan.csv"))
p_sampler <- sampler |>
  mutate(chain = factor(chain)) |>
  ggplot(aes(x = chain, y = e_bfmi)) +
  geom_col() +
  geom_hline(yintercept = 0.3, linetype = "dashed") +
  labs(
    title = "Diagnóstico E-BFMI por cadeia",
    subtitle = "Linha tracejada em 0,3 como referência prática",
    x = "Cadeia",
    y = "E-BFMI"
  ) +
  theme_minimal(base_size = 11)
ggsave(file.path(out_figures, "grafico_ebfmi_cadeias.png"), p_sampler, width = 7, height = 4.5, dpi = 160)

if (run_stan) {
  require_pkg("cmdstanr")
  require_pkg("posterior")

  stan_path <- require_file(file.path(stan_dir, "modelo_operacional_poisson_lognormal_e07.stan"))
  data_json <- require_file(file.path(model_input_dir, "dados_stan_e07_entrada.json"))

  log_msg("Compilando modelo Stan", stan_path)
  mod <- cmdstanr::cmdstan_model(stan_path)

  log_msg("Iniciando amostragem CmdStan/NUTS")
  fit <- mod$sample(
    data = data_json,
    seed = opts$seed,
    chains = opts$chains,
    parallel_chains = opts$parallel_chains,
    iter_warmup = opts$iter_warmup,
    iter_sampling = opts$iter_sampling,
    adapt_delta = opts$adapt_delta,
    max_treedepth = opts$max_treedepth,
    refresh = opts$refresh,
    output_dir = out_cmdstan
  )

  readr::write_csv(fit$summary(), file.path(out_tables, "resumo_posterior_cmdstan_reexecutado.csv"))
  readr::write_csv(fit$sampler_diagnostics(format = "df"), file.path(out_tables, "sampler_diagnostics_cmdstan_reexecutado.csv"))
  fit$save_object(file.path(out_dir, "modelo_cmdstan_fit.rds"))
  log_msg("Amostragem finalizada")
} else {
  log_msg("Execução padrão sem reamostragem Stan. Use --run_stan=1 para reexecutar CmdStan/NUTS.")
}

manifest <- tibble::tibble(
  grupo = c("entrada", "entrada", "modelo", "saida", "saida", "saida"),
  caminho = c(
    raw_path,
    file.path(model_input_dir, "dados_modelo_obra_termo_e07.csv"),
    file.path(stan_dir, "modelo_operacional_poisson_lognormal_e07.stan"),
    out_tables,
    out_figures,
    if (run_stan) out_cmdstan else NA_character_
  ),
  funcao = c(
    "CSV bruto do corpus Sherlock Holmes.",
    "Base obra-termo usada na modelagem.",
    "Modelo Poisson-lognormal hierárquico com offset de exposição.",
    "Tabelas finais, auditorias e cópias dos artefatos auditáveis.",
    "Figuras regeneradas a partir das tabelas finais.",
    "CSVs brutos da reexecução CmdStan/NUTS, quando solicitada."
  )
)
readr::write_csv(manifest, file.path(out_logs, "manifesto_execucao_sherlock_holmes_terms.csv"))

log_msg("Sherlock Holmes finalizado. Saídas em", out_dir)
