#!/usr/bin/env Rscript

###############################################################################
# Composição dos motivos de retratação científica — R + Stan via rstan
# Base externa: Global Scientific Retractions 1927–2026 (Kaggle)
# Padrão do repositório:
#   - sem setwd()
#   - execução a partir da raiz do repo
#   - dados brutos fora do versionamento
#   - saída principal no console (sem artefatos do estudo por padrão)
#
# Como rodar:
#   Rscript scripts/retractions_reason_composition/retractions_reason_composition_rstan.R
#
# Exemplo com parâmetros:
#   Rscript scripts/retractions_reason_composition/retractions_reason_composition_rstan.R \
#     --top_publishers_n=10 --top_countries_n=12 --period_width=10 \
#     --min_total_mentions_per_stratum=20 --chains=4 --iter_warmup=1000 \
#     --iter_sampling=1000 --adapt_delta=0.97 --max_treedepth=13
#
# Dependências:
#   Rscript scripts/_setup/install_deps.R --all
###############################################################################

options(stringsAsFactors = FALSE)

parse_args <- function(args) {
  out <- list(
    input_csv = file.path(
      "data", "raw", "retractions_time_to_retraction",
      "global_scientific_retractions_1927_2026.csv"
    ),
    seed = 20260407L,
    top_publishers_n = 10L,
    top_countries_n = 12L,
    period_width = 10L,
    min_total_mentions_per_stratum = 20L,
    chains = 4L,
    iter_warmup = 1000L,
    iter_sampling = 1000L,
    adapt_delta = 0.97,
    max_treedepth = 13L,
    refresh = 100L,
    cores = NA_integer_,
    fail_on_unmapped = 1L
  )

  if (length(args) == 0L) return(out)

  if (any(args %in% c("--help", "-h"))) {
    cat(
      "Uso:\n",
      "  Rscript scripts/retractions_reason_composition/retractions_reason_composition_rstan.R [opcoes]\n\n",
      "Opções principais:\n",
      "  --input_csv=...\n",
      "  --seed=20260407\n",
      "  --top_publishers_n=10\n",
      "  --top_countries_n=12\n",
      "  --period_width=10\n",
      "  --min_total_mentions_per_stratum=20\n",
      "  --chains=4\n",
      "  --iter_warmup=1000\n",
      "  --iter_sampling=1000\n",
      "  --adapt_delta=0.97\n",
      "  --max_treedepth=13\n",
      "  --refresh=100\n",
      "  --cores=...\n",
      "  --fail_on_unmapped=1\n",
      sep = ""
    )
    quit(status = 0)
  }

  for (a in args) {
    if (!startsWith(a, "--")) next
    a2 <- sub("^--", "", a)
    if (!grepl("=", a2, fixed = TRUE)) next

    key <- sub("=.*$", "", a2)
    val <- sub("^.*=", "", a2)

    if (key %in% names(out)) out[[key]] <- val
  }

  int_keys <- c(
    "seed", "top_publishers_n", "top_countries_n", "period_width",
    "min_total_mentions_per_stratum", "chains", "iter_warmup",
    "iter_sampling", "max_treedepth", "refresh", "cores",
    "fail_on_unmapped"
  )
  num_keys <- c("adapt_delta")

  for (k in int_keys) {
    out[[k]] <- if (is.na(suppressWarnings(as.integer(out[[k]])))) {
      NA_integer_
    } else {
      as.integer(out[[k]])
    }
  }
  for (k in num_keys) out[[k]] <- as.numeric(out[[k]])

  out
}

require_pkgs <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    stop(
      "Pacotes obrigatórios ausentes: ", paste(missing, collapse = ", "),
      "\nInstale-os antes de rodar o script com: Rscript scripts/_setup/install_deps.R --all",
      call. = FALSE
    )
  }
}

print_section <- function(title) {
  cat("\n", strrep("=", 100), "\n", title, "\n", strrep("=", 100), "\n", sep = "")
}

fmt_pct <- function(x, digits = 1) {
  paste0(formatC(100 * x, format = "f", digits = digits), "%")
}

q025 <- function(x) as.numeric(stats::quantile(x, 0.025, na.rm = TRUE))
q500 <- function(x) as.numeric(stats::quantile(x, 0.500, na.rm = TRUE))
q975 <- function(x) as.numeric(stats::quantile(x, 0.975, na.rm = TRUE))

grepl_any <- function(x, patterns) {
  if (length(patterns) == 0L) return(rep(FALSE, length(x)))
  Reduce(`|`, lapply(patterns, function(p) stringr::str_detect(x, stringr::regex(p, ignore_case = TRUE))))
}

extract_country_main <- function(x) {
  parts <- stringr::str_split(stringr::str_squish(x), ";")
  out <- purrr::map_chr(parts, ~ {
    z <- stringr::str_squish(.x)
    z <- z[z != ""]
    if (length(z) == 0L) "Unknown" else z[1]
  })
  out[out == "" | is.na(out)] <- "Unknown"
  out
}

extract_subject_code <- function(x) {
  parts <- stringr::str_split(stringr::str_squish(x), ";")
  first_piece <- purrr::map_chr(parts, ~ {
    z <- stringr::str_squish(.x)
    z <- z[z != ""]
    if (length(z) == 0L) "UNK" else z[1]
  })
  code <- stringr::str_match(first_piece, "^\\(([^)]+)\\)")[, 2]
  code[is.na(code) | code == ""] <- "UNK"
  code
}

map_reason_group <- function(reason_token) {
  x <- stringr::str_squish(stringr::str_to_lower(reason_token))

  dplyr::case_when(
    # 1) Dados / resultados / métodos / reprodutibilidade
    grepl_any(x, c(
      "unreliable",
      "error in analyses",
      "error in data",
      "error in image",
      "error in results",
      "error in methods",
      "error in materials",
      "error in cell lines",
      "error in text",
      "results not reproducible",
      "concerns/issues about data",
      "concerns/issues about image",
      "concerns/issues about results",
      "concerns/issues about methods",
      "concerns/issues about article",
      "original data and/or images not provided",
      "contamination of cell lines",
      "contamination of materials"
    )) ~ "Data/Results/Methods",

    # 2) Fabricação / manipulação / fraude industrializada
    grepl_any(x, c(
      "falsification/fabrication",
      "manipulation of data",
      "manipulation of images",
      "manipulation of results",
      "paper mill",
      "computer-aided content or computer-generated content",
      "hoax paper",
      "misconduct - official investigation",
      "misconduct by author",
      "misconduct by company",
      "misconduct by third party",
      "euphemisms for misconduct",
      "sabotage of materials/methods"
    )) ~ "Fabrication/Manipulation",

    # 3) Plágio / duplicação / apropriação / atribuição
    grepl_any(x, c(
      "plagiarism",
      "duplication",
      "referencing/attributions",
      "salami slicing",
      "taken from dissertation/thesis",
      "taken via peer review",
      "taken via translation",
      "cites retracted work",
      "euphemisms for duplication",
      "euphemisms for plagiarism"
    )) ~ "Plagiarism/Duplication",

    # 4) Revisão por pares / editorial / processo de publicação
    grepl_any(x, c(
      "peer review",
      "rogue editor",
      "error by journal/publisher",
      "error by third party",
      "duplication of content through error by journal/publisher",
      "retract and replace",
      "updated to correction",
      "updated to retraction",
      "upgrade/update of prior notice",
      "temporary removal",
      "withdrawn to publish in different journal",
      "withdrawn as out of date",
      "not presented at conference",
      "nonpayment of fees"
    )) ~ "Peer Review/Editorial",

    # 5) Ética / legal / compliance / policy
    grepl_any(x, c(
      "animal welfare",
      "human subject welfare",
      "informed/patient consent",
      "irb/iacuc",
      "ethical violations",
      "conflict of interest",
      "breach of policy by author",
      "legal reasons",
      "civil proceedings",
      "criminal proceedings",
      "publishing ban",
      "copyright",
      "transfer of copyright"
    )) ~ "Ethics/Legal/Policy",

    # 6) Autoria / afiliação / disputas / aprovações
    grepl_any(x, c(
      "authorship/affiliation",
      "false/forged affiliation",
      "false/forged authorship",
      "lack of approval from author",
      "lack of approval from company",
      "lack of approval from third party",
      "author unresponsive",
      "complaints about author",
      "complaints about company",
      "complaints about third party",
      "miscommunication with/by author",
      "miscommunication with/by company",
      "miscommunication with/by journal/publisher",
      "miscommunication with/by third party",
      "objections by author",
      "objections by company",
      "objections by third party",
      "bias issues or lack of balance"
    )) ~ "Authorship/Dispute",

    # 7) Investigação / aviso / informação insuficiente
    grepl_any(x, c(
      "investigation by",
      "notice -",
      "date of article and/or notice unknown",
      "^removed$",
      "doing the right thing",
      "no further action",
      "concerns/issues about third party involvement"
    )) ~ "Investigation/Notice",

    TRUE ~ "Other"
  )
}

weighted_share_draws <- function(p_hat_draws, idx, weights, reason_levels) {
  idx <- as.integer(idx)
  w <- weights[idx]
  w <- w / sum(w)

  D <- dim(p_hat_draws)[1]
  K <- dim(p_hat_draws)[3]

  out <- matrix(NA_real_, nrow = D, ncol = K)
  colnames(out) <- reason_levels

  for (k in seq_len(K)) {
    arr_k <- p_hat_draws[, idx, k, drop = FALSE]

    if (length(idx) == 1L) {
      out[, k] <- as.numeric(arr_k[, 1, 1]) * w[1]
    } else {
      out[, k] <- as.vector(as.matrix(arr_k[, , 1]) %*% w)
    }
  }

  out
}

summarise_share_draws <- function(draw_mat, label) {
  tibble::tibble(
    group = label,
    reason_group = colnames(draw_mat),
    q2.5 = apply(draw_mat, 2, q025),
    q50 = apply(draw_mat, 2, q500),
    q97.5 = apply(draw_mat, 2, q975)
  )
}

as_percent_summary <- function(df) {
  df %>%
    dplyr::mutate(
      q2.5 = round(100 * q2.5, 1),
      q50 = round(100 * q50, 1),
      q97.5 = round(100 * q97.5, 1)
    )
}

ebfmi_chain <- function(energy_vec) {
  mean(diff(energy_vec)^2) / stats::var(energy_vec)
}

opt <- parse_args(commandArgs(trailingOnly = TRUE))

required_pkgs <- c(
  "readr", "rstan", "dplyr", "tidyr", "stringr", "purrr",
  "tibble", "posterior"
)
require_pkgs(required_pkgs)

suppressPackageStartupMessages({
  library(readr)
  library(rstan)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(posterior)
})

rstan::rstan_options(auto_write = FALSE)
available_cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
if (is.na(available_cores) || available_cores < 1L) available_cores <- 1L
mc_cores <- if (is.na(opt$cores) || opt$cores < 1L) {
  max(1L, min(opt$chains, available_cores))
} else {
  max(1L, min(opt$cores, available_cores))
}
options(mc.cores = mc_cores)
set.seed(opt$seed)

if (!file.exists(opt$input_csv)) {
  stop(
    "Arquivo CSV não encontrado em: ", opt$input_csv,
    "\nBaixe a base externa indicada em data/raw/retractions_time_to_retraction/README.md ",
    "e salve o arquivo com esse nome, ou informe --input_csv=...",
    call. = FALSE
  )
}

print_section("1. CONFIGURAÇÃO")
cat("input_csv                       :", opt$input_csv, "\n")
cat("seed                            :", opt$seed, "\n")
cat("top_publishers_n                :", opt$top_publishers_n, "\n")
cat("top_countries_n                 :", opt$top_countries_n, "\n")
cat("period_width                    :", opt$period_width, "\n")
cat("min_total_mentions_per_stratum  :", opt$min_total_mentions_per_stratum, "\n")
cat("chains                          :", opt$chains, "\n")
cat("iter_warmup                     :", opt$iter_warmup, "\n")
cat("iter_sampling                   :", opt$iter_sampling, "\n")
cat("adapt_delta                     :", opt$adapt_delta, "\n")
cat("max_treedepth                   :", opt$max_treedepth, "\n")
cat("refresh                         :", opt$refresh, "\n")
cat("mc.cores                        :", mc_cores, "\n")
cat("fail_on_unmapped                :", opt$fail_on_unmapped, "\n")

# -----------------------------------------------------------------------------
# Leitura e saneamento da base
# -----------------------------------------------------------------------------

print_section("2. LEITURA DA BASE E CHECAGEM INICIAL")

raw <- readr::read_csv(opt$input_csv, show_col_types = FALSE, progress = FALSE)

required_cols <- c(
  "record_id", "publisher", "country", "subject", "reason",
  "retraction_year", "original_year"
)
missing_required <- setdiff(required_cols, names(raw))
if (length(missing_required) > 0L) {
  stop("Colunas ausentes na base: ", paste(missing_required, collapse = ", "), call. = FALSE)
}

if ("row_quality_flag" %in% names(raw)) {
  raw <- raw %>% dplyr::filter(.data$row_quality_flag == "OK")
}

raw <- raw %>% dplyr::mutate(dplyr::across(where(is.character), stringr::str_squish))

n_initial <- nrow(raw)

raw <- raw %>%
  dplyr::mutate(
    retraction_year = as.integer(.data$retraction_year),
    original_year = as.integer(.data$original_year),
    invalid_temporal = !is.na(.data$original_year) & !is.na(.data$retraction_year) &
      .data$original_year > .data$retraction_year,
    missing_essential =
      is.na(.data$retraction_year) |
      is.na(.data$reason) | .data$reason == "" |
      is.na(.data$publisher) | .data$publisher == "" |
      is.na(.data$country) | .data$country == "" |
      is.na(.data$subject) | .data$subject == ""
  )

n_invalid_temporal <- sum(raw$invalid_temporal, na.rm = TRUE)
n_missing_essential <- sum(raw$missing_essential, na.rm = TRUE)

analytic_base <- raw %>%
  dplyr::filter(!.data$invalid_temporal, !.data$missing_essential) %>%
  dplyr::select(dplyr::all_of(required_cols))

cat("Registros iniciais:                     ", n_initial, "\n")
cat("Registros com data temporal inválida:  ", n_invalid_temporal, "\n")
cat("Registros com campos essenciais faltando:", n_missing_essential, "\n")
cat("Registros após saneamento:              ", nrow(analytic_base), "\n")

# -----------------------------------------------------------------------------
# Taxonomia analítica dos motivos
# -----------------------------------------------------------------------------

print_section("3. EXPLOSÃO DOS MOTIVOS E TAXONOMIA ANALÍTICA")

record_meta <- analytic_base %>%
  dplyr::transmute(
    record_id,
    publisher,
    country_main = extract_country_main(.data$country),
    subject_main = extract_subject_code(.data$subject),
    retraction_year,
    period_start = (.data$retraction_year %/% opt$period_width) * opt$period_width
  )

reason_long_raw <- analytic_base %>%
  dplyr::select(.data$record_id, .data$reason) %>%
  dplyr::mutate(reason_token = stringr::str_split(.data$reason, ";")) %>%
  tidyr::unnest(.data$reason_token) %>%
  dplyr::mutate(reason_token = stringr::str_squish(.data$reason_token)) %>%
  dplyr::filter(.data$reason_token != "")

reason_long <- reason_long_raw %>%
  dplyr::mutate(reason_group = map_reason_group(.data$reason_token))

unknown_reasons <- reason_long %>%
  dplyr::filter(.data$reason_group == "Other") %>%
  dplyr::count(.data$reason_token, sort = TRUE)

if (nrow(unknown_reasons) > 0L) {
  cat("Motivos não mapeados detectados:\n")
  print(unknown_reasons, n = Inf)

  if (isTRUE(opt$fail_on_unmapped == 1L)) {
    stop(
      "Existem motivos não mapeados pela taxonomia. ",
      "Ajuste map_reason_group() ou rode com --fail_on_unmapped=0 para descartá-los da modelagem.",
      call. = FALSE
    )
  }

  reason_long <- reason_long %>% dplyr::filter(.data$reason_group != "Other")
  cat("\nAviso: motivos não mapeados foram descartados da modelagem.\n")
}

reason_levels <- c(
  "Data/Results/Methods",
  "Plagiarism/Duplication",
  "Peer Review/Editorial",
  "Fabrication/Manipulation",
  "Ethics/Legal/Policy",
  "Authorship/Dispute",
  "Investigation/Notice"
)

reason_long <- reason_long %>%
  dplyr::select(.data$record_id, .data$reason_token, .data$reason_group) %>%
  dplyr::distinct(.data$record_id, .data$reason_group, .keep_all = TRUE) %>%
  dplyr::left_join(record_meta, by = "record_id") %>%
  dplyr::mutate(reason_group = factor(.data$reason_group, levels = reason_levels))

taxonomy_summary <- reason_long %>%
  dplyr::count(.data$reason_group, sort = TRUE, name = "n_mentions") %>%
  dplyr::mutate(share = .data$n_mentions / sum(.data$n_mentions))

cat("Número de registros únicos:             ", dplyr::n_distinct(reason_long$record_id), "\n")
cat("Número de presenças de macrogrupos:     ", nrow(reason_long), "\n")
cat(
  "Média de macrogrupos por registro:      ",
  round(nrow(reason_long) / dplyr::n_distinct(reason_long$record_id), 3),
  "\n\n",
  sep = ""
)

print(
  taxonomy_summary %>% dplyr::mutate(share = fmt_pct(.data$share)),
  n = Inf
)

# -----------------------------------------------------------------------------
# Estratificação para o modelo
# -----------------------------------------------------------------------------

print_section("4. CONSTRUÇÃO DOS ESTRATOS PARA MODELAGEM")

top_publishers <- record_meta %>%
  dplyr::count(.data$publisher, sort = TRUE) %>%
  dplyr::slice_head(n = opt$top_publishers_n) %>%
  dplyr::pull(.data$publisher)

top_countries <- record_meta %>%
  dplyr::count(.data$country_main, sort = TRUE) %>%
  dplyr::slice_head(n = opt$top_countries_n) %>%
  dplyr::pull(.data$country_main)

subject_order <- c("B/T", "BLS", "HSC", "PHY", "SOC", "ENV", "HUM", "UNK")

reason_model_base <- reason_long %>%
  dplyr::mutate(
    publisher_grp = dplyr::if_else(.data$publisher %in% top_publishers, .data$publisher, "Other"),
    country_grp = dplyr::if_else(.data$country_main %in% top_countries, .data$country_main, "Other"),
    subject_grp = dplyr::if_else(.data$subject_main %in% subject_order, .data$subject_main, "UNK"),
    period_grp = .data$period_start
  ) %>%
  dplyr::select(
    .data$record_id, .data$publisher_grp, .data$country_grp,
    .data$subject_grp, .data$period_grp, .data$reason_group
  )

counts_long <- reason_model_base %>%
  dplyr::count(
    .data$publisher_grp, .data$subject_grp, .data$country_grp,
    .data$period_grp, .data$reason_group, name = "n"
  )

strata <- reason_model_base %>%
  dplyr::distinct(.data$publisher_grp, .data$subject_grp, .data$country_grp, .data$period_grp)

model_long <- strata %>%
  tidyr::crossing(reason_group = factor(reason_levels, levels = reason_levels)) %>%
  dplyr::left_join(
    counts_long,
    by = c("publisher_grp", "subject_grp", "country_grp", "period_grp", "reason_group")
  ) %>%
  dplyr::mutate(n = dplyr::if_else(is.na(.data$n), 0L, as.integer(.data$n)))

strata_totals <- model_long %>%
  dplyr::group_by(.data$publisher_grp, .data$subject_grp, .data$country_grp, .data$period_grp) %>%
  dplyr::summarise(total_mentions = sum(.data$n), .groups = "drop")

kept_strata <- strata_totals %>%
  dplyr::filter(.data$total_mentions >= opt$min_total_mentions_per_stratum)

dropped_strata <- strata_totals %>%
  dplyr::filter(.data$total_mentions < opt$min_total_mentions_per_stratum)

model_wide <- model_long %>%
  dplyr::semi_join(
    kept_strata,
    by = c("publisher_grp", "subject_grp", "country_grp", "period_grp")
  ) %>%
  dplyr::mutate(reason_group = factor(.data$reason_group, levels = reason_levels)) %>%
  tidyr::pivot_wider(
    names_from = .data$reason_group,
    values_from = .data$n,
    values_fill = 0L
  ) %>%
  dplyr::arrange(.data$period_grp, .data$publisher_grp, .data$subject_grp, .data$country_grp)

if (nrow(model_wide) == 0L) {
  stop(
    "Nenhum estrato elegível restou após o filtro mínimo de total de menções por estrato.",
    call. = FALSE
  )
}

pub_levels <- c(
  sort(setdiff(unique(model_wide$publisher_grp), "Other")),
  if ("Other" %in% unique(model_wide$publisher_grp)) "Other" else character(0)
)
subj_levels <- subject_order[subject_order %in% unique(model_wide$subject_grp)]
ctry_levels <- c(
  sort(setdiff(unique(model_wide$country_grp), "Other")),
  if ("Other" %in% unique(model_wide$country_grp)) "Other" else character(0)
)
time_levels <- sort(unique(model_wide$period_grp))

model_wide <- model_wide %>%
  dplyr::mutate(
    pub_id = match(.data$publisher_grp, pub_levels),
    subj_id = match(.data$subject_grp, subj_levels),
    country_id = match(.data$country_grp, ctry_levels),
    period_id = match(.data$period_grp, time_levels)
  )

y_mat <- as.matrix(model_wide[, reason_levels, drop = FALSE])
storage.mode(y_mat) <- "integer"

if (any(rowSums(y_mat) <= 0L)) {
  stop("Há estratos com soma zero. Isso não deveria ocorrer após o filtro.", call. = FALSE)
}

cat("Estratos observados antes do filtro:     ", nrow(strata_totals), "\n")
cat("Estratos mantidos no modelo:             ", nrow(kept_strata), "\n")
cat("Estratos descartados por baixa massa:    ", nrow(dropped_strata), "\n")
cat("Número de macrogrupos K:                 ", length(reason_levels), "\n")
cat("Níveis de publisher:                     ", length(pub_levels), "\n")
cat("Níveis de subject:                       ", length(subj_levels), "\n")
cat("Níveis de country:                       ", length(ctry_levels), "\n")
cat("Níveis de período:                       ", length(time_levels), "\n\n")

print(
  kept_strata %>%
    dplyr::summarise(
      min_total = min(.data$total_mentions),
      p25 = as.numeric(stats::quantile(.data$total_mentions, 0.25)),
      median = stats::median(.data$total_mentions),
      mean = mean(.data$total_mentions),
      p75 = as.numeric(stats::quantile(.data$total_mentions, 0.75)),
      max_total = max(.data$total_mentions)
    )
)

# -----------------------------------------------------------------------------
# Dados para Stan
# -----------------------------------------------------------------------------

stan_data <- list(
  N = nrow(model_wide),
  K = ncol(y_mat),
  Km1 = ncol(y_mat) - 1L,
  y = y_mat,
  total_n = as.integer(rowSums(y_mat)),
  P = length(pub_levels),
  S = length(subj_levels),
  C = length(ctry_levels),
  T = length(time_levels),
  pub_id = as.integer(model_wide$pub_id),
  subj_id = as.integer(model_wide$subj_id),
  country_id = as.integer(model_wide$country_id),
  period_id = as.integer(model_wide$period_id)
)

# -----------------------------------------------------------------------------
# Stan: Dirichlet-multinomial hierárquico para composição
# -----------------------------------------------------------------------------

print_section("5. COMPILAÇÃO E AJUSTE DO MODELO STAN")

stan_code <- "
functions {
  real dirichlet_multinomial_lpmf(array[] int y, vector alpha) {
    int K = num_elements(y);
    int n_sum = 0;
    real alpha_sum = sum(alpha);
    real lp;

    for (k in 1:K) {
      n_sum += y[k];
    }

    lp = lgamma(alpha_sum)
         - lgamma(alpha_sum + n_sum)
         + lgamma(n_sum + 1);

    for (k in 1:K) {
      lp += lgamma(y[k] + alpha[k])
            - lgamma(alpha[k])
            - lgamma(y[k] + 1);
    }

    return lp;
  }
}

data {
  int<lower=1> N;
  int<lower=2> K;
  int<lower=1> Km1;
  array[N, K] int<lower=0> y;
  array[N] int<lower=1> total_n;

  int<lower=1> P;
  int<lower=1> S;
  int<lower=1> C;
  int<lower=1> T;

  array[N] int<lower=1, upper=P> pub_id;
  array[N] int<lower=1, upper=S> subj_id;
  array[N] int<lower=1, upper=C> country_id;
  array[N] int<lower=1, upper=T> period_id;
}

parameters {
  vector[Km1] alpha;

  matrix[Km1, P] z_pub;
  matrix[Km1, S] z_subj;
  matrix[Km1, C] z_country;
  matrix[Km1, T] z_period;

  vector<lower=0>[Km1] sigma_pub;
  vector<lower=0>[Km1] sigma_subj;
  vector<lower=0>[Km1] sigma_country;
  vector<lower=0>[Km1] sigma_period;

  real<lower=0> phi;
}

transformed parameters {
  matrix[Km1, P] b_pub;
  matrix[Km1, S] b_subj;
  matrix[Km1, C] b_country;
  matrix[Km1, T] b_period;

  for (k in 1:Km1) {
    real mu_pub = 0;
    real mu_subj = 0;
    real mu_country = 0;
    vector[T] tmp_period;

    for (p in 1:P) mu_pub += z_pub[k, p];
    mu_pub /= P;
    for (p in 1:P) b_pub[k, p] = sigma_pub[k] * (z_pub[k, p] - mu_pub);

    for (s in 1:S) mu_subj += z_subj[k, s];
    mu_subj /= S;
    for (s in 1:S) b_subj[k, s] = sigma_subj[k] * (z_subj[k, s] - mu_subj);

    for (c in 1:C) mu_country += z_country[k, c];
    mu_country /= C;
    for (c in 1:C) b_country[k, c] = sigma_country[k] * (z_country[k, c] - mu_country);

    tmp_period[1] = sigma_period[k] * z_period[k, 1];
    for (t in 2:T) tmp_period[t] = tmp_period[t - 1] + sigma_period[k] * z_period[k, t];
    tmp_period = tmp_period - mean(tmp_period);
    for (t in 1:T) b_period[k, t] = tmp_period[t];
  }
}

model {
  alpha ~ normal(0, 1.0);

  to_vector(z_pub) ~ normal(0, 1);
  to_vector(z_subj) ~ normal(0, 1);
  to_vector(z_country) ~ normal(0, 1);
  to_vector(z_period) ~ normal(0, 1);

  sigma_pub ~ normal(0, 0.35);
  sigma_subj ~ normal(0, 0.35);
  sigma_country ~ normal(0, 0.35);
  sigma_period ~ normal(0, 0.25);

  phi ~ lognormal(log(20), 0.5);

  for (n in 1:N) {
    vector[K] eta;
    vector[K] conc;

    for (k in 1:Km1) {
      eta[k] =
        alpha[k] +
        b_pub[k, pub_id[n]] +
        b_subj[k, subj_id[n]] +
        b_country[k, country_id[n]] +
        b_period[k, period_id[n]];
    }

    eta[K] = 0;
    conc = softmax(eta) * phi;

    target += dirichlet_multinomial_lpmf(y[n] | conc);
  }
}

generated quantities {
  array[N] real log_lik;
  matrix[N, K] p_hat;

  for (n in 1:N) {
    vector[K] eta;
    vector[K] conc;
    vector[K] p;

    for (k in 1:Km1) {
      eta[k] =
        alpha[k] +
        b_pub[k, pub_id[n]] +
        b_subj[k, subj_id[n]] +
        b_country[k, country_id[n]] +
        b_period[k, period_id[n]];
    }

    eta[K] = 0;
    p = softmax(eta);
    conc = p * phi;

    p_hat[n] = to_row_vector(p);
    log_lik[n] = dirichlet_multinomial_lpmf(y[n] | conc);
  }
}
"

sm <- rstan::stan_model(
  model_code = stan_code,
  model_name = "retractions_reason_composition_dm_hier"
)

fit <- rstan::sampling(
  object = sm,
  data = stan_data,
  seed = opt$seed,
  chains = opt$chains,
  iter = opt$iter_warmup + opt$iter_sampling,
  warmup = opt$iter_warmup,
  thin = 1,
  refresh = opt$refresh,
  control = list(
    adapt_delta = opt$adapt_delta,
    max_treedepth = opt$max_treedepth
  )
)

# -----------------------------------------------------------------------------
# Resumo dos hiperparâmetros
# -----------------------------------------------------------------------------

print_section("6. RESUMO DOS HIPERPARÂMETROS")

hyper_summary <- rstan::summary(
  fit,
  pars = c("alpha", "sigma_pub", "sigma_subj", "sigma_country", "sigma_period", "phi"),
  probs = c(0.025, 0.50, 0.975)
)$summary %>%
  as.data.frame() %>%
  tibble::rownames_to_column("parameter") %>%
  dplyr::select(.data$parameter, .data$mean, .data$sd, `2.5%`, `50%`, `97.5%`, .data$n_eff, .data$Rhat)

print(hyper_summary, row.names = FALSE)

# -----------------------------------------------------------------------------
# Diagnósticos MCMC
# -----------------------------------------------------------------------------

print_section("7. DIAGNÓSTICOS MCMC")

sampler_params <- rstan::get_sampler_params(fit, inc_warmup = FALSE)

n_divergent <- sum(purrr::map_dbl(sampler_params, ~ sum(.x[, "divergent__"])))
n_treedepth <- sum(purrr::map_dbl(sampler_params, ~ sum(.x[, "treedepth__"] >= opt$max_treedepth)))
ebfmi_vals <- purrr::map_dbl(sampler_params, ~ ebfmi_chain(.x[, "energy__"]))

draws_array <- posterior::as_draws_array(fit)
var_names <- posterior::variables(draws_array)

diag_vars <- var_names[
  stringr::str_detect(
    var_names,
    "^(alpha|sigma_pub|sigma_subj|sigma_country|sigma_period|phi|z_pub|z_subj|z_country|z_period)(\\[|$)"
  )
]

diag_tbl <- posterior::summarise_draws(
  posterior::subset_draws(draws_array, variable = diag_vars),
  mean = base::mean,
  sd = stats::sd,
  rhat = posterior::rhat,
  ess_bulk = posterior::ess_bulk,
  ess_tail = posterior::ess_tail
) %>%
  tibble::as_tibble()

names(diag_tbl) <- tolower(names(diag_tbl))
names(diag_tbl) <- sub("^posterior::", "", names(diag_tbl))

needed_cols <- c("rhat", "ess_bulk", "ess_tail")
missing_cols <- setdiff(needed_cols, names(diag_tbl))
if (length(missing_cols) > 0L) {
  stop("Colunas ausentes em diag_tbl: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

rhat_max <- max(diag_tbl[["rhat"]], na.rm = TRUE)
ess_bulk_min <- min(diag_tbl[["ess_bulk"]], na.rm = TRUE)
ess_tail_min <- min(diag_tbl[["ess_tail"]], na.rm = TRUE)
ebfmi_min <- min(ebfmi_vals, na.rm = TRUE)

diag_overall <- tibble::tibble(
  metric = c(
    "Rhat_max",
    "ESS_bulk_min",
    "ESS_tail_min",
    "Divergences",
    "Treedepth_hits",
    "E_BFMI_min"
  ),
  value = c(
    rhat_max,
    ess_bulk_min,
    ess_tail_min,
    n_divergent,
    n_treedepth,
    ebfmi_min
  )
)

print(diag_overall, n = Inf)

cat("\nTop 10 piores Rhat:\n")
print(
  diag_tbl %>%
    dplyr::arrange(dplyr::desc(.data[["rhat"]])) %>%
    dplyr::slice_head(n = 10),
  n = Inf
)

cat("\nTop 10 menores ESS bulk:\n")
print(
  diag_tbl %>%
    dplyr::arrange(.data[["ess_bulk"]]) %>%
    dplyr::slice_head(n = 10),
  n = Inf
)

cat("\nE-BFMI por cadeia:\n")
print(round(ebfmi_vals, 3))

diag_ok <- (
  is.finite(rhat_max) &&
    rhat_max <= 1.01 &&
    ess_bulk_min >= 400 &&
    ess_tail_min >= 400 &&
    n_divergent == 0 &&
    n_treedepth == 0 &&
    ebfmi_min > 0.30
)

cat("\nParecer automático de MCMC: ", if (diag_ok) "APROVADO" else "REVISAR AJUSTE", "\n", sep = "")

# -----------------------------------------------------------------------------
# Extração das probabilidades posteriores
# -----------------------------------------------------------------------------

print_section("8. EXTRAÇÃO DAS PROBABILIDADES POSTERIORES")

p_hat_draws <- rstan::extract(fit, pars = "p_hat", permuted = TRUE)$p_hat
obs_y <- y_mat
weights_strata <- rowSums(obs_y)

# -----------------------------------------------------------------------------
# Posterior predictive checks
# -----------------------------------------------------------------------------

print_section("9. POSTERIOR PREDICTIVE CHECKS")

obs_share <- obs_y / rowSums(obs_y)
p_mean <- apply(p_hat_draws, c(2, 3), mean)

mae_comp <- mean(abs(obs_share - p_mean))
rmse_comp <- sqrt(mean((obs_share - p_mean)^2))

cat("MAE médio das composições por estrato:  ", round(mae_comp, 4), "\n")
cat("RMSE médio das composições por estrato: ", round(rmse_comp, 4), "\n")

worst_strata <- model_wide %>%
  dplyr::transmute(
    publisher_grp,
    subject_grp,
    country_grp,
    period_grp,
    total_mentions = rowSums(obs_y),
    mae_stratum = rowMeans(abs(obs_share - p_mean))
  ) %>%
  dplyr::arrange(dplyr::desc(.data$mae_stratum)) %>%
  dplyr::slice_head(n = 10)

cat("\nTop 10 estratos com maior erro médio absoluto da composição:\n")
print(worst_strata, n = Inf)

set.seed(opt$seed + 1L)
ndraws_total <- dim(p_hat_draws)[1]
ppc_draw_ids <- sample(seq_len(ndraws_total), size = min(400L, ndraws_total), replace = FALSE)

ppc_totals <- matrix(0, nrow = length(ppc_draw_ids), ncol = length(reason_levels))
colnames(ppc_totals) <- reason_levels

for (i in seq_along(ppc_draw_ids)) {
  d <- ppc_draw_ids[i]
  sim_total <- rep(0L, length(reason_levels))

  for (n in seq_len(nrow(obs_y))) {
    sim_total <- sim_total + as.vector(
      rmultinom(
        n = 1L,
        size = rowSums(obs_y)[n],
        prob = p_hat_draws[d, n, ]
      )
    )
  }

  ppc_totals[i, ] <- sim_total
}

obs_totals <- colSums(obs_y)

ppc_tbl <- tibble::tibble(
  reason_group = reason_levels,
  observed = obs_totals,
  ppc_q2.5 = apply(ppc_totals, 2, q025),
  ppc_q50 = apply(ppc_totals, 2, q500),
  ppc_q97.5 = apply(ppc_totals, 2, q975)
) %>%
  dplyr::mutate(covered = .data$observed >= .data$ppc_q2.5 & .data$observed <= .data$ppc_q97.5)

print(ppc_tbl, n = Inf)

cat(
  "\nCobertura PPC agregada por macrogrupo: ",
  sum(ppc_tbl$covered), "/", nrow(ppc_tbl),
  " = ", fmt_pct(mean(ppc_tbl$covered)), "\n",
  sep = ""
)

# -----------------------------------------------------------------------------
# Composição posterior global
# -----------------------------------------------------------------------------

print_section("10. COMPOSIÇÃO POSTERIOR GLOBAL")

global_draws <- weighted_share_draws(
  p_hat_draws = p_hat_draws,
  idx = seq_len(nrow(model_wide)),
  weights = weights_strata,
  reason_levels = reason_levels
)

global_summary <- summarise_share_draws(global_draws, "Global") %>%
  as_percent_summary()

print(global_summary, n = Inf)

# -----------------------------------------------------------------------------
# Composição por publisher
# -----------------------------------------------------------------------------

print_section("11. COMPOSIÇÃO POSTERIOR POR PUBLISHER (TOP 5 NA AMOSTRA MODELADA)")

publisher_report_levels <- model_wide %>%
  dplyr::count(.data$publisher_grp, wt = weights_strata, sort = TRUE, name = "total_mentions") %>%
  dplyr::slice_head(n = 5) %>%
  dplyr::pull(.data$publisher_grp)

publisher_summary <- purrr::map_dfr(publisher_report_levels, function(g) {
  idx <- which(model_wide$publisher_grp == g)
  summarise_share_draws(
    weighted_share_draws(
      p_hat_draws = p_hat_draws,
      idx = idx,
      weights = weights_strata,
      reason_levels = reason_levels
    ),
    label = g
  )
}) %>%
  as_percent_summary()

print(publisher_summary, n = Inf)

# -----------------------------------------------------------------------------
# Composição por área
# -----------------------------------------------------------------------------

print_section("12. COMPOSIÇÃO POSTERIOR POR ÁREA")

subject_summary <- purrr::map_dfr(subj_levels, function(g) {
  idx <- which(model_wide$subject_grp == g)
  summarise_share_draws(
    weighted_share_draws(
      p_hat_draws = p_hat_draws,
      idx = idx,
      weights = weights_strata,
      reason_levels = reason_levels
    ),
    label = g
  )
}) %>%
  as_percent_summary()

print(subject_summary, n = Inf)

# -----------------------------------------------------------------------------
# Composição por país
# -----------------------------------------------------------------------------

print_section("13. COMPOSIÇÃO POSTERIOR POR PAÍS (TOP 5 NA AMOSTRA MODELADA)")

country_report_levels <- model_wide %>%
  dplyr::count(.data$country_grp, wt = weights_strata, sort = TRUE, name = "total_mentions") %>%
  dplyr::slice_head(n = 5) %>%
  dplyr::pull(.data$country_grp)

country_summary <- purrr::map_dfr(country_report_levels, function(g) {
  idx <- which(model_wide$country_grp == g)
  summarise_share_draws(
    weighted_share_draws(
      p_hat_draws = p_hat_draws,
      idx = idx,
      weights = weights_strata,
      reason_levels = reason_levels
    ),
    label = g
  )
}) %>%
  as_percent_summary()

print(country_summary, n = Inf)

# -----------------------------------------------------------------------------
# Mudança temporal da composição
# -----------------------------------------------------------------------------

print_section("14. MUDANÇA TEMPORAL DA COMPOSIÇÃO")

first_period <- min(time_levels)
last_period <- max(time_levels)

draws_first <- weighted_share_draws(
  p_hat_draws = p_hat_draws,
  idx = which(model_wide$period_grp == first_period),
  weights = weights_strata,
  reason_levels = reason_levels
)

draws_last <- weighted_share_draws(
  p_hat_draws = p_hat_draws,
  idx = which(model_wide$period_grp == last_period),
  weights = weights_strata,
  reason_levels = reason_levels
)

delta_draws <- draws_last - draws_first

delta_summary <- tibble::tibble(
  reason_group = reason_levels,
  q2.5 = apply(delta_draws, 2, q025),
  q50 = apply(delta_draws, 2, q500),
  q97.5 = apply(delta_draws, 2, q975)
) %>%
  dplyr::mutate(
    q2.5 = round(100 * .data$q2.5, 1),
    q50 = round(100 * .data$q50, 1),
    q97.5 = round(100 * .data$q97.5, 1)
  )

cat("Período inicial:", first_period, "\n")
cat("Período final:  ", last_period, "\n\n")
cat("Variação em pontos percentuais (período final - período inicial):\n")
print(delta_summary, n = Inf)

# -----------------------------------------------------------------------------
# Parecer final automático
# -----------------------------------------------------------------------------

print_section("15. PARECER FINAL DO SCRIPT")

cat("Modelo final: Dirichlet-multinomial hierárquico para composição de macrogrupos de motivos.\n")
cat("Unidade modelada: composição de presenças de macrogrupos por estrato publisher x área x país x período.\n")
cat("Estratos usados no ajuste: ", nrow(model_wide), "\n", sep = "")
cat("Total de menções de macrogrupos no modelo: ", sum(weights_strata), "\n", sep = "")
cat("Status MCMC: ", if (diag_ok) "APROVADO" else "REVISAR", "\n", sep = "")
cat("MAE composição por estrato: ", round(mae_comp, 4), "\n", sep = "")
cat("RMSE composição por estrato: ", round(rmse_comp, 4), "\n", sep = "")
cat(
  "Cobertura PPC agregada: ", sum(ppc_tbl$covered), "/", nrow(ppc_tbl),
  " = ", fmt_pct(mean(ppc_tbl$covered)), "\n",
  sep = ""
)

if (!diag_ok) {
  cat("\nAjustes sugeridos se o parecer não aprovar:\n")
  cat("- aumentar iter_sampling para 1500 ou 2000;\n")
  cat("- subir adapt_delta para 0.98 ou 0.99;\n")
  cat("- aumentar min_total_mentions_per_stratum para reduzir ruído de estratos muito raros;\n")
  cat("- reduzir top_publishers_n/top_countries_n se houver ESS baixo persistente;\n")
  cat("- manter a taxonomia fixa e só então comparar sensibilidade em um segundo ajuste.\n")
}
