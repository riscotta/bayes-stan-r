#!/usr/bin/env Rscript

# Sobrevivência cadastral de estabelecimentos de alimentação fora do lar em Porto Alegre
# Execução a partir da raiz do repositório:
#   Rscript scripts/sobrevivencia_cadastral_poa/sobrevivencia_cadastral_poa_cmdstanr.R
# Reestimação opcional:
#   Rscript scripts/sobrevivencia_cadastral_poa/sobrevivencia_cadastral_poa_cmdstanr.R --run_stan=1

options(stringsAsFactors = FALSE, scipen = 999, warn = 1)

parse_args <- function(args) {
  cfg <- list(run_stan = FALSE, chains = 4L, parallel_chains = 4L,
              iter_warmup = 1500L, iter_sampling = 1000L,
              adapt_delta = 0.95, max_treedepth = 13L,
              metric = "dense_e", seed = 20260721L)
  for (arg in args) {
    if (!grepl("^--[^=]+=", arg)) stop("Use argumentos no formato --chave=valor.")
    key <- gsub("-", "_", sub("^--([^=]+)=.*$", "\\1", arg))
    value <- sub("^--[^=]+=", "", arg)
    if (!key %in% names(cfg)) stop("Argumento desconhecido: --", key)
    if (is.logical(cfg[[key]])) {
      cfg[[key]] <- tolower(value) %in% c("1", "true", "sim", "yes")
    } else if (is.integer(cfg[[key]])) {
      cfg[[key]] <- as.integer(value)
    } else if (is.numeric(cfg[[key]])) {
      cfg[[key]] <- as.numeric(value)
    } else {
      cfg[[key]] <- value
    }
  }
  cfg
}

find_repo_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, "README.md")) &&
        dir.exists(file.path(current, "scripts")) &&
        dir.exists(file.path(current, "data"))) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) stop("Raiz do repositório não encontrada.")
    current <- parent
  }
}

require_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Pacotes R ausentes: ", paste(missing, collapse = ", "))
}

read_delim_auto <- function(path) {
  first <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
  delim <- if (length(first) && lengths(regmatches(first, gregexpr(";", first, fixed = TRUE))) >
               lengths(regmatches(first, gregexpr(",", first, fixed = TRUE)))) ";" else ","
  readr::read_delim(path, delim = delim, show_col_types = FALSE,
                    locale = readr::locale(encoding = "UTF-8"), trim_ws = TRUE)
}

copy_tree <- function(from, to) {
  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(from, full.names = TRUE, recursive = TRUE)
  for (file in files) {
    rel <- substring(file, nchar(from) + 2L)
    target <- file.path(to, rel)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(file, target, overwrite = TRUE, copy.date = TRUE)) {
      stop("Falha ao copiar: ", file)
    }
  }
}

cfg <- parse_args(commandArgs(trailingOnly = TRUE))
root <- find_repo_root()
study <- "sobrevivencia_cadastral_poa"
script_dir <- file.path(root, "scripts", study)
data_dir <- file.path(root, "data", "raw", study)
out_dir <- file.path(root, "outputs", study)
tables_out <- file.path(out_dir, "tables")
figures_out <- file.path(out_dir, "figures")
logs_out <- file.path(out_dir, "logs")
models_out <- file.path(out_dir, "models")
for (p in c(tables_out, figures_out, logs_out, models_out)) dir.create(p, recursive = TRUE, showWarnings = FALSE)

require_packages(c("readr", "dplyr", "ggplot2", "scales", "jsonlite"))

# A execução padrão recompõe a entrega publicada a partir de artefatos auditáveis.
copy_tree(file.path(data_dir, "resultados_auditados"), tables_out)

resumo <- read_delim_auto(file.path(data_dir, "resultados_auditados", "resumo_executivo.csv"))
surv <- read_delim_auto(file.path(data_dir, "resultados_auditados", "sobrevivencia_horizontes.csv"))
coortes <- read_delim_auto(file.path(data_dir, "resultados_auditados", "estimandos_coorte_selecionados.csv"))

# Regera duas figuras centrais a partir das tabelas auditadas.
p1 <- ggplot2::ggplot(surv, ggplot2::aes(x = horizonte_meses)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = intervalo_90_inferior,
                                    ymax = intervalo_90_superior), alpha = 0.18) +
  ggplot2::geom_line(ggplot2::aes(y = sobrevivencia_predita_mediana,
                                  linetype = "Predita (mediana)"), linewidth = 0.9) +
  ggplot2::geom_point(ggplot2::aes(y = sobrevivencia_observada,
                                   shape = "Observada"), size = 2.4) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  ggplot2::scale_x_continuous(breaks = surv$horizonte_meses) +
  ggplot2::labs(x = "Meses desde a abertura", y = "Sobrevivência cadastral",
                linetype = NULL, shape = NULL,
                title = "Sobrevivência cadastral observada e predita") +
  ggplot2::theme_minimal(base_size = 11)
ggplot2::ggsave(file.path(figures_out, "sobrevivencia_observada_predita_recriada.png"),
                p1, width = 8, height = 5, dpi = 160)

coortes12 <- dplyr::filter(coortes, horizonte_meses == 12)
p2 <- ggplot2::ggplot(coortes12,
                      ggplot2::aes(x = factor(coorte_ano), y = prob_baixa_mediana)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = q5, ymax = q95), width = 0.15) +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::labs(x = "Coorte de abertura", y = "Probabilidade de baixa em 12 meses",
                title = "Risco cadastral por coorte no perfil de referência") +
  ggplot2::theme_minimal(base_size = 11)
ggplot2::ggsave(file.path(figures_out, "risco_12m_por_coorte_recriado.png"),
                p2, width = 8, height = 5, dpi = 160)

if (cfg$run_stan) {
  require_packages(c("cmdstanr", "posterior"))
  version <- tryCatch(cmdstanr::cmdstan_version(), error = function(e) NULL)
  if (is.null(version)) stop("CmdStan não está configurado. Execute scripts/_setup/install_cmdstan.R.")

  run_model <- function(label) {
    stan_file <- file.path(script_dir, paste0("modelo_", label, "_rw2_centrado_qr.stan"))
    data_gz <- file.path(data_dir, "model_inputs", paste0("stan_data_", label, "_agregado.json.gz"))
    data_file <- file.path(tempdir(), paste0("stan_data_", label, "_agregado.json"))
    in_con <- gzfile(data_gz, open = "rb")
    raw_data <- readBin(in_con, what = "raw", n = 1e8)
    close(in_con)
    out_con <- file(data_file, open = "wb")
    writeBin(raw_data, out_con)
    close(out_con)
    csv_dir <- file.path(models_out, paste0(label, "_cmdstan_csv"))
    dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
    model <- cmdstanr::cmdstan_model(stan_file, quiet = FALSE)
    fit <- model$sample(
      data = data_file,
      chains = cfg$chains,
      parallel_chains = cfg$parallel_chains,
      iter_warmup = cfg$iter_warmup,
      iter_sampling = cfg$iter_sampling,
      adapt_delta = cfg$adapt_delta,
      max_treedepth = cfg$max_treedepth,
      metric = cfg$metric,
      seed = cfg$seed,
      output_dir = csv_dir,
      refresh = 100
    )
    readr::write_csv(fit$summary(), file.path(tables_out, paste0("resumo_posterior_", label, "_reamostrado.csv")))
    readr::write_csv(fit$diagnostic_summary(), file.path(tables_out, paste0("diagnosticos_", label, "_reamostrado.csv")))
    fit
  }

  fit_m0 <- run_model("M0")
  fit_m1 <- run_model("M1")
  invisible(fit_m0); invisible(fit_m1)
}

cat("\nSobrevivência cadastral de estabelecimentos de alimentação fora do lar em Porto Alegre\n")
print(resumo, n = Inf)
cat("\nSaídas: ", out_dir, "\n", sep = "")
cat("Escopo: descrição, associação e predição interna; não há identificação causal nem validação prospectiva.\n")
