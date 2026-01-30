#!/usr/bin/env Rscript

# scripts/_setup/install_deps.R
# Instala dependências R do repositório (modo mínimo ou completo).
#
# Uso:
#   Rscript scripts/_setup/install_deps.R
#   Rscript scripts/_setup/install_deps.R --all
#   Rscript scripts/_setup/install_deps.R --no-pak
#
# Flags:
#   --all     instala um conjunto mais amplo (diagnósticos, relatórios, etc.)
#   --no-pak  força install.packages mesmo se 'pak' estiver disponível

args <- commandArgs(trailingOnly = TRUE)
flag_all   <- "--all"   %in% args
flag_no_pak <- "--no-pak" %in% args

message("==> instalando dependências R (", if (flag_all) "ALL" else "MINIMAL", ")")

repos <- getOption("repos")
if (is.null(repos) || identical(repos, "@CRAN@") || length(repos) == 0) {
  repos <- c(CRAN = "https://cloud.r-project.org")
  options(repos = repos)
}

use_pak <- FALSE
if (!flag_no_pak) {
  use_pak <- requireNamespace("pak", quietly = TRUE)
}

install_pkgs <- function(pkgs) {
  pkgs <- unique(pkgs)
  if (length(pkgs) == 0) return(invisible(NULL))

  if (use_pak) {
    message("==> usando 'pak' para instalar/atualizar pacotes...")
    pak::pkg_install(pkgs)
  } else {
    message("==> usando install.packages()...")
    install.packages(pkgs, repos = repos)
  }
}

installed <- rownames(installed.packages())
is_installed <- function(p) p %in% installed

# Base mínima: o suficiente para rodar scripts típicos e cmdstanr
pkgs_min <- c(
  "cmdstanr",
  "posterior",
  "bayesplot",
  "ggplot2",
  "dplyr",
  "tidyr",
  "readr",
  "purrr",
  "tibble",
  "stringr",
  "here",
  "fs",
  "glue",
  "cli",
  "checkmate"
)

# Conjunto expandido (quando o repo começar a crescer, isso evita dor de cabeça)
pkgs_all <- c(
  pkgs_min,
  "loo",
  "withr",
  "testthat",
  "knitr",
  "rmarkdown",
  "yaml"
)

pkgs <- if (flag_all) pkgs_all else pkgs_min
missing <- pkgs[!vapply(pkgs, is_installed, logical(1))]

if (length(missing) == 0) {
  message("==> tudo certo: nenhum pacote faltando.")
} else {
  message("==> pacotes faltando: ", paste(missing, collapse = ", "))
  install_pkgs(missing)
}

# Check rápido
message("\n==> versões (principais):")
show_ver <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    v <- as.character(utils::packageVersion(pkg))
    message(sprintf("  - %-10s %s", pkg, v))
  } else {
    message(sprintf("  - %-10s (não instalado)", pkg))
  }
}
invisible(lapply(c("cmdstanr", "posterior", "bayesplot", "ggplot2", "dplyr"), show_ver))

message("\nOK. Dependências instaladas.")
