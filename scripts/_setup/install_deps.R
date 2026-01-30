#!/usr/bin/env Rscript

# Instala dependências R do repositório.
# Uso:
#   Rscript scripts/_setup/install_deps.R
#   Rscript scripts/_setup/install_deps.R --all
#   Rscript scripts/_setup/install_deps.R --no-pak

args <- commandArgs(trailingOnly = TRUE)
flag_all    <- "--all" %in% args
flag_no_pak <- "--no-pak" %in% args

# CRAN default (para ambientes “limpos”)
repos <- getOption("repos")
if (is.null(repos) || identical(repos, "@CRAN@") || length(repos) == 0) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

use_pak <- (!flag_no_pak) && requireNamespace("pak", quietly = TRUE)

install_pkgs <- function(pkgs) {
  pkgs <- unique(pkgs)
  if (length(pkgs) == 0) return(invisible(NULL))
  if (use_pak) pak::pkg_install(pkgs) else install.packages(pkgs)
}

# “mínimo” para os scripts que você já tem hoje
pkgs_min <- c(
  "cmdstanr",
  "posterior",
  "bayesplot",
  "loo",
  "ggplot2",
  "dplyr",
  "tidyr",
  "readr",
  "tibble",
  "stringr",
  "here"
)

# “all” (ferramentas úteis pro repo crescer)
pkgs_all <- c(
  pkgs_min,
  "fs",
  "glue",
  "cli",
  "checkmate",
  "withr",
  "testthat",
  "knitr",
  "rmarkdown",
  "yaml"
)

pkgs <- if (flag_all) pkgs_all else pkgs_min
installed <- rownames(installed.packages())
missing <- pkgs[!pkgs %in% installed]

message("==> instalando dependências R (", if (flag_all) "ALL" else "MINIMAL", ")")
if (length(missing) == 0) {
  message("==> tudo certo: nenhum pacote faltando.")
} else {
  message("==> pacotes faltando: ", paste(missing, collapse = ", "))
  install_pkgs(missing)
}
message("OK.")
