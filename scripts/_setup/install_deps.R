#!/usr/bin/env Rscript

# Instala dependências R do repositório.
# Uso:
#   Rscript scripts/_setup/install_deps.R
#   Rscript scripts/_setup/install_deps.R --all
#   Rscript scripts/_setup/install_deps.R --no-pak

args <- commandArgs(trailingOnly = TRUE)
flag_all    <- "--all" %in% args
flag_no_pak <- "--no-pak" %in% args

# Repositórios: garante CRAN + Stan (r-universe) para instalar cmdstanr sem dor.
# - cmdstanr normalmente NÃO está no CRAN.
# - Stan recomenda: https://stan-dev.r-universe.dev
ensure_repos <- function() {
  repos <- getOption("repos")

  # casos comuns: repos NULL, vazio, ou CRAN apontando para "@CRAN@"
  if (is.null(repos) || length(repos) == 0) repos <- c()
  if (identical(repos, "@CRAN@")) repos <- c()
  if (!is.null(names(repos)) && "CRAN" %in% names(repos) && identical(repos[["CRAN"]], "@CRAN@")) {
    repos[["CRAN"]] <- NULL
  }

  # garante nomes (caso algum ambiente devolva vetor sem names)
  if (length(repos) > 0 && is.null(names(repos))) {
    names(repos) <- paste0("repo", seq_along(repos))
  }

  # defaults seguros
  if (!("CRAN" %in% names(repos)) || is.null(repos[["CRAN"]]) || repos[["CRAN"]] == "") {
    repos[["CRAN"]] <- "https://cloud.r-project.org"
  }
  if (!("stan" %in% names(repos)) || is.null(repos[["stan"]]) || repos[["stan"]] == "") {
    repos[["stan"]] <- "https://stan-dev.r-universe.dev"
  }

  options(repos = repos)
}

ensure_repos()

use_pak <- (!flag_no_pak) && requireNamespace("pak", quietly = TRUE)

install_pkgs <- function(pkgs) {
  pkgs <- unique(pkgs)
  if (length(pkgs) == 0) return(invisible(NULL))
  if (use_pak) {
    pak::pkg_install(pkgs)
  } else {
    install.packages(pkgs)
  }
}

# “mínimo” para os scripts que você já tem hoje
pkgs_min <- c(
  "data.table",
  "cmdstanr",
  "posterior",
  "bayesplot",
  "loo",
  "ggplot2",
  "dplyr",
  "tidyr",
  "readr",
  "readxl",
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
message("==> repos em uso: ", paste(names(getOption("repos")), collapse = ", "))

if (length(missing) == 0) {
  message("==> tudo certo: nenhum pacote faltando.")
} else {
  message("==> pacotes faltando: ", paste(missing, collapse = ", "))
  install_pkgs(missing)
}

# Dica pós-instalação: cmdstanr instalado ≠ CmdStan instalado.
# CmdStan precisa ser instalado uma vez (toolchain) e fica em cache.
if (requireNamespace("cmdstanr", quietly = TRUE)) {
  ver <- tryCatch(as.character(cmdstanr::cmdstan_version()), error = function(e) NA_character_)
  if (is.na(ver) || ver == "") {
    message("==> cmdstanr OK, mas CmdStan NÃO encontrado.")
    message("    Rode: Rscript scripts/_setup/install_cmdstan.R")
  } else {
    message("==> CmdStan encontrado: ", ver)
  }
}

message("OK.")
