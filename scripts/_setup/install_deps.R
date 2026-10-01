#!/usr/bin/env Rscript

# Instala dependencias R do repositorio.
# Uso:
#   Rscript scripts/_setup/install_deps.R
#   Rscript scripts/_setup/install_deps.R --all
#   Rscript scripts/_setup/install_deps.R --no-pak

args <- commandArgs(trailingOnly = TRUE)
flag_all    <- "--all" %in% args
flag_no_pak <- "--no-pak" %in% args

deps_file <- file.path("scripts", "_setup", "dependencies.R")
if (!file.exists(deps_file)) {
  stop(
    "Arquivo de dependencias nao encontrado. Execute este script a partir da raiz do repositorio.",
    call. = FALSE
  )
}

source(deps_file, local = TRUE)

cran_repo <- "https://cloud.r-project.org"
stan_repo <- "https://stan-dev.r-universe.dev"
options(repos = c(CRAN = cran_repo))

use_pak <- (!flag_no_pak) && requireNamespace("pak", quietly = TRUE)

install_cran_pkgs <- function(pkgs) {
  pkgs <- unique(pkgs)
  if (length(pkgs) == 0) return(invisible(NULL))

  if (use_pak) {
    pak::pkg_install(pkgs)
  } else {
    install.packages(pkgs, repos = cran_repo)
  }
}

pkgs <- if (flag_all) pkgs_all else pkgs_min
pkgs_cran <- setdiff(pkgs, "cmdstanr")

installed <- rownames(installed.packages())
missing_cran <- pkgs_cran[!pkgs_cran %in% installed]

message("==> instalando dependencias R (", if (flag_all) "ALL" else "MINIMAL", ")")
if (!flag_all) {
  message("==> dica: use --all para testes, relatorios e o exemplo INHALER via brms::inhaler.")
}
message("==> CRAN: ", cran_repo)

if (length(missing_cran) == 0) {
  message("==> dependencias CRAN ja instaladas.")
} else {
  message("==> pacotes CRAN faltando: ", paste(missing_cran, collapse = ", "))
  install_cran_pkgs(missing_cran)
}

if (!requireNamespace("cmdstanr", quietly = TRUE)) {
  message("==> instalando cmdstanr pelo Stan r-universe")
  install.packages(
    "cmdstanr",
    repos = c(stan = stan_repo, CRAN = cran_repo)
  )
}

# cmdstanr instalado != CmdStan instalado.
if (requireNamespace("cmdstanr", quietly = TRUE)) {
  ver <- tryCatch(as.character(cmdstanr::cmdstan_version()), error = function(e) NA_character_)
  if (is.na(ver) || ver == "") {
    message("==> cmdstanr OK, mas CmdStan NAO encontrado.")
    message("    Rode: Rscript scripts/_setup/install_cmdstan.R")
  } else {
    message("==> CmdStan encontrado: ", ver)
  }
}

message("OK.")
