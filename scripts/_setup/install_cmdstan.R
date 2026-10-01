#!/usr/bin/env Rscript

# Instala o CmdStan para uso com cmdstanr.
#
# Versao canonica do repositorio: CmdStan 2.40.0
#
# Uso:
#   Rscript scripts/_setup/install_cmdstan.R
#   Rscript scripts/_setup/install_cmdstan.R --version=2.40.0 --cores=4
#
# Uma versao diferente pode ser informada explicitamente com --version,
# mas deixa de representar o ambiente de referencia do repositorio.

args <- commandArgs(trailingOnly = TRUE)

get_arg_value <- function(prefix, default = NULL) {
  hit <- grep(paste0("^", prefix, "="), args, value = TRUE)
  if (length(hit) == 0) return(default)
  sub(paste0("^", prefix, "="), "", hit[[1]])
}

reference_version <- "2.40.0"

version <- get_arg_value("--version", default = reference_version)
cores   <- as.integer(get_arg_value("--cores", default = "2"))
dir     <- get_arg_value("--dir", default = NULL)

if (!requireNamespace("cmdstanr", quietly = TRUE)) {
  stop(
    "Pacote 'cmdstanr' nao esta instalado.\n",
    "Restaure o ambiente com renv::restore() ou rode: ",
    "Rscript scripts/_setup/install_deps.R --all\n",
    call. = FALSE
  )
}

try(cmdstanr::check_cmdstan_toolchain(fix = TRUE, quiet = FALSE), silent = TRUE)

cmdstanr::install_cmdstan(
  dir = if (is.null(dir) || dir == "") NULL else dir,
  version = version,
  cores = if (is.na(cores) || cores < 1) 2L else cores
)

installed_version <- tryCatch(
  as.character(cmdstanr::cmdstan_version()),
  error = function(e) NA_character_
)

if (is.na(installed_version) || installed_version == "") {
  stop("CmdStan foi instalado, mas sua versao nao pode ser confirmada.", call. = FALSE)
}

if (!identical(installed_version, version)) {
  stop(
    "Versao do CmdStan diferente da solicitada.\n",
    "Solicitada: ", version, "\n",
    "Detectada:  ", installed_version,
    call. = FALSE
  )
}

message("cmdstan_path(): ", cmdstanr::cmdstan_path())
message("cmdstan_version(): ", installed_version)

if (!identical(version, reference_version)) {
  message(
    "ATENCAO: a versao instalada difere da referencia do repositorio (",
    reference_version, ")."
  )
}

message("OK.")
