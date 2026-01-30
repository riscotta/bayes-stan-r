#!/usr/bin/env Rscript

# Instala o CmdStan para uso com cmdstanr.
# Uso:
#   Rscript scripts/_setup/install_cmdstan.R
#   Rscript scripts/_setup/install_cmdstan.R --version=2.35.0 --cores=4

args <- commandArgs(trailingOnly = TRUE)

get_arg_value <- function(prefix, default = NULL) {
  hit <- grep(paste0("^", prefix, "="), args, value = TRUE)
  if (length(hit) == 0) return(default)
  sub(paste0("^", prefix, "="), "", hit[[1]])
}

version <- get_arg_value("--version", default = NULL)
cores   <- as.integer(get_arg_value("--cores", default = "2"))
dir     <- get_arg_value("--dir", default = NULL)

if (!requireNamespace("cmdstanr", quietly = TRUE)) {
  stop(
    "Pacote 'cmdstanr' não está instalado.\n",
    "Rode primeiro: Rscript scripts/_setup/install_deps.R --all\n",
    call. = FALSE
  )
}

# tenta ajudar com toolchain quando aplicável
try(cmdstanr::check_cmdstan_toolchain(fix = TRUE, quiet = FALSE), silent = TRUE)

cmdstanr::install_cmdstan(
  dir = if (is.null(dir) || dir == "") NULL else dir,
  version = if (is.null(version) || version == "") NULL else version,
  cores = if (is.na(cores) || cores < 1) 2L else cores
)

message("cmdstan_path(): ", tryCatch(cmdstanr::cmdstan_path(), error = function(e) "NA"))
message("cmdstan_version(): ", tryCatch(cmdstanr::cmdstan_version(), error = function(e) "NA"))
message("OK.")
