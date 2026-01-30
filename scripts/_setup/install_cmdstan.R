#!/usr/bin/env Rscript

# scripts/_setup/install_cmdstan.R
# Instala o CmdStan para uso com cmdstanr.
#
# Uso:
#   Rscript scripts/_setup/install_cmdstan.R
#   Rscript scripts/_setup/install_cmdstan.R --version=2.35.0
#   Rscript scripts/_setup/install_cmdstan.R --cores=4
#   Rscript scripts/_setup/install_cmdstan.R --dir=/caminho/para/cmdstan
#
# Observação:
# - Por padrão, o cmdstanr instala em um diretório padrão do usuário (~/.cmdstan).
# - Este script não coloca CmdStan dentro do repositório (pra não sujar o git).

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
    "Rode primeiro: Rscript scripts/_setup/install_deps.R\n",
    call. = FALSE
  )
}

message("==> preparando toolchain (compiladores) ...")
try({
  # Em alguns sistemas isso pode orientar/validar setup do compilador
  cmdstanr::check_cmdstan_toolchain(fix = TRUE, quiet = FALSE)
}, silent = TRUE)

message("==> instalando CmdStan ...")
message("    version: ", if (is.null(version) || version == "") "latest" else version)
message("    cores:   ", cores)
message("    dir:     ", if (is.null(dir) || dir == "") "(default)" else dir)

# install_cmdstan aceita version=NULL para "latest"
cmdstanr::install_cmdstan(
  dir = if (is.null(dir) || dir == "") NULL else dir,
  version = if (is.null(version) || version == "") NULL else version,
  cores = if (is.na(cores) || cores < 1) 2L else cores
)

# Verificação
message("\n==> verificação:")
path <- tryCatch(cmdstanr::cmdstan_path(), error = function(e) NA_character_)
ver  <- tryCatch(cmdstanr::cmdstan_version(), error = function(e) NA_character_)

message("  cmdstan_path():    ", path)
message("  cmdstan_version(): ", ver)

message("\nOK. CmdStan instalado e reconhecido pelo cmdstanr.")
