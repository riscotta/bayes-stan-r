#!/usr/bin/env Rscript

# Inicializa e congela o ambiente R do repositorio com renv.
#
# Uso recomendado, a partir da raiz:
#   Rscript scripts/_setup/init_renv.R
#
# O lockfile oficial usa:
# - R 4.5.1
# - pacotes gerais resolvidos no CRAN estavel
# - cmdstanr 0.9.0 resolvido no repositorio oficial Stan r-universe
#
# A arvore e resolvida sem instalar todos os pacotes. A instalacao efetiva
# da biblioteca do projeto e feita depois com renv::restore().

args <- commandArgs(trailingOnly = TRUE)
flag_minimal <- "--minimal" %in% args
flag_allow_r_mismatch <- "--allow-r-version-mismatch" %in% args

project_markers <- c("README.md", file.path("scripts", "_setup", "install_deps.R"))
if (!all(file.exists(project_markers))) {
  stop("Execute este script a partir da raiz do repositorio.", call. = FALSE)
}

required_r <- "4.5.1"
required_cmdstanr <- "0.9.0"
cran_repo <- "https://cloud.r-project.org"
stan_repo <- "https://stan-dev.r-universe.dev"

current_r <- as.character(getRversion())
if (!identical(current_r, required_r) && !flag_allow_r_mismatch) {
  stop(
    "Versao de R diferente da referencia do projeto.\n",
    "Esperada: R ", required_r, "\n",
    "Atual:    R ", current_r, "\n\n",
    "Instale/use R ", required_r,
    " ou, se a diferenca for intencional, rode novamente com ",
    "--allow-r-version-mismatch.",
    call. = FALSE
  )
}

# O plano principal usa apenas CRAN para impedir que pacotes que tambem
# existem no Stan r-universe sejam substituidos por builds de desenvolvimento.
options(repos = c(CRAN = cran_repo))

if (!requireNamespace("renv", quietly = TRUE)) {
  message("==> instalando renv")
  install.packages("renv", repos = cran_repo)
}

if (utils::packageVersion("renv") < "1.2.0") {
  stop("Este setup requer renv >= 1.2.0, pois usa renv::plan().", call. = FALSE)
}

source(file.path("scripts", "_setup", "dependencies.R"), local = TRUE)
pkgs <- if (flag_minimal) pkgs_min else pkgs_all
pkgs_cran <- setdiff(pkgs, "cmdstanr")

message("==> R de referencia: ", required_r)
message("==> conjunto de dependencias: ", if (flag_minimal) "MINIMAL" else "ALL")
message("==> CRAN: ", cran_repo)
message("==> cmdstanr: ", required_cmdstanr, " via ", stan_repo)

if (!file.exists("renv/activate.R")) {
  message("==> criando infraestrutura renv")
  renv::scaffold(
    project = ".",
    repos = c(CRAN = cran_repo),
    settings = list(snapshot.type = "all", r.version = required_r)
  )
}

renv::settings$snapshot.type("all", project = ".")
renv::settings$r.version(required_r, project = ".")

message("==> resolvendo dependencias CRAN")
renv::plan(
  packages = pkgs_cran,
  project = "."
)

# O cmdstanr nao e resolvido junto com o restante porque o Stan r-universe
# tambem publica builds de desenvolvimento de outros pacotes do ecossistema.
# Primeiro adicionamos os dois repositorios ao lockfile e depois registramos
# exclusivamente o cmdstanr na versao auditada.
lock <- renv::lockfile_read(file = "renv.lock", project = ".")
lock <- renv::lockfile_modify(
  lockfile = lock,
  repos = c(CRAN = cran_repo, stan = stan_repo),
  project = "."
)
renv::lockfile_write(lockfile = lock, file = "renv.lock", project = ".")

options(repos = c(CRAN = cran_repo, stan = stan_repo))
message("==> registrando cmdstanr ", required_cmdstanr)
renv::record(
  paste0("cmdstanr@", required_cmdstanr),
  project = "."
)


if (!file.exists("renv.lock") || file.info("renv.lock")$size <= 0) {
  stop("renv.lock nao foi gerado corretamente.", call. = FALSE)
}

message("")
message("OK.")
message("O ambiente foi congelado sem instalar todos os pacotes.")
message("Para materializar a biblioteca do projeto:")
message("  Rscript -e \"renv::restore(prompt = FALSE)\"")
