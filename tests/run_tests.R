#!/usr/bin/env Rscript

# Runner simples para testthat (opcional).
# Uso:
#   Rscript tests/run_tests.R

if (!dir.exists(file.path("tests", "testthat"))) {
  message("Pasta tests/testthat não existe. Nada para rodar.")
  quit(status = 0)
}

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop(
    "Pacote 'testthat' não está instalado.\n",
    "Rode: Rscript scripts/_setup/install_deps.R --all\n",
    call. = FALSE
  )
}

test_files <- list.files(file.path("tests", "testthat"), pattern = "\\.R$", full.names = TRUE)
if (length(test_files) == 0) {
  message("Nenhum teste encontrado em tests/testthat/. (ok)")
  quit(status = 0)
}

message("Rodando testthat...")
testthat::test_dir(file.path("tests", "testthat"))
