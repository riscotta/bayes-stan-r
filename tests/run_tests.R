#!/usr/bin/env Rscript

# Runner leve para os testes estruturais deste repositório.
# Uso:
#   Rscript tests/run_tests.R
#
# O projeto não é um pacote R. Os testes usam caminhos relativos à raiz,
# portanto cada arquivo é carregado sem alterar o diretório de trabalho.

test_dir <- file.path("tests", "testthat")

if (!dir.exists(test_dir)) {
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

test_files <- list.files(
  test_dir,
  pattern = "^test.*\\.R$",
  full.names = TRUE
)

if (length(test_files) == 0) {
  message("Nenhum teste encontrado em tests/testthat/. (ok)")
  quit(status = 0)
}

message("Rodando ", length(test_files), " arquivos de teste...")

for (path in sort(test_files)) {
  message(" - ", basename(path))
  test_env <- new.env(parent = globalenv())
  sys.source(path, envir = test_env, chdir = FALSE)
}

message("Todos os arquivos de teste foram executados com sucesso.")
