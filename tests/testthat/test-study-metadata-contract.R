testthat::test_that("contrato T02.2 define oito campos obrigatorios uma unica vez no modelo", {
  path <- file.path("scripts", "STUDY_METADATA_CONTRACT_T02_2.md")
  testthat::expect_true(file.exists(path))
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")

  begin <- which(lines == "## Modelo copiável (preenchimento obrigatório)")
  end <- which(lines == "## Exemplos documentais (não são homologação)")
  testthat::expect_length(begin, 1L)
  testthat::expect_length(end, 1L)
  testthat::expect_gt(end, begin)

  template <- lines[seq.int(begin + 1L, end - 1L)]
  field_lines <- grep("^\\*\\*[^*]+\\*\\*:", template, value = TRUE)
  fields <- sub("^\\*\\*([^*]+)\\*\\*:.*$", "\\1", field_lines)
  required <- c(
    "Pergunta", "Objeto", "Dados", "Método", "Estado",
    "Resultados", "Limites", "Responsável"
  )
  testthat::expect_identical(fields, required)
  testthat::expect_true(all(nzchar(sub("^.*?:\\s*", "", field_lines))))
  testthat::expect_true(all(grepl("PENDENTE_DE_VERIFICACAO", field_lines, fixed = TRUE)))
})

testthat::test_that("contrato preserva incerteza e separa as duas categorias", {
  lines <- readLines(
    file.path("scripts", "STUDY_METADATA_CONTRACT_T02_2.md"),
    warn = FALSE, encoding = "UTF-8"
  )
  all_text <- paste(lines, collapse = "\n")
  for (marker in c("PENDENTE_DE_VERIFICACAO", "INDISPONIVEL_NO_HISTORICO",
                   "NAO_APLICAVEL", "RECONSTRUCAO_EDITORIAL")) {
    testthat::expect_match(all_text, marker, fixed = TRUE)
  }
  testthat::expect_match(all_text, "### Exemplo A — Laboratório de modelos:", fixed = TRUE)
  testthat::expect_match(all_text, "### Exemplo B — Estudo aplicado:", fixed = TRUE)
  testthat::expect_match(all_text, "T02.7", fixed = TRUE)
  testthat::expect_match(all_text, "Issue #14", fixed = TRUE)
})

testthat::test_that("catalogo aponta para o contrato de metadados", {
  readme <- paste(readLines("scripts/README.md", warn = FALSE, encoding = "UTF-8"),
                  collapse = "\n")
  testthat::expect_match(
    readme, "STUDY_METADATA_CONTRACT_T02_2.md", fixed = TRUE
  )
})
