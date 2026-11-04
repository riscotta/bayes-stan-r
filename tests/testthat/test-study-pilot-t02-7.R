testthat::test_that("T02.7 publica politica normativa e piloto aplicado sem homologacao", {
  policy <- readLines("CONTRIBUTING.md", warn = FALSE, encoding = "UTF-8")
  catalog <- readLines(file.path("scripts", "README.md"),
                       warn = FALSE, encoding = "UTF-8")
  txt <- paste(policy, collapse = "\n")
  for (term in c(
    "Contrato editorial obrigatório para estudos (T02.1–T02.7)",
    "Laboratório de modelos", "Estudo aplicado", "Pergunta", "Objeto",
    "Dados", "Método", "Estado", "Resultados", "Limites", "Responsável",
    "NAO_APLICAVEL", "INDISPONIVEL_NO_HISTORICO",
    "RECONSTRUCAO_EDITORIAL", "NAO_DECIDIR", "Issue #14",
    "QA independente"
  )) {
    testthat::expect_true(grepl(term, txt, fixed = TRUE), info = term)
  }
  for (file in c(
    "STUDY_TAXONOMY_T02_1.md", "STUDY_METADATA_CONTRACT_T02_2.md",
    "STUDY_E00_TEMPLATE_T02_3.md", "STUDY_E06_E07_MATRIX_T02_4.md",
    "STUDY_E08_E09_MATRIX_T02_5.md", "STUDY_STAGE_STATUS_T02_6.md",
    "STUDY_PILOT_T02_7_MORTALIDADE_SP.md"
  )) {
    testthat::expect_true(file.exists(file.path("scripts", file)), info = file)
    testthat::expect_true(grepl(file, txt, fixed = TRUE), info = file)
  }
  testthat::expect_true(any(grepl("STUDY_PILOT_T02_7_MORTALIDADE_SP.md",
                                 catalog, fixed = TRUE)))
})

testthat::test_that("T02.7 piloto tem oito campos, fontes e 11 estados nao aprovados", {
  path <- file.path("scripts", "STUDY_PILOT_T02_7_MORTALIDADE_SP.md")
  testthat::expect_true(file.exists(path))
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  text <- paste(lines, collapse = "\n")
  testthat::expect_true(any(startsWith(lines, "## 1. Metadados obrigatórios")))
  testthat::expect_true(any(startsWith(lines, "## 5. Estados E00–E10")))
  expected <- c("Pergunta", "Objeto", "Dados", "Método", "Estado",
                "Resultados", "Limites", "Responsável")
  field_lines <- grep("^\\| (Pergunta|Objeto|Dados|Método|Estado|Resultados|Limites|Responsável) \\|",
                      lines, value = TRUE)
  field_names <- sub("^\\| ([^| ]+) \\|.*$", "\\1", field_lines)
  testthat::expect_identical(field_names, expected)
  testthat::expect_true(all(nchar(field_lines) > 40L))
  testthat::expect_true(all(grepl("](", field_lines, fixed = TRUE) |
                            grepl("PENDENTE_DE_VERIFICACAO", field_lines,
                                  fixed = TRUE)))

  from <- which(startsWith(lines, "## 5. Estados E00–E10"))
  to <- which(startsWith(lines, "## 6. Resultado do piloto"))
  testthat::expect_length(from, 1L)
  testthat::expect_length(to, 1L)
  rows <- grep("^\\| E(0[0-9]|10) \\|", lines[seq.int(from + 1L, to - 1L)],
               value = TRUE)
  actual <- sub("^\\| (E[0-9]{2}) \\|.*$", "\\1", rows)
  testthat::expect_identical(actual, sprintf("E%02d", 0:10))
  for (row in rows) {
    testthat::expect_true(grepl("| `PENDENTE` |", row, fixed = TRUE))
    testthat::expect_false(grepl("| `APROVADO` |", row, fixed = TRUE))
    testthat::expect_false(grepl("| `RESSALVA` |", row, fixed = TRUE))
  }
  for (term in c(
    "RECONSTRUCAO_EDITORIAL", "Não houve", "ABERTA", "H01", "H02", "H03",
    "M06-P01", "M06-P02", "M06-P03", "A08-P01",
    "D4", "não é excesso causal", "sem equivalência certificada",
    "Issue", "T03.*"
  )) {
    testthat::expect_true(grepl(term, text, fixed = TRUE), info = term)
  }
})

testthat::test_that("T02.7 piloto identifica fontes por caminho e nao muda resultados", {
  paths <- c(
    file.path("scripts", "mortalidade_sp_0_74", "README.md"),
    file.path("scripts", "mortalidade_sp_0_74", "01_E07_ajustar_modelos.R"),
    file.path("scripts", "mortalidade_sp_0_74", "02_E08_diagnosticar.R"),
    file.path("reports", "mortalidade_sp_0_74", "caderno_E01_E10.md"),
    file.path("reports", "mortalidade_sp_0_74", "matriz_evidencias.md"),
    file.path("reports", "mortalidade_sp_0_74", "relatorio_cientifico.md")
  )
  testthat::expect_true(all(file.exists(paths)))
  pilot <- paste(readLines(file.path("scripts",
    "STUDY_PILOT_T02_7_MORTALIDADE_SP.md"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n")
  for (p in basename(paths)) {
    testthat::expect_true(grepl(p, pilot, fixed = TRUE), info = p)
  }
  testthat::expect_true(grepl("Rscript indisponível", pilot, fixed = TRUE))
  testthat::expect_true(grepl("QA da T02.7 depende de parecer técnico separado", pilot, fixed = TRUE))
})
