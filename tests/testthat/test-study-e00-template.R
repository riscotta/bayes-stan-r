testthat::test_that("T02.3 oferece secoes E00 com dez blocos e oito metadados", {
  path <- file.path("scripts", "STUDY_E00_TEMPLATE_T02_3.md")
  testthat::expect_true(file.exists(path))
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")

  section_lines <- grep("^## [0-9]+\\.", lines, value = TRUE)
  section_numbers <- as.integer(sub("^## ([0-9]+)\\..*$", "\\1", section_lines))
  testthat::expect_identical(section_numbers, 1:10)

  start <- which(startsWith(lines, "## 1. Identificação"))
  finish <- which(startsWith(lines, "## 2. Pergunta"))
  testthat::expect_length(start, 1L)
  testthat::expect_length(finish, 1L)
  testthat::expect_gt(finish, start)

  block <- lines[seq.int(start + 1L, finish - 1L)]
  fields <- grep("^\\| \\*\\*[^*]+\\*\\* \\|", block, value = TRUE)
  fields <- sub("^\\| \\*\\*([^*]+)\\*\\* \\|.*$", "\\1", fields)
  testthat::expect_identical(
    fields,
    c("Pergunta", "Objeto", "Dados", "Método", "Estado",
      "Resultados", "Limites", "Responsável")
  )
  testthat::expect_true(all(grepl("PENDENTE_DE_VERIFICACAO", block[grepl("^\\| \\*\\*", block)], fixed = TRUE)))
})

testthat::test_that("T02.3 registra hipoteses concorrentes e decisoes verificaveis", {
  lines <- readLines(
    file.path("scripts", "STUDY_E00_TEMPLATE_T02_3.md"),
    warn = FALSE, encoding = "UTF-8"
  )
  txt <- paste(lines, collapse = "\n")
  testthat::expect_true(all(c("H01", "H02", "H03") %in% sub("^\\| (H0[0-9]) \\|.*", "\\1",
    grep("^\\| H0[0-9] \\|", lines, value = TRUE))))
  testthat::expect_true(all(c("D00-01", "D00-02", "D00-03", "D00-04") %in%
    sub("^\\| (D00-[0-9]+) \\|.*", "\\1", grep("^\\| D00-", lines, value = TRUE))))
  testthat::expect_true(all(c("R00-01", "R00-02", "R00-03") %in%
    sub("^\\| (R00-[0-9]+) \\|.*", "\\1", grep("^\\| R00-", lines, value = TRUE))))
  testthat::expect_true(all(c("P00-01", "P00-02", "P00-03") %in%
    sub("^\\| (P00-[0-9]+) \\|.*", "\\1", grep("^\\| P00-", lines, value = TRUE))))

  for (required in c(
    "Critério verificável de fechamento", "Responsável e prova de atribuição",
    "ABERTA", "FECHADA", "REABERTA", "NAO_DECIDIR",
    "INDISPONIVEL_NO_HISTORICO", "NAO_APLICAVEL", "RECONSTRUCAO_EDITORIAL",
    "Laboratório de modelos", "Estudo aplicado", "sensibilidade",
    "## 8. Índice de evidências", "## 9. Registro de pendências"
  )) {
    testthat::expect_true(grepl(required, txt, fixed = TRUE), info = required)
  }
})

testthat::test_that("T02.3 preserva fronteiras de escopo e vinculos editoriais", {
  e00 <- readLines(
    file.path("scripts", "STUDY_E00_TEMPLATE_T02_3.md"),
    warn = FALSE, encoding = "UTF-8"
  )
  catalog <- readLines(file.path("scripts", "README.md"), warn = FALSE, encoding = "UTF-8")
  txt <- paste(e00, collapse = "\n")

  testthat::expect_true(file.exists(file.path("scripts", "STUDY_METADATA_CONTRACT_T02_2.md")))
  testthat::expect_true(file.exists(file.path("scripts", "STUDY_TAXONOMY_T02_1.md")))
  testthat::expect_true(any(grepl("STUDY_E00_TEMPLATE_T02_3.md", catalog, fixed = TRUE)))
  testthat::expect_true(grepl("](STUDY_METADATA_CONTRACT_T02_2.md)", txt, fixed = TRUE))
  testthat::expect_true(grepl("](STUDY_TAXONOMY_T02_1.md)", txt, fixed = TRUE))
  testthat::expect_true(grepl("T02.4, T02.5, T02.6 e T02.7", txt, fixed = TRUE))
  testthat::expect_true(grepl("um **modelo vazio e verificável**", txt, fixed = TRUE))
})