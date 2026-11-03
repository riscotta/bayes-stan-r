testthat::test_that("T02.4 possui matriz editavel com sete classes e IDs unicos", {
  path <- file.path("scripts", "STUDY_E06_E07_MATRIX_T02_4.md")
  testthat::expect_true(file.exists(path))
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")

  headings <- lines[startsWith(lines, "## ")]
  expected <- paste0("## ", 1:7, ".")
  testthat::expect_length(headings, 7L)
  testthat::expect_true(all(startsWith(headings, expected)))

  classes <- c(
    "EQUACAO", "DADO_INDICE", "PARAMETRO", "PRIOR",
    "LIKELIHOOD", "TRANSFORMACAO_RESTRICAO", "GENERATED_QUANTITY"
  )
  rows <- lines[startsWith(lines, "| M06-")]
  testthat::expect_length(rows, length(classes))
  for (i in seq_along(classes)) {
    prefix <- paste0("| M06-", sprintf("%02d", i), " | ", classes[[i]], " |")
    testthat::expect_true(startsWith(rows[[i]], prefix), info = prefix)
    testthat::expect_true(grepl("PENDENTE_DE_VERIFICACAO", rows[[i]], fixed = TRUE))
    testthat::expect_true(grepl("ABERTA", rows[[i]], fixed = TRUE))
    testthat::expect_equal(
      lengths(regmatches(rows[[i]], gregexpr("|", rows[[i]], fixed = TRUE))),
      9L,
      info = paste("Numero de separadores:", classes[[i]])
    )
  }
  ids <- sub(" .*", "", substring(rows, 3L))
  testthat::expect_equal(anyDuplicated(ids), 0L)
})

testthat::test_that("T02.4 documenta campos obrigatorios e confronto inferencial", {
  lines <- readLines(
    file.path("scripts", "STUDY_E06_E07_MATRIX_T02_4.md"),
    warn = FALSE, encoding = "UTF-8"
  )
  txt <- paste(lines, collapse = " ")
  required <- c(
    "equações", "parâmetros", "priors", "likelihood",
    "generated quantities", "dimensões", "suporte",
    "Jacobian", "E06→E07", "Stan", "R:",
    "linhas", "SHA", "indexação", "transformações",
    "cada", "uma linha por correspondência",
    "RECONSTRUCAO_EDITORIAL", "INDISPONIVEL_NO_HISTORICO",
    "PENDENTE_DE_VERIFICACAO", "NAO_APLICAVEL"
  )
  # "Jacobian" pode ser descrito em portugues como Jacobiano.
  required[required == "Jacobian"] <- "Jacobiano"
  for (term in required) {
    testthat::expect_true(grepl(term, txt, fixed = TRUE), info = term)
  }
})

testthat::test_that("T02.4 controla divergencias, execucao e escopo", {
  txt <- paste(
    readLines(file.path("scripts", "STUDY_E06_E07_MATRIX_T02_4.md"),
              warn = FALSE, encoding = "UTF-8"),
    collapse = " "
  )
  for (term in c(
    "D0 — ABERTA", "D1 — DOCUMENTAL", "D2 — SUBSTANTIVA",
    "D3 — EXECUCAO", "D4 — INDISPONIBILIDADE",
    "CONFORME_CODIGO", "CONFORME_EXECUCAO", "DIVERGENCIA",
    "não implica", "sem alegação de PPC", "T02.5", "T02.6",
    "T02.7", "Issue #14", "não uma inspeção"
  )) {
    testthat::expect_true(grepl(term, txt, fixed = TRUE), info = term)
  }
})

testthat::test_that("T02.4 tem referencias e link de descoberta", {
  catalog <- readLines(file.path("scripts", "README.md"),
                       warn = FALSE, encoding = "UTF-8")
  matrix <- readLines(file.path("scripts", "STUDY_E06_E07_MATRIX_T02_4.md"),
                      warn = FALSE, encoding = "UTF-8")
  text <- paste(matrix, collapse = " ")
  testthat::expect_true(any(grepl("STUDY_E06_E07_MATRIX_T02_4.md",
                                 catalog, fixed = TRUE)))
  for (link in c(
    "STUDY_METADATA_CONTRACT_T02_2.md",
    "STUDY_E00_TEMPLATE_T02_3.md",
    "STUDY_TAXONOMY_T02_1.md",
    "README.md"
  )) {
    testthat::expect_true(file.exists(file.path("scripts", link)), info = link)
    testthat::expect_true(grepl(link, text, fixed = TRUE), info = link)
  }
})
