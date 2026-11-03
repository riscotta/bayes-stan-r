testthat::test_that("T02.5 tem sete secoes e matriz de cinco classes sem conclusoes preenchidas", {
  path <- file.path("scripts", "STUDY_E08_E09_MATRIX_T02_5.md")
  testthat::expect_true(file.exists(path))
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  headings <- grep("^## [0-9]+\\.", lines, value = TRUE)
  nums <- as.integer(sub("^## ([0-9]+)\\..*$", "\\1", headings))
  testthat::expect_identical(nums, 1:7)

  classes <- c("DESCRITIVA", "POSTERIOR_MARGINAL",
               "CONTRASTE_CONJUNTO", "CENARIO_CONDICIONAL",
               "INTERPRETACAO_OU_AUSENCIA")
  rows <- grep("^\\| A08-", lines, value = TRUE)
  testthat::expect_length(rows, length(classes))
  ids <- sub("^\\| (A08-[0-9]+) \\|.*$", "\\1", rows)
  testthat::expect_equal(anyDuplicated(ids), 0L)
  for (i in seq_along(classes)) {
    testthat::expect_true(
      startsWith(rows[[i]], paste0("| A08-", sprintf("%02d", i),
                                      " | ", classes[[i]], " — ")),
      info = classes[[i]]
    )
    testthat::expect_true(grepl("PENDENTE_DE_VERIFICACAO", rows[[i]], fixed = TRUE))
    testthat::expect_true(grepl("ABERTA", rows[[i]], fixed = TRUE))
    testthat::expect_equal(
      lengths(regmatches(rows[[i]], gregexpr("|", rows[[i]], fixed = TRUE))),
      11L, info = paste("Colunas da matriz:", classes[[i]])
    )
  }
})

testthat::test_that("T02.5 exige evidencia diagnostico sensibilidade e limite por afirmacao", {
  txt <- paste(readLines(file.path("scripts", "STUDY_E08_E09_MATRIX_T02_5.md"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  for (term in c(
    "afirmação", "E08", "E09", "evidência", "diagnóstico", "sensibilidade",
    "limite de validade", "estimando", "recorte", "localizador",
    "SHA", "PPC", "R-hat", "ESS", "ρ", "arredondamento",
    "CENARIO_CONDICIONAL", "POSTERIOR_MARGINAL",
    "CONTRASTE_CONJUNTO", "posterior conjunta",
    "RECONSTRUCAO_EDITORIAL", "INDISPONIVEL_NO_HISTORICO",
    "NAO_APLICAVEL", "PENDENTE_DE_VERIFICACAO"
  )) {
    testthat::expect_true(grepl(term, txt, fixed = TRUE), info = term)
  }
})

testthat::test_that("T02.5 registra divergencias e nao confunde CI com validade", {
  txt <- paste(readLines(file.path("scripts", "STUDY_E08_E09_MATRIX_T02_5.md"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  for (term in c(
    "D0 — SEM_EVIDENCIA", "D1 — RECONCILIADA", "D2 — INFERENCIAL",
    "D3 — NUMERICA_OU_VERSAO", "D4 — DIAGNOSTICO_OU_SENSIBILIDADE",
    "D5 — RESTRICAO", "CONFORME_DOCUMENTO", "CONFORME_EXECUCAO",
    "DIVERGENCIA", "não demonstra", "não prova", "T02.6", "T02.7",
    "T03.*", "Issue #14", "não uma auditoria científica"
  )) {
    testthat::expect_true(grepl(term, txt, fixed = TRUE), info = term)
  }
})

testthat::test_that("T02.5 usa estado local valido para restricao D5", {
  lines <- readLines(file.path("scripts", "STUDY_E08_E09_MATRIX_T02_5.md"),
                     warn = FALSE, encoding = "UTF-8")
  d5 <- grep("^\\| D5 — RESTRICAO \\|", lines, value = TRUE)
  testthat::expect_length(d5, 1L)
  testthat::expect_true(grepl("`ABERTA`", d5, fixed = TRUE))
  testthat::expect_true(grepl("`PENDENTE_DE_VERIFICACAO`", d5, fixed = TRUE))
  testthat::expect_true(grepl("`INDISPONIVEL_NO_HISTORICO`", d5, fixed = TRUE))
  testthat::expect_true(grepl("como **motivo**, não como estado local", d5, fixed = TRUE))
})

testthat::test_that("T02.5 tem links relativos e entrada no catalogo", {
  lines <- readLines(file.path("scripts", "STUDY_E08_E09_MATRIX_T02_5.md"),
                     warn = FALSE, encoding = "UTF-8")
  catalog <- readLines(file.path("scripts", "README.md"),
                       warn = FALSE, encoding = "UTF-8")
  txt <- paste(lines, collapse = "\n")
  testthat::expect_true(any(grepl("STUDY_E08_E09_MATRIX_T02_5.md",
                                 catalog, fixed = TRUE)))
  for (path in c(
    "STUDY_TAXONOMY_T02_1.md",
    "STUDY_METADATA_CONTRACT_T02_2.md",
    "STUDY_E00_TEMPLATE_T02_3.md",
    "STUDY_E06_E07_MATRIX_T02_4.md", "README.md"
  )) {
    testthat::expect_true(file.exists(file.path("scripts", path)), info = path)
    testthat::expect_true(grepl(paste0("](", path, ")"), txt, fixed = TRUE),
                          info = path)
  }
})
