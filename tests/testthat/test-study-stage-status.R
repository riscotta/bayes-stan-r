testthat::test_that("T02.6 define cinco estados fechados sem validar estudos", {
  path <- file.path("scripts", "STUDY_STAGE_STATUS_T02_6.md")
  testthat::expect_true(file.exists(path))
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  txt <- paste(lines, collapse = "\n")
  codes <- c("APROVADO", "RESSALVA", "PENDENTE", "NAO_APLICAVEL",
             "INDISPONIVEL_NO_HISTORICO")
  dictionary <- grep("^\\| \\x60[A-Z_]+\\x60 \\|", lines, value = TRUE)
  found <- sub("^\\| \\x60([A-Z_]+)\\x60 \\|.*$", "\\1", dictionary)
  testthat::expect_identical(found, codes)
  for (term in c("parecer independente", "SHA exato", "CI verde",
                 "busca documentada", "reconstruir", "dados",
                 "validade científica", "não conta como", "RESSALVA")) {
    testthat::expect_true(grepl(term, txt, fixed = TRUE), info = term)
  }
})

testthat::test_that("T02.6 exige critérios e modelo com E00 a E10 sem estado aprovado fictício", {
  lines <- readLines(file.path("scripts", "STUDY_STAGE_STATUS_T02_6.md"),
                     warn = FALSE, encoding = "UTF-8")
  headers <- grep("^## [0-9]+\\.", lines, value = TRUE)
  nums <- as.integer(sub("^## ([0-9]+)\\..*$", "\\1", headers))
  testthat::expect_identical(nums, 1:7)

  section3 <- match("## 3. Critérios de evidência específicos por etapa", lines)
  section4 <- match("## 4. Registro de evidências, revisão e transições", lines)
  criteria <- lines[seq.int(section3 + 1L, section4 - 1L)]
  crows <- grep("^\\| E[0-9]{2} \\|", criteria, value = TRUE)
  testthat::expect_identical(
    sub("^\\| (E[0-9]{2}) \\|.*$", "\\1", crows),
    sprintf("E%02d", 0:10)
  )

  section5 <- match("## 5. Modelo copiável — todos os estados iniciais são pendentes", lines)
  section6 <- match("## 6. Tratamento de legado e exemplos condicionais (não avaliados)", lines)
  templ <- lines[seq.int(section5 + 1L, section6 - 1L)]
  rows <- grep("^\\| E[0-9]{2} \\|", templ, value = TRUE)
  testthat::expect_identical(
    sub("^\\| (E[0-9]{2}) \\|.*$", "\\1", rows),
    sprintf("E%02d", 0:10)
  )
  for (row in rows) {
    testthat::expect_true(grepl("| \\x60PENDENTE\\x60 |", row, fixed = TRUE))
    testthat::expect_true(grepl("PENDENTE_DE_VERIFICACAO", row, fixed = TRUE))
    testthat::expect_false(grepl("\\x60APROVADO\\x60", row, fixed = TRUE))
    testthat::expect_length(strsplit(row, "|", fixed = TRUE)[[1]], 8L)
  }
})

testthat::test_that("T02.6 preserva estados locais, transicoes e fronteiras de escopo", {
  path <- file.path("scripts", "STUDY_STAGE_STATUS_T02_6.md")
  txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  catalog <- readLines(file.path("scripts", "README.md"),
                       warn = FALSE, encoding = "UTF-8")
  for (term in c(
    "ABERTA", "CONFORME_CODIGO", "CONFORME_EXECUCAO", "DIVERGENCIA",
    "Exec./QA", "PENDENTE_DE_VERIFICACAO", "DOCUMENTACAO_ATUAL",
    "RECONSTRUCAO_EDITORIAL", "reabertura", "T02.7", "CONTRIBUTING.md",
    "Issue #14", "T03.*", "não é auditoria", "independente"
  )) {
    testthat::expect_true(grepl(term, txt, fixed = TRUE), info = term)
  }
  for (name in c(
    "STUDY_TAXONOMY_T02_1.md", "STUDY_METADATA_CONTRACT_T02_2.md",
    "STUDY_E00_TEMPLATE_T02_3.md", "STUDY_E06_E07_MATRIX_T02_4.md",
    "STUDY_E08_E09_MATRIX_T02_5.md", "README.md"
  )) {
    testthat::expect_true(file.exists(file.path("scripts", name)), info = name)
    testthat::expect_true(grepl(paste0("](", name, ")"), txt, fixed = TRUE))
  }
  testthat::expect_true(any(grepl("STUDY_STAGE_STATUS_T02_6.md",
                                 catalog, fixed = TRUE)))
})
