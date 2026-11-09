testthat::test_that("T04.1 E00 separa documento atual de E00 historico", {
  path <- file.path("reports", "pnadc_estudo_i", "e00_consolidado_T04_1.md")
  testthat::expect_true(file.exists(path))
  txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  for (term in c(
    "RECONSTRUCAO_EDITORIAL", "não é", "pergunta original",
    "Estudo aplicado", "PENDENTE_DE_VERIFICACAO",
    "não constitui", "Não há QA independente",
    "T01.3–T01.7", "Issue #14", "T04.2–T04.7",
    "não", "ABERTA", "NAO_DECIDIR"
  )) {
    testthat::expect_true(grepl(term, txt, fixed = TRUE), info = term)
  }
  testthat::expect_true(grepl(
    "O desemprego brasileiro caiu para 5,2%. Mas o mercado de trabalho melhorou em todas as dimensões?",
    txt, fixed = TRUE
  ))
})

testthat::test_that("T04.1 contem oito campos, janelas e seis decisoes abertas", {
  path <- file.path("reports", "pnadc_estudo_i", "e00_consolidado_T04_1.md")
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")

  fields <- c("Pergunta", "Objeto", "Dados", "Método", "Estado",
              "Resultados", "Limites", "Responsável")
  for (field in fields) {
    testthat::expect_identical(
      sum(grepl(paste0("| **", field, "** |"), lines, fixed = TRUE)),
      1L, info = field
    )
  }

  for (term in c("202509", "202606", "202609", "anual", "recente",
                 "julho–setembro", "abril–junho", "14 anos ou mais")) {
    testthat::expect_true(any(grepl(term, lines, fixed = TRUE)), info = term)
  }

  hypothesis <- grep("^\\| H0[1-3] \\|", lines, value = TRUE)
  testthat::expect_length(hypothesis, 3L)
  decisions <- grep("^\\| D00-0[1-6] \\|", lines, value = TRUE)
  testthat::expect_length(decisions, 6L)
  testthat::expect_true(all(grepl("| `ABERTA` |", decisions, fixed = TRUE)))
  testthat::expect_true(any(grepl("não são modelos mutuamente exclusivos",
                                 lines, fixed = TRUE)))
})

testthat::test_that("T04.1 eixos e indicadores batem com E09 descritiva", {
  doc <- readLines(file.path("reports", "pnadc_estudo_i",
                             "e00_consolidado_T04_1.md"),
                   warn = FALSE, encoding = "UTF-8")
  begin <- which(startsWith(doc, "## 3. Eixos, indicadores"))
  end <- which(startsWith(doc, "## 4. Hipóteses concorrentes"))
  testthat::expect_length(begin, 1L)
  testthat::expect_length(end, 1L)
  testthat::expect_gt(end, begin)
  block <- paste(doc[seq.int(begin + 1L, end - 1L)], collapse = "\n")

  csv_path <- file.path("data", "raw", "pnadc_estudo_i",
                        "resultados_auditados", "contrastes_descritos_e09.csv")
  testthat::expect_true(file.exists(csv_path))
  dat <- read.csv(csv_path, sep = ";", fileEncoding = "UTF-8",
                  colClasses = "character", check.names = FALSE)
  ids <- unique(dat$IndicadorId)

  testthat::expect_identical(nrow(dat), 26L)
  testthat::expect_identical(length(ids), 13L)
  testthat::expect_setequal(unique(dat$Eixo), LETTERS[1:5])
  testthat::expect_setequal(unique(dat$Contraste), c("anual", "recente"))
  testthat::expect_true(all(table(dat$IndicadorId) == 2L))
  testthat::expect_true(all(dat$PeriodoDestino == "202609"))
  testthat::expect_true(all(dat$PeriodoOrigem[dat$Contraste == "anual"] == "202509"))
  testthat::expect_true(all(dat$PeriodoOrigem[dat$Contraste == "recente"] == "202606"))
  testthat::expect_true(all(vapply(ids, function(id) {
    grepl(paste0("`", id, "`"), block, fixed = TRUE)
  }, logical(1))))
  testthat::expect_true(grepl("populacao_14mais_mil", block, fixed = TRUE))

  expected <- c(A = 2L, B = 4L, C = 4L, D = 2L, E = 1L)
  actual <- table(dat$Eixo[!duplicated(dat$IndicadorId)])
  testthat::expect_identical(unname(as.integer(actual)),
                             unname(as.integer(expected)))
  delta <- as.numeric(dat$ValorDestino) - as.numeric(dat$ValorOrigem)
  testthat::expect_equal(as.numeric(dat$Delta), delta, tolerance = 1e-6)
})

testthat::test_that("T04.1 registra fonte e nao promete inferencia conjunta", {
  doc <- paste(readLines(file.path("reports", "pnadc_estudo_i",
                                  "e00_consolidado_T04_1.md"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  refs <- c("STUDY_E00_TEMPLATE_T02_3.md",
            "STUDY_METADATA_CONTRACT_T02_2.md",
            "inventario_estimandos_e07_T03_1.md",
            "comparacao_alternativas_inferenciais_T03_5.md",
            "rotulos_inferenciais_T03_6.md",
            "nota_cientifica.md", "contrastes_descritos_e09.csv")
  for (ref in refs) testthat::expect_true(grepl(ref, doc, fixed = TRUE), info = ref)
  for (risk in c("não está identificado", "covariâncias", "arredondamento",
                 "marginal", "descritivo", "autoridade", "QA")) {
    testthat::expect_true(grepl(risk, doc, fixed = TRUE), info = risk)
  }
})