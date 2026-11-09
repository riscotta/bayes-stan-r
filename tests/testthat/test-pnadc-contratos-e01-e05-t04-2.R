testthat::test_that("T04.2 documenta E01-E05 sem simular execucoes historicas", {
  path <- file.path("reports", "pnadc_estudo_i", "contratos_e01_e05_T04_2.md")
  testthat::expect_true(file.exists(path))
  txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  for (token in c(
    "RECONSTRUCAO_EDITORIAL", "NAO_DISPONIVEL_NO_GIT",
    "FONTE_HISTORICA_VERSIONADA", "NAO_REPRODUZIDA",
    "DECLARADO_NO_CODIGO", "PENDENTE_INSTITUCIONAL",
    "E01", "E02", "E03", "E04", "E05",
    "22.130", "2.405", "175", "201203", "202609",
    "201510", "201511", "201512",
    "T01.3–T01.7", "Issue #14", "T04.3"
  )) {
    testthat::expect_true(grepl(token, txt, fixed = TRUE), info = token)
  }
  for (ref in c(
    "e00_consolidado_T04_1.md",
    "construir_base_e04.py", "explorar_e05.py",
    "DATA_PROVENANCE.csv", "contrastes_descritos_e09.csv"
  )) testthat::expect_true(grepl(ref, txt, fixed = TRUE), info = ref)
})

testthat::test_that("T04.2 dicionario corresponde a 14 especificacoes historicas E04", {
  script <- readLines(file.path("scripts", "pnadc_estudo_i", "estudo_I",
                                "e04", "construir_base_e04.py"),
                      warn = FALSE, encoding = "UTF-8")
  doc <- readLines(file.path("reports", "pnadc_estudo_i",
                             "contratos_e01_e05_T04_2.md"),
                   warn = FALSE, encoding = "UTF-8")
  ini <- which(startsWith(script, "ESPECIFICACOES = ["))
  fim <- which(trimws(script) == "]")
  testthat::expect_length(ini, 1L)
  testthat::expect_true(any(fim > ini))
  bloco <- script[seq.int(ini + 1L, min(fim[fim > ini]) - 1L)]
  tuplas <- bloco[startsWith(trimws(bloco), "('")]
  testthat::expect_length(tuplas, 14L)
  testthat::expect_identical(sum(grepl("'principal'", tuplas, fixed = TRUE)), 13L)
  testthat::expect_identical(sum(grepl("'apoio'", tuplas, fixed = TRUE)), 1L)
  esperado <- c(
    "desocupacao_pct", "desocupados_mil",
    "nivel_ocupacao_pct", "participacao_pct",
    "ocupados_mil", "forca_trabalho_mil", "populacao_14mais_mil",
    "subutilizacao_pct", "subocupacao_horas_pct",
    "desalentados_mil", "desalento_pct",
    "informalidade_pct", "contrib_previdencia_pct",
    "rendimento_real_habitual_rs"
  )
  for (id in esperado) {
    testthat::expect_identical(sum(grepl(paste0("('", id, "',"), tuplas, fixed = TRUE)),
                               1L, info = paste("fonte", id))
    testthat::expect_identical(sum(startsWith(doc, paste0("| `", id, "` |"))),
                               1L, info = paste("dicionario", id))
  }
  testthat::expect_false(any(grepl("^\\| `populacao_14mais_mil` \\| [ACDE] / principal",
                                  doc)))
  testthat::expect_true(any(grepl("A=2, B=4, C=4, D=2, E=1 = 13",
                                 doc, fixed = TRUE)))
})

testthat::test_that("T04.2 registra contratos existentes sem executar E04 ou E05", {
  e04 <- paste(readLines(file.path("scripts", "pnadc_estudo_i", "estudo_I",
                                    "e04", "construir_base_e04.py"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  e05 <- paste(readLines(file.path("scripts", "pnadc_estudo_i", "estudo_I",
                                    "e05", "explorar_e05.py"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  doc <- paste(readLines(file.path("reports", "pnadc_estudo_i",
                                  "contratos_e01_e05_T04_2.md"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  for (token in c(
    "len(origem) == 22130", "len(periodos) == 175",
    "len(dados)", "NAO_DIVULGADO_OFICIAL", "CVPercentual",
    "RegimeConceitual2015", "'201510':2, '201511':2",
    "informalidade_pct", "contrib_previdencia_pct",
    "base_analitica_e04_longa.csv", "base_analitica_e04_larga.csv",
    "resumo_cobertura_e04.csv", "auditoria_e04.json"
  )) testthat::expect_true(grepl(token, e04, fixed = TRUE), info = token)
  for (token in c(
    "long.shape == (2405,31)", "wide.shape == (175,31)",
    "len(changes) == 14", "len(stats) == 16",
    "MANIFESTO_SHA256.csv", "contrastes_e05.csv",
    "distribuicoes_temporais_e05.csv", "auditoria_e05.json",
    "202509", "202606", "202609"
  )) testthat::expect_true(grepl(token, e05, fixed = TRUE), info = token)
  for (token in c(
    "não foram executados", "não versionadas", "valores e CV",
    "0,051", "1 mil", "MANIFESTO_SHA256.csv", "descritiva",
    "não", "covariância", "não foram confrontados"
  )) testthat::expect_true(grepl(token, doc, fixed = TRUE), info = token)
})

testthat::test_that("T04.2 so verifica projecao descritiva E09 versionada", {
  csv <- file.path("data", "raw", "pnadc_estudo_i",
                   "resultados_auditados", "contrastes_descritos_e09.csv")
  tab <- read.csv(csv, sep = ";", fileEncoding = "UTF-8",
                  colClasses = "character", check.names = FALSE)
  testthat::expect_identical(nrow(tab), 26L)
  testthat::expect_identical(length(unique(tab$IndicadorId)), 13L)
  testthat::expect_setequal(unique(tab$Eixo), LETTERS[1:5])
  testthat::expect_setequal(unique(tab$Contraste), c("anual", "recente"))
  testthat::expect_true(all(table(tab$IndicadorId) == 2L))
  testthat::expect_true(all(tab$PeriodoDestino == "202609"))
  testthat::expect_true(all(tab$PeriodoOrigem[tab$Contraste == "anual"] == "202509"))
  testthat::expect_true(all(tab$PeriodoOrigem[tab$Contraste == "recente"] == "202606"))
  delta <- as.numeric(tab$ValorDestino) - as.numeric(tab$ValorOrigem)
  testthat::expect_equal(as.numeric(tab$Delta), delta, tolerance = 1e-6)
  testthat::expect_false(any(grepl("CV", names(tab), fixed = TRUE)))
})

testthat::test_that("T04.2 consta no indice sem alegar QA independente", {
  lines <- readLines(file.path("scripts", "pnadc_estudo_i", "README.md"),
                     warn = FALSE, encoding = "UTF-8")
  testthat::expect_true(any(grepl("contratos_e01_e05_T04_2.md",
                                 lines, fixed = TRUE)))
  testthat::expect_true(any(grepl("QA independente", lines, fixed = TRUE)))
  testthat::expect_true(any(grepl("snapshot", lines, fixed = TRUE)))
})
