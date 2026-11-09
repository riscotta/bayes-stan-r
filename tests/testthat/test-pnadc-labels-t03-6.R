ler_t03_6 <- function(path) {
  paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}

testthat::test_that("T03.6: rotulos refletem campos preservados de E07-E09", {
  base <- file.path("scripts", "pnadc_estudo_i", "estudo_I")
  e081 <- file.path(base, "e08", "diagnostico_e081.py")
  e082 <- file.path(base, "e08", "sensibilidade_e082.py")
  e083 <- file.path(base, "e08", "sensibilidade_e083.py")
  e084 <- file.path(base, "e08", "avaliacao_e084.py")
  e09 <- file.path(base, "e09", "consolidar_e09.py")
  rotulos <- file.path("reports", "pnadc_estudo_i",
                       "rotulos_inferenciais_T03_6.md")
  arquivos <- c(e081, e082, e083, e084, e09, rotulos)
  testthat::expect_true(all(file.exists(arquivos)))
  d <- ler_t03_6(rotulos)
  campos <- list(
    e081 = c("ppc_cdf_obs"),
    e082 = c("RhoHipotetico", "ProbDeltaPositivo", "ProbSentido",
             "IC95Inf", "IC95Sup", "InfProbSentidoAbre", "SupProbSentidoAbre"),
    e083 = c("ProbSentido_NormMomentos", "ProbMinExtremos", "ProbMaxExtremos"),
    e084 = c("MinProbCenariosRho", "MaxProbCenariosRho",
             "LimiteInferiorFrechet", "LimiteSuperiorFrechet"),
    e09 = c("ProbSentido", "ProbMinCenariosRho", "ProbMaxCenariosRho")
  )
  fontes <- list(e081 = e081, e082 = e082, e083 = e083,
                 e084 = e084, e09 = e09)
  for (etapa in names(fontes)) {
    origem <- ler_t03_6(fontes[[etapa]])
    for (campo in campos[[etapa]]) {
      testthat::expect_true(grepl(campo, origem, fixed = TRUE),
                            info = paste(etapa, campo, "ausente no fonte"))
      testthat::expect_true(grepl(campo, d, fixed = TRUE),
                            info = paste(etapa, campo, "ausente no dicionario"))
    }
  }
  testthat::expect_true(grepl("RhoHipotetico'] == 0",
                              ler_t03_6(e09), fixed = TRUE))
  testthat::expect_true(grepl("1.959963984540054",
                              ler_t03_6(e082), fixed = TRUE))
})

testthat::test_that("T03.6: nota e indice limitam a interpretacao", {
  nota <- ler_t03_6(file.path("reports", "pnadc_estudo_i", "nota_cientifica.md"))
  indice <- ler_t03_6(file.path("scripts", "pnadc_estudo_i", "README.md"))
  dic <- ler_t03_6(file.path("reports", "pnadc_estudo_i",
                               "rotulos_inferenciais_T03_6.md"))
  testthat::expect_true(grepl("rotulos_inferenciais_T03_6.md",
                              nota, fixed = TRUE))
  testthat::expect_true(grepl("rotulos_inferenciais_T03_6.md",
                              indice, fixed = TRUE))
  termos <- c("posterior temporal conjunta", "hipotético",
             "cinco", "Fréchet", "IBGE", "arredondamento")
  for (termo in termos) {
    testthat::expect_true(grepl(termo, nota, fixed = TRUE), info = termo)
    testthat::expect_true(grepl(termo, dic, fixed = TRUE), info = termo)
  }
  testthat::expect_true(grepl("sem alteração", dic, fixed = TRUE) ||
                              grepl("não altera", dic, fixed = TRUE))
  testthat::expect_true(grepl("T03.7", indice, fixed = TRUE))
  testthat::expect_true(grepl("Issue #14", dic, fixed = TRUE))
})

testthat::test_that("T03.6: a projecao publica E09 continua descritiva", {
  csv <- file.path("data", "raw", "pnadc_estudo_i",
                   "resultados_auditados", "contrastes_descritos_e09.csv")
  testthat::expect_true(file.exists(csv))
  header <- readLines(csv, n = 1L, encoding = "UTF-8", warn = FALSE)
  testthat::expect_length(header, 1L)
  testthat::expect_true(grepl("Delta", header, fixed = TRUE))
  testthat::expect_true(grepl("SituacaoOficialIBGE", header, fixed = TRUE))
  testthat::expect_false(grepl("ProbSentido", header, fixed = TRUE))
  testthat::expect_false(grepl("RhoHipotetico", header, fixed = TRUE))
})
