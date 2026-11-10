# T04.7 — verificacoes editoriais e aritmeticas, sem dados novos ou MCMC.
ler_t04_7 <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}
root_t04_7 <- file.path("reports", "pnadc_estudo_i")
csv_t04_7 <- file.path("data", "raw", "pnadc_estudo_i",
                       "resultados_auditados", "contrastes_descritos_e09.csv")

testthat::test_that("T04.7: exemplos PNAD batem com a projecao historica", {
  testthat::expect_true(file.exists(csv_t04_7))
  d <- utils::read.csv(csv_t04_7, sep = ";", dec = ".", check.names = FALSE,
                       fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  testthat::expect_equal(nrow(d), 26L)
  testthat::expect_equal(length(unique(d$IndicadorId)), 13L)
  ex <- list(
    list(id = "desocupacao_pct", janela = "recente", ini = 5.4, fim = 5.2,
         delta = -0.2, situacao = "caiu"),
    list(id = "desalentados_mil", janela = "recente", ini = 2288, fim = 2459,
         delta = 171, situacao = "cresceu"),
    list(id = "rendimento_real_habitual_rs", janela = "recente",
         ini = 3753, fim = 3790, delta = 37, situacao = "estavel"),
    list(id = "nivel_ocupacao_pct", janela = "anual",
         ini = 58.7, fim = 59.0, delta = 0.3, situacao = "estavel"),
    list(id = "nivel_ocupacao_pct", janela = "recente",
         ini = 58.8, fim = 59.0, delta = 0.2, situacao = "cresceu")
  )
  for (e in ex) {
    x <- d[d$IndicadorId == e$id & d$Contraste == e$janela, ]
    testthat::expect_equal(nrow(x), 1L, info = e$id)
    testthat::expect_equal(x$ValorOrigem, e$ini, tolerance = 1e-8)
    testthat::expect_equal(x$ValorDestino, e$fim, tolerance = 1e-8)
    testthat::expect_equal(x$Delta, e$delta, tolerance = 1e-8)
    testthat::expect_equal(x$SituacaoOficialIBGE, e$situacao)
    testthat::expect_equal(x$PeriodoDestino, 202609L)
    testthat::expect_equal(x$PeriodoOrigem,
                           if (e$janela == "recente") 202606L else 202509L)
  }
  testthat::expect_false(any(c("ProbSentido", "RhoHipotetico",
                               "CVPercentual", "Covariancia") %in% names(d)))
})

testthat::test_that("T04.7: analogias ficam subordinadas aos exemplos PNAD", {
  paths <- c(
    "derivacao_contrastes_gaussianos_T03_2.md",
    "auditoria_suporte_cv_arredondamento_T03_4.md",
    "comparacao_alternativas_inferenciais_T03_5.md",
    "rotulos_inferenciais_T03_6.md",
    "invariantes_sinteticos_T03_7.md",
    "exemplos_didaticos_T04_7.md")
  full <- file.path(root_t04_7, paths)
  testthat::expect_true(all(file.exists(full)))
  docs <- lapply(full, ler_t04_7)
  names(docs) <- paths
  for (k in seq_len(4L)) {
    testthat::expect_true(grepl("desocupacao_pct", docs[[k]], fixed = TRUE),
                          info = paths[[k]])
    testthat::expect_true(grepl("5,4", docs[[k]], fixed = TRUE),
                          info = paths[[k]])
    testthat::expect_true(grepl("5,2", docs[[k]], fixed = TRUE),
                          info = paths[[k]])
    testthat::expect_true(grepl("sintétic", docs[[k]], fixed = TRUE) ||
                          grepl("sintetic", docs[[k]], fixed = TRUE),
                          info = paths[[k]])
  }
  testthat::expect_true(grepl("desocupacao_pct", docs[[5]], fixed = TRUE))
  testthat::expect_true(grepl("p_i=0,9", docs[[5]], fixed = TRUE))
  guia <- docs[[6]]
  for (term in c("desalentados_mil", "rendimento_real_habitual_rs",
                 "NAO_PNAD", "D00-01", "D00-06", "Issue #14",
                 "R1:", "R2:", "R3:", "E08.3 → E08.4 → E09",
                 "QA PENDENTE", "N3/N4")) {
    testthat::expect_true(grepl(term, guia, fixed = TRUE), info = term)
  }
})

testthat::test_that("T04.7: matematica e legenda nao viram estimativas", {
  # Oraculo de arredondamento: condicional a h=0.05, nao IC nem valor oficial.
  h <- 0.05
  delta <- 5.2 - 5.4
  testthat::expect_equal(delta, -0.2, tolerance = 1e-12)
  testthat::expect_equal(c(delta-2*h, delta+2*h),
                         c(-0.3, -0.1), tolerance = 1e-12)
  e032 <- ler_t04_7(file.path(root_t04_7,
                               "derivacao_contrastes_gaussianos_T03_2.md"))
  e036 <- ler_t04_7(file.path(root_t04_7,
                               "rotulos_inferenciais_T03_6.md"))
  guia <- ler_t04_7(file.path(root_t04_7,
                              "exemplos_didaticos_T04_7.md"))
  testthat::expect_true(grepl("não se calcula aqui", e032, fixed = TRUE))
  testthat::expect_true(grepl("não autoriza atribuir probabilidade", e036,
                              fixed = TRUE))
  testthat::expect_true(grepl("não calcula a probabilidade", guia,
                              fixed = TRUE))
  testthat::expect_true(grepl("rho=0", guia, fixed = TRUE))
  testthat::expect_true(grepl("p_i=0,9", guia, fixed = TRUE))
  readme <- ler_t04_7(file.path("scripts", "pnadc_estudo_i", "README.md"))
  testthat::expect_true(grepl("exemplos_didaticos_T04_7.md", readme,
                              fixed = TRUE))
  testthat::expect_true(grepl("T04.7", readme, fixed = TRUE))
})
