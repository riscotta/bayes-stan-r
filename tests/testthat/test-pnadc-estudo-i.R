testthat::test_that("pnadc_estudo_i: fontes, eixos e sintese E09", {
  estudo <- file.path("scripts", "pnadc_estudo_i")
  fontes <- file.path(estudo, "estudo_I", "e07")
  resultado <- file.path("data", "raw", "pnadc_estudo_i", "resultados_auditados",
                         "contrastes_descritos_e09.csv")
  testthat::expect_true(file.exists(file.path(estudo, "README.md")))
  testthat::expect_true(file.exists(file.path(estudo, "auditar_e10.py")))
  for (modelo in c("taxa_marginal.stan", "contagem_marginal.stan", "rendimento_marginal.stan")) {
    testthat::expect_true(file.exists(file.path(fontes, "stan", modelo)))
  }
  for (script in c("funcoes_e07.R", "preflight_e07.R", "ajustar_marginais_e07.R",
                   "reprocessar_resumos_e07.R")) {
    testthat::expect_true(file.exists(file.path(fontes, "R", script)))
  }
  testthat::expect_true(file.exists(resultado))
  d <- utils::read.csv(resultado, sep = ";", check.names = FALSE, fileEncoding = "UTF-8")
  testthat::expect_equal(nrow(d), 26L)
  testthat::expect_equal(length(unique(d$IndicadorId)), 13L)
  testthat::expect_equal(as.integer(table(d$Eixo)), c(4L, 8L, 8L, 4L, 2L))
  testthat::expect_true(all(d$PeriodoDestino == 202609L))
  testthat::expect_true(all(d$PeriodoOrigem == ifelse(d$Contraste == "anual", 202509L, 202606L)))
  testthat::expect_lt(max(abs(d$ValorDestino - d$ValorOrigem - d$Delta)), 1e-7)
  p <- d[d$IndicadorId == "desocupacao_pct", ]
  testthat::expect_equal(nrow(p), 2L)
  testthat::expect_true(all(p$ValorDestino == 5.2))
})
