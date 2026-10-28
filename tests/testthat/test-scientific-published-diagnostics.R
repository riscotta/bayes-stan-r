testthat::test_that("Missing Migrants: diagnóstico publicado preserva ressalvas", {
  dir <- "data/raw/missing_migrants/resultados_auditados"
  diag <- read.csv(file.path(dir, "E08_resumo_diagnostico_mcmc.csv"))
  summary <- read.csv(file.path(dir, "E10_resultados_chave.csv"), fileEncoding = "UTF-8-BOM")
  value <- function(key) as.numeric(diag$valor[diag$metrica == key])
  testthat::expect_equal(value("n_rhat_gt_1_01"), 4)
  testthat::expect_gt(value("rhat_max"), 1.01)
  testthat::expect_lt(value("ess_bulk_min"), 400)
  testthat::expect_equal(value("divergencias_total"), 0)
  published_rhat <- as.numeric(summary$valor[summary$indicador == "R-hat máximo"])
  testthat::expect_equal(published_rhat, value("rhat_max"), tolerance = 1e-10)
  ppc <- summary$valor[summary$indicador == "Falhas principais"]
  testthat::expect_length(ppc, 1L)
  testthat::expect_true(grepl("n_zeros", ppc, fixed = TRUE))
  testthat::expect_true(grepl("desvio_padrao", ppc, fixed = TRUE))
})

testthat::test_that("UNODC: PSIS-LOO por país não é usado como validação para país novo", {
  dir <- "data/raw/prisons_prisoners_unodc/resultados_auditados"
  loo <- read.csv(file.path(dir, "PSIS_LOO_resumo_M1_M2_M3.csv"))
  by_country <- loo[loo$level == "country", ]
  testthat::expect_setequal(by_country$model, c("M1","M2","M3"))
  testthat::expect_true(all(by_country$n_k_gt_0_7 > 0))
  testthat::expect_true(all(tolower(as.character(by_country$warning)) == "true"))
  obs <- loo[loo$level == "observation", ]
  testthat::expect_equal(length(unique(obs$n_units)), 3L)

  rhat <- read.csv(file.path(dir, "auditoria_rhat_M1_REV5_por_categoria.csv"))
  testthat::expect_equal(rhat$n_rhat_gt_101[rhat$categoria == "country_effect"], 45L)
  testthat::expect_equal(rhat$n_rhat_gt_101[rhat$categoria == "region_base"], 2L)

  sbc2 <- read.csv(file.path(dir, "sbc_status_M2.csv"))
  sbc3 <- read.csv(file.path(dir, "sbc_status_M3.csv"))
  testthat::expect_equal(c(nrow(sbc2), nrow(sbc3)), c(20L, 20L))
  testthat::expect_gt(max(sbc2$rhat_max), 1.05)
  testthat::expect_gt(max(sbc3$rhat_max), 1.09)
})
