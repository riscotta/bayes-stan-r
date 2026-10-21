testthat::test_that("caes_raca_f5: estrutura e fontes historicas", {
  study <- file.path("scripts", "caes_raca_f5")
  raw <- file.path("data", "raw", "caes_raca_f5", "resultados_auditados")
  testthat::expect_true(file.exists(file.path(study,"caes_raca_f5_cmdstanr.R")))
  for (f in c("metricas_full_E08_MP.csv", "metricas_por_fold_auditadas.csv",
              "contrastes_por_fold_auditados.csv", "bootstrap_independente_2000.csv"))
    testthat::expect_true(file.exists(file.path(raw,f)))
  m <- utils::read.csv(file.path(raw,"metricas_full_E08_MP.csv"))
  testthat::expect_equal(nrow(m),7L)
  testthat::expect_equal(unique(m$n_caes),2084L)
  for (fam in c("t5","normal","skew")) {
    a <- m[m$familia==fam & m$modelo=="M1",]
    b <- m[m$familia==fam & m$modelo=="M2",]
    testthat::expect_lt(b$rmse,a$rmse)
  }
  original <- file.path(study,"original","E08_MP_Entregaveis")
  testthat::expect_true(file.exists(file.path(original,"02_codigo","executar_E08_MP.R")))
  testthat::expect_true(file.exists(file.path(original,"01_especificacao","aprovacao_MP1.json")))
})
