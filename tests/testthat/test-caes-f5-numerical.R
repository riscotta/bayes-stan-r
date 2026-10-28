testthat::test_that("F5: métricas auditadas preservam ganho pequeno e fold adverso", {
  dir <- "data/raw/caes_raca_f5/resultados_auditados"
  m <- read.csv(file.path(dir, "metricas_full_E08_MP.csv"))
  folds <- read.csv(file.path(dir, "metricas_por_fold_auditadas.csv"))
  ci <- read.csv(file.path(dir, "bootstrap_independente_2000.csv"))
  testthat::expect_equal(nrow(m), 7L)
  testthat::expect_equal(unique(m$n_caes), 2084L)
  testthat::expect_equal(sort(unique(folds$fold)), 1:5)
  for (f in c("normal", "skew", "t5")) {
    m1 <- m$rmse[m$familia == f & m$modelo == "M1"]
    m2 <- m$rmse[m$familia == f & m$modelo == "M2"]
    testthat::expect_length(m1, 1L)
    testthat::expect_length(m2, 1L)
    testthat::expect_gt(m1, m2)
    fold1 <- folds[folds$familia == f & folds$fold == 1, ]
    testthat::expect_gt(fold1$rmse[fold1$modelo == "M2"],
                        fold1$rmse[fold1$modelo == "M1"])
  }
  skew <- ci[ci$familia == "skew" &
             ci$contraste == "delta_logscore_M2_menos_M1", ]
  testthat::expect_equal(nrow(skew), 1L)
  testthat::expect_lt(skew$LI95, 0)
  testthat::expect_gt(skew$LS95, 0)
  testthat::expect_true(all(ci$bootstrap_B == 2000L))
})
