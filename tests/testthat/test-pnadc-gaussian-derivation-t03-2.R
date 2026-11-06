testthat::test_that("T03.2: derivacao de contrastes gaussianos documentada e coerente", {
  relatorio <- file.path("reports", "pnadc_estudo_i",
                         "derivacao_contrastes_gaussianos_T03_2.md")
  fonte <- file.path("scripts", "pnadc_estudo_i", "estudo_I", "e08",
                     "sensibilidade_e082.py")
  testthat::expect_true(file.exists(relatorio))
  testthat::expect_true(file.exists(fonte))

  doc <- paste(readLines(relatorio, warn = FALSE, encoding = "UTF-8"),
               collapse = "\n")
  src <- readLines(fonte, warn = FALSE, encoding = "UTF-8")
  testthat::expect_true(grepl("Var(Delta | rho)", doc, fixed = TRUE))
  testthat::expect_true(grepl("não identificada", doc, fixed = TRUE))
  testthat::expect_true(grepl("T03.3", doc, fixed = TRUE))
  testthat::expect_true(grepl("T03.7", doc, fixed = TRUE))
  testthat::expect_true(grepl("s0*s0+s1*s1-2*rho*s0*s1",
                            paste(src, collapse = " "), fixed = TRUE))
  testthat::expect_true(any(grepl("norm.cdf(delta/sd)", src, fixed = TRUE)))
  testthat::expect_true(any(grepl("RHOS = (-0.5, 0.0, 0.5, 0.9)", src,
                                 fixed = TRUE)))
  testthat::expect_true(any(grepl("sd_min = abs(s1-s0)", src, fixed = TRUE)))
  testthat::expect_true(any(grepl("sd_max = s1+s0", src, fixed = TRUE)))

  # Exemplo sintético puramente matemático, sem ler os insumos E04/E07.
  s0 <- 2
  s1 <- 1
  delta <- 3
  rhos <- c(-0.5, 0, 0.5, 0.9)
  var_esperada <- c(7, 5, 3, 1.4)
  prob_esperada <- c(0.8715803710, 0.9101437526,
                     0.9583677417, 0.9943850567)
  for (i in seq_along(rhos)) {
    rho <- rhos[[i]]
    sigma <- matrix(c(s0^2, rho*s0*s1, rho*s0*s1, s1^2),
                    nrow = 2)
    testthat::expect_true(min(eigen(sigma, symmetric = TRUE,
                                    only.values = TRUE)$values) > 0)
    a <- c(-1, 1)
    var_matriz <- as.numeric(t(a) %*% sigma %*% a)
    var_formula <- s0^2 + s1^2 - 2*rho*s0*s1
    testthat::expect_equal(var_matriz, var_formula, tolerance = 1e-12)
    testthat::expect_equal(var_formula, var_esperada[[i]], tolerance = 1e-12)
    testthat::expect_equal(stats::pnorm(delta/sqrt(var_formula)),
                           prob_esperada[[i]], tolerance = 1e-9)
    testthat::expect_equal(stats::pnorm(-delta/sqrt(var_formula)),
                           1-stats::pnorm(delta/sqrt(var_formula)),
                           tolerance = 1e-12)
  }
  testthat::expect_equal(abs(s1-s0), 1)
  testthat::expect_equal(s1+s0, 3)
})
