testthat::test_that("T03.5: fontes e classes de inferencia estao delimitadas", {
  dir_base <- file.path("scripts", "pnadc_estudo_i", "estudo_I")
  doc <- file.path("reports", "pnadc_estudo_i",
                   "comparacao_alternativas_inferenciais_T03_5.md")
  fontes <- c(
    doc,
    file.path(dir_base, "e07", "R", "ajustar_marginais_e07.R"),
    file.path(dir_base, "e08", "sensibilidade_e082.py"),
    file.path(dir_base, "e08", "sensibilidade_e083.py"),
    file.path(dir_base, "e09", "consolidar_e09.py")
  )
  testthat::expect_true(all(file.exists(fontes)))
  ler <- function(path) paste(readLines(path, encoding = "UTF-8",
                                        warn = FALSE), collapse = "\n")
  d <- ler(doc)
  e07 <- ler(fontes[[2]])
  e082 <- ler(fontes[[3]])
  e083 <- ler(fontes[[4]])
  e09 <- ler(fontes[[5]])
  tem <- function(x, texto) grepl(texto, x, fixed = TRUE)

  # Teste de origem estrutural: nao executa Stan, E04 ou MCMC.
  testthat::expect_true(tem(e07, "202609"))
  testthat::expect_true(tem(e082, "RHOS = (-0.5, 0.0, 0.5, 0.9)"))
  testthat::expect_true(tem(e082, "prob_positive = float(norm.cdf(delta/sd))"))
  testthat::expect_true(tem(e083, "ProbSentido_NormMomentos"))
  testthat::expect_true(tem(e09, "RhoHipotetico'] == 0"))
  testthat::expect_true(tem(d, "não identificam"))
  testthat::expect_true(tem(d, "covariâncias"))
  testthat::expect_true(tem(d, "T03.6"))
  testthat::expect_true(tem(d, "T03.7"))
  testthat::expect_true(tem(d, "QA independente"))
  testthat::expect_true(tem(d, "Issue #14"))
})

testthat::test_that("T03.5: mesmas marginais admitem dependencias diferentes", {
  s0 <- 2
  s1 <- 1
  delta <- 3
  rh <- c(-0.5, 0, 0.5, 0.9)
  sigma <- function(rho) {
    matrix(c(s0^2, rho*s0*s1,
             rho*s0*s1, s1^2), nrow = 2)
  }
  matrizes <- lapply(rh, sigma)
  testthat::expect_true(all(vapply(matrizes, function(m) {
    isTRUE(all.equal(diag(m), c(s0^2, s1^2)))
  }, logical(1))))
  testthat::expect_true(all(vapply(matrizes, function(m) {
    min(eigen(m, symmetric = TRUE, only.values = TRUE)$values) > 0
  }, logical(1))))
  testthat::expect_true(length(unique(vapply(matrizes, function(m) m[1, 2],
                                         numeric(1)))) == length(rh))

  a <- c(-1, 1)
  var_linear <- vapply(matrizes, function(m) as.numeric(t(a) %*% m %*% a),
                       numeric(1))
  var_formula <- s0^2 + s1^2 - 2*rh*s0*s1
  testthat::expect_equal(var_linear, var_formula, tolerance = 1e-12)
  testthat::expect_equal(var_formula, c(7, 5, 3, 1.4),
                         tolerance = 1e-12)
  prob <- stats::pnorm(delta / sqrt(var_linear))
  testthat::expect_true(all(diff(prob) > 0))
  testthat::expect_equal(round(prob, 6),
                         c(0.871580, 0.910144, 0.958368, 0.994385),
                         tolerance = 1e-6)
  # Mudancas de rho podem reduzir P se o contraste for desfavoravel.
  testthat::expect_true(all(diff(stats::pnorm(-delta/sqrt(var_linear))) < 0))
  testthat::expect_equal(stats::pnorm(0/sqrt(var_linear)),
                         rep(0.5, length(rh)))
})

testthat::test_that("T03.5: marginais de eventos nao identificam intersecao", {
  p <- rep(0.9, 5)
  inferior <- max(0, sum(p) - (length(p)-1))
  superior <- min(p)
  testthat::expect_equal(inferior, 0.5, tolerance = 1e-12)
  testthat::expect_equal(superior, 0.9, tolerance = 1e-12)
  independencia_postulada <- prod(p)
  testthat::expect_gt(independencia_postulada, inferior)
  testthat::expect_lt(independencia_postulada, superior)

  # Extremos das marginais gaussianas nao impõem suporte para niveis.
  testthat::expect_gt(stats::pnorm(0, mean = 0.2, sd = 1), 0.42)
  testthat::expect_equal(abs(0)*8/100, 0)
  # Verificacao sintética nao prova covariancia, cobertura ou modelo conjunto.
})
