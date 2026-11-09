# T03.7 — Casos inteiramente sinteticos; sem refit, MCMC ou dados restritos.
ler_t03_7 <- function(caminho) {
  paste(readLines(caminho, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}
raiz_t03_7 <- file.path("scripts", "pnadc_estudo_i", "estudo_I")

testthat::test_that("T03.7: oraculo sintetico gaussiano verifica covariancia e sinal", {
  e082 <- ler_t03_7(file.path(raiz_t03_7, "e08", "sensibilidade_e082.py"))
  testthat::expect_true(grepl("s0*s0+s1*s1-2*rho*s0*s1", e082, fixed = TRUE))
  testthat::expect_true(grepl("norm.cdf(delta/sd)", e082, fixed = TRUE))
  testthat::expect_true(grepl("RHOS = (-0.5, 0.0, 0.5, 0.9)", e082, fixed = TRUE))

  s0 <- 2; s1 <- 1; delta <- 3
  rhos <- c(-0.5, 0, 0.5, 0.9)
  v_esperada <- c(7, 5, 3, 1.4)
  p_esperada <- c(0.8715803710, 0.9101437526,
                  0.9583677417, 0.9943850567)
  v <- numeric(length(rhos))
  for (i in seq_along(rhos)) {
    rho <- rhos[i]
    sigma <- matrix(c(s0^2, rho*s0*s1, rho*s0*s1, s1^2), 2, 2)
    testthat::expect_true(min(eigen(sigma, symmetric = TRUE,
                                   only.values = TRUE)$values) > 0)
    v[i] <- as.numeric(t(c(-1, 1)) %*% sigma %*% c(-1, 1))
    testthat::expect_equal(v[i], s0^2+s1^2-2*rho*s0*s1,
                           tolerance = 1e-12)
    testthat::expect_equal(v[i], v_esperada[i], tolerance = 1e-12)
    testthat::expect_equal(stats::pnorm(delta/sqrt(v[i])),
                           p_esperada[i], tolerance = 1e-9)
  }
  testthat::expect_true(all(diff(v) < 0))
  testthat::expect_true(all(v >= (s0-s1)^2 & v <= (s0+s1)^2))
  # Mesmas marginais, quatro contrastes condicionais diferentes.
  testthat::expect_true(diff(range(p_esperada)) > 0.1)
  # O limite em rho=+1 com s0=s1 pode ser singular: nao simular posterior.
  testthat::expect_equal((s0-s1)^2, 1)
  testthat::expect_equal((s0+s1)^2, 9)
})

testthat::test_that("T03.7: fronteiras de direcao, escala e denominador", {
  prob_sinal <- function(delta, s0, s1, rho, direcao) {
    stopifnot(s0 > 0, s1 > 0, abs(rho) < 1,
              direcao %in% c(-1, 1))
    v <- s0^2 + s1^2 - 2*rho*s0*s1
    stopifnot(v > 0)
    stats::pnorm(direcao*delta/sqrt(v))
  }
  for (rho in c(-0.5, 0, 0.5, 0.9)) {
    testthat::expect_equal(prob_sinal(0, 2, 1, rho, 1), 0.5)
    p_mais <- prob_sinal(3, 2, 1, rho, 1)
    p_menos <- prob_sinal(3, 2, 1, rho, -1)
    testthat::expect_equal(p_mais+p_menos, 1, tolerance = 1e-14)
    testthat::expect_equal(prob_sinal(-3, 2, 1, rho, 1), p_menos,
                           tolerance = 1e-14)
    testthat::expect_true(all(c(p_mais, p_menos) >= 0 &
                              c(p_mais, p_menos) <= 1))
  }
  cv <- 8 # unidade: percentual (nao fracao).
  se <- function(y, cv_pct) abs(y)*cv_pct/100
  testthat::expect_equal(se(125, cv), 10)
  testthat::expect_equal(se(-125, cv), 10)
  testthat::expect_equal(se(0, cv), 0)
  testthat::expect_equal(se(250, cv), 2*se(125, cv))
  # CV positivo nao garante escala estritamente positiva em y=0.
  testthat::expect_error(prob_sinal(1, se(0, cv), 1, 0, 1))
  taxa <- 100*c(0, 0.4, 1)
  contagem <- 120000*c(0, 0.4, 1)
  testthat::expect_true(all(taxa >= 0 & taxa <= 100))
  testthat::expect_true(all(contagem >= 0 & contagem <= 120000))
  testthat::expect_true(stats::pnorm(0, mean = 1, sd = 2) > 0)
  # theta tem suporte; y_rep Normal nao tem o mesmo suporte.
  stan <- file.path(raiz_t03_7, "e07", "stan")
  testthat::expect_true(grepl("theta = 100 * p",
          ler_t03_7(file.path(stan, "taxa_marginal.stan")), fixed = TRUE))
  testthat::expect_true(grepl("populacao_14mais_mil * q",
          ler_t03_7(file.path(stan, "contagem_marginal.stan")), fixed = TRUE))
  testthat::expect_true(grepl("real<lower=0> mu;",
          ler_t03_7(file.path(stan, "rendimento_marginal.stan")), fixed = TRUE))
})

testthat::test_that("T03.7: arredondamento sintetico e grade finita de vertices", {
  limites_delta <- function(y0, y1, h0, h1) {
    c((y1-y0)-(h0+h1), (y1-y0)+(h0+h1))
  }
  testthat::expect_equal(limites_delta(5.2, 5.3, 0.05, 0.05),
                         c(0, 0.2), tolerance = 1e-12)
  testthat::expect_equal(limites_delta(10, 10.21, 0.05, 0.05),
                         c(0.11, 0.31), tolerance = 1e-12)
  testthat::expect_equal(limites_delta(10, 10, 0.5, 0.5), c(-1, 1))
  # Grade de 2^4 cantos: dois valores e dois CVs perturbados.
  cantos <- expand.grid(e0 = c(-1, 1), e1 = c(-1, 1),
                        f0 = c(-1, 1), f1 = c(-1, 1))
  testthat::expect_equal(nrow(cantos), 16L)
  y0 <- 10; y1 <- 13; cv0 <- 10; cv1 <- 10
  h_y <- 0.05; h_cv <- 0.05; rho <- 0.5
  probabilidades <- apply(cantos, 1, function(canto) {
    a <- y0+canto[1]*h_y; b <- y1+canto[2]*h_y
    c0 <- cv0+canto[3]*h_cv; c1 <- cv1+canto[4]*h_cv
    s0 <- abs(a)*c0/100; s1 <- abs(b)*c1/100
    v <- s0^2+s1^2-2*rho*s0*s1
    stopifnot(c0 > 0, c1 > 0, v > 0)
    stats::pnorm((b-a)/sqrt(v))
  })
  testthat::expect_equal(length(probabilidades), 16L)
  testthat::expect_true(all(is.finite(probabilidades)))
  testthat::expect_true(all(probabilidades >= 0 & probabilidades <= 1))
  testthat::expect_true(min(probabilidades) <= max(probabilidades))
  e083 <- ler_t03_7(file.path(raiz_t03_7, "e08", "sensibilidade_e083.py"))
  testthat::expect_true(grepl("product((-1,1),repeat=4)", e083, fixed = TRUE))
  testthat::expect_true(grepl("ProbMinExtremos", e083, fixed = TRUE))
  # Min/max sobre 16 pontos NAO constitui limite global continuo comprovado.
})

testthat::test_that("T03.7: limites de Frechet para cinco proxies", {
  frechet5 <- function(p) {
    stopifnot(length(p) == 5L, all(is.finite(p)),
              all(p >= 0 & p <= 1))
    c(inferior = max(0, sum(p)-4), superior = min(p))
  }
  testthat::expect_equal(unname(frechet5(rep(0.9, 5))), c(0.5, 0.9),
                         tolerance = 1e-14)
  testthat::expect_equal(unname(frechet5(c(1, 0, 0.6, 0.8, 0.4))),
                         c(0, 0))
  for (p in list(rep(0, 5), rep(1, 5), rep(0.9, 5),
                 c(0.2, 0.5, 0.7, 0.8, 0.9))) {
    b <- frechet5(p)
    testthat::expect_true(0 <= b[1] && b[1] <= b[2] && b[2] <= 1)
    testthat::expect_true(prod(p) >= b[1]-1e-12 &&
                          prod(p) <= b[2]+1e-12)
  }
  e084 <- ler_t03_7(file.path(raiz_t03_7, "e08", "avaliacao_e084.py"))
  testthat::expect_true(grepl("LimiteInferiorFrechet", e084, fixed = TRUE))
  testthat::expect_true(grepl("LimiteSuperiorFrechet", e084, fixed = TRUE))
  testthat::expect_true(grepl("proxy_ids", e084, fixed = TRUE))
  # O produto e apenas hipotese de independencia, nao uma estimativa.
})

testthat::test_that("T03.7: E09 publico preserva invariantes descritivas", {
  csv <- file.path("data", "raw", "pnadc_estudo_i",
                   "resultados_auditados", "contrastes_descritos_e09.csv")
  testthat::expect_true(file.exists(csv))
  d <- utils::read.csv(csv, sep = ";", check.names = FALSE,
                       fileEncoding = "UTF-8")
  testthat::expect_equal(nrow(d), 26L)
  testthat::expect_equal(length(unique(d$IndicadorId)), 13L)
  testthat::expect_true(all(table(d$IndicadorId) == 2L))
  testthat::expect_true(all(table(d$Contraste) == 13L))
  testthat::expect_false(any(duplicated(d[c("IndicadorId", "Contraste")])))
  testthat::expect_true(all(is.finite(d$Delta)))
  testthat::expect_lt(max(abs(d$ValorDestino-d$ValorOrigem-d$Delta)), 1e-7)
  testthat::expect_true(all(d$PeriodoDestino == 202609L))
  testthat::expect_true(all(d$PeriodoOrigem ==
                           ifelse(d$Contraste == "anual", 202509L, 202606L)))
  testthat::expect_false(any(c("RhoHipotetico", "ProbSentido",
                               "theta", "y_rep", "Covariancia") %in% names(d)))
  e09 <- ler_t03_7(file.path(raiz_t03_7, "e09", "consolidar_e09.py"))
  testthat::expect_true(grepl("RhoHipotetico'] == 0", e09, fixed = TRUE))
  # E09 integral nao esta aqui: este teste nao o recria.
})
