testthat::test_that("T04.3 E06 transcreve as tres familias Stan historicas", {
  base <- file.path("scripts", "pnadc_estudo_i", "estudo_I", "e07", "stan")
  read_stan <- function(n) paste(readLines(file.path(base, n), warn = FALSE,
                                           encoding = "UTF-8"), collapse = "\n")
  taxa <- read_stan("taxa_marginal.stan")
  cont <- read_stan("contagem_marginal.stan")
  renda <- read_stan("rendimento_marginal.stan")
  for (x in list(taxa, cont, renda)) {
    testthat::expect_true(grepl("real y;", x, fixed = TRUE))
    testthat::expect_true(grepl("real<lower=0> s;", x, fixed = TRUE))
    testthat::expect_true(grepl("generated quantities", x, fixed = TRUE))
    testthat::expect_true(grepl("real y_rep = normal_rng(theta, s);", x, fixed = TRUE))
  }
  for (token in c("real<lower=0, upper=1> p;", "p ~ beta(1, 1);",
                  "y ~ normal(100 * p, s);", "real theta = 100 * p;")) {
    testthat::expect_true(grepl(token, taxa, fixed = TRUE), info = token)
  }
  for (token in c("real<lower=0, upper=1> q;", "real<lower=0> populacao_14mais_mil;",
                  "q ~ beta(1, 1);",
                  "y ~ normal(populacao_14mais_mil * q, s);",
                  "real theta = populacao_14mais_mil * q;")) {
    testthat::expect_true(grepl(token, cont, fixed = TRUE), info = token)
  }
  for (token in c("real<lower=0> mu;", "mu ~ normal(0, 10000);",
                  "y ~ normal(mu, s);", "real theta = mu;")) {
    testthat::expect_true(grepl(token, renda, fixed = TRUE), info = token)
  }
  testthat::expect_false(any(grepl("rho|cov_matrix|multi_normal",
                                  c(taxa, cont, renda))))
})

testthat::test_that("T04.3 E06 confere selecao R, suporte e plug-in do CV", {
  r <- paste(readLines(file.path("scripts", "pnadc_estudo_i", "estudo_I",
                                 "e07", "R", "ajustar_marginais_e07.R"),
                       warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  f <- paste(readLines(file.path("scripts", "pnadc_estudo_i", "estudo_I",
                                 "e07", "R", "funcoes_e07.R"),
                       warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  for (token in c("Periodo == '202609' & Papel == 'principal'",
                  "if (modo == 'smoke')", "desocupacao_pct",
                  "s <- abs(y) * cv / 100",
                  "s <= 0", "tipo == 'contagem'",
                  "data_stan$populacao_14mais_mil <- P",
                  "fit$summary(variables = 'theta')",
                  "summary", "saida")) {
    testthat::expect_true(grepl(token, r, fixed = TRUE), info = token)
  }
  for (token in c("nrow(ref) != 13L", "CVPercentual",
                  "Papel", "PUBLICADO_NUMERICO")) {
    testthat::expect_true(grepl(token, f, fixed = TRUE), info = token)
  }
  y <- 5.2
  cv <- 2
  s <- abs(y) * cv / 100
  testthat::expect_equal(s, 0.104, tolerance = 1e-12)
  testthat::expect_true(s > 0)
  testthat::expect_identical(sum(c(8L, 4L, 1L)), 13L)
})

testthat::test_that("T04.3 verifica priors induzidas e suporte em casos sinteticos", {
  # Transformacoes deterministicas das priors Stan, nao dados PNAD.
  p <- c(0, .25, .5, 1)
  q <- p
  P <- 180
  testthat::expect_equal(100 * p, c(0, 25, 50, 100))
  testthat::expect_equal(P * q, c(0, 45, 90, 180))
  testthat::expect_true(all(100 * p >= 0 & 100 * p <= 100))
  testthat::expect_true(all(P * q >= 0 & P * q <= P))
  # Beta(1,1) e uniforme; theta=100*p ou P*q tambem e uniforme no suporte.
  testthat::expect_equal(stats::dbeta(c(.2, .5, .8), 1, 1), rep(1, 3))
  testthat::expect_equal(stats::dunif(c(20, 50, 80), 0, 100), rep(.01, 3))
  testthat::expect_equal(stats::dunif(c(20, 50, 80), 0, P), rep(1/P, 3))
  # A Normal(0,10000) restrita a mu>=0 tem densidade meia-Normal.
  dens <- function(mu) 2 * stats::dnorm(mu, 0, 10000)
  z <- stats::integrate(dens, 0, Inf)$value
  testthat::expect_equal(z, 1, tolerance = 1e-7)
  testthat::expect_equal(dens(1000), 2 * stats::dnorm(1000, 0, 10000))
  # Normal de observacao nao herda os limites do parametro.
  testthat::expect_gt(stats::pnorm(0, mean = 1, sd = 2), 0)
  testthat::expect_gt(stats::pnorm(101, mean = 99, sd = 2,
                                  lower.tail = FALSE), 0)
})

testthat::test_that("T04.3 nao confunde varianças marginais com rho identificado", {
  # Apenas identidade matematica sintética de T03.2, nao reexecuta E08.
  s0 <- 2
  s1 <- 1
  rhos <- c(-.5, 0, .5, .9)
  vars <- s0^2 + s1^2 - 2*rhos*s0*s1
  testthat::expect_equal(vars, c(7, 5, 3, 1.4))
  testthat::expect_true(all(vars > 0))
  prob <- stats::pnorm(3 / sqrt(vars))
  testthat::expect_true(all(diff(prob) > 0))
  testthat::expect_true(all(prob > 0 & prob < 1))
})

testthat::test_that("T04.3 E06 indica reconstrucao editorial e limites inferenciais", {
  path <- file.path("reports", "pnadc_estudo_i",
                    "e06_especificacao_modelos_T04_3.md")
  testthat::expect_true(file.exists(path))
  d <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  terms <- c("RECONSTRUCAO_EDITORIAL", "E06 contemporâneo",
             "Beta(1,1)", "Uniform(0,100)", "Uniform(0,P_i)",
             "10000", "meia-Normal", "s_i=|y_i| c_i/100",
             "generated quantities", "y_rep",
             "8 taxas", "4 totais", "1 rendimento",
             "202609", "202509", "202606",
             "covariâncias", "rho=0", "1-2(5)",
             "202609", "NAO_DECIDIR", "não demonstra",
             "T04.4", "T01.3–T01.7", "Issue #14",
             "Issues #15", "Etapa 04", "ABERTA",
             "QA PENDENTE", "revisor distinto")
  for (token in terms) {
    testthat::expect_true(grepl(token, d, fixed = TRUE), info = token)
  }
  links <- c("e00_consolidado_T04_1.md",
             "contratos_e01_e05_T04_2.md",
             "inventario_estimandos_e07_T03_1.md",
             "comparacao_alternativas_inferenciais_T03_5.md",
             "taxa_marginal.stan", "contagem_marginal.stan",
             "rendimento_marginal.stan", "ajustar_marginais_e07.R",
             "funcoes_e07.R")
  for (token in links) {
    testthat::expect_true(grepl(token, d, fixed = TRUE), info = token)
  }
  testthat::expect_false(grepl("posterior conjunta identificada",
                              d, fixed = TRUE) &&
                         !grepl("não", d, fixed = TRUE))
})

testthat::test_that("T04.3 foi indexada no README PNAD", {
  d <- paste(readLines(file.path("scripts", "pnadc_estudo_i", "README.md"),
                       warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  for (token in c("e06_especificacao_modelos_T04_3.md",
                  "T04.3", "reconstrução editorial",
                  "QA independente", "covariâncias")) {
    testthat::expect_true(grepl(token, d, fixed = TRUE), info = token)
  }
})
