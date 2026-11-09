# T04.4: verificacoes de fonte e calculos sinteticos, sem CmdStan/MCMC/E04.
testthat::test_that("T04.4 confronta todas as declaracoes e operacoes dos 3 Stan", {
  pasta <- file.path("scripts", "pnadc_estudo_i", "estudo_I", "e07", "stan")
  statements <- function(nome) {
    x <- readLines(file.path(pasta, nome), warn = FALSE)
    x <- sub("//.*$", "", x)
    x <- trimws(x)
    unname(x[nzchar(x) & grepl(";[[:space:]]*$", x)])
  }
  testthat::expect_identical(statements("taxa_marginal.stan"), c(
    "real y;", "real<lower=0> s;", "real<lower=0, upper=1> p;",
    "p ~ beta(1, 1);", "y ~ normal(100 * p, s);",
    "real theta = 100 * p;", "real y_rep = normal_rng(theta, s);"
  ))
  testthat::expect_identical(statements("contagem_marginal.stan"), c(
    "real y;", "real<lower=0> s;", "real<lower=0> populacao_14mais_mil;",
    "real<lower=0, upper=1> q;", "q ~ beta(1, 1);",
    "y ~ normal(populacao_14mais_mil * q, s);",
    "real theta = populacao_14mais_mil * q;",
    "real y_rep = normal_rng(theta, s);"
  ))
  testthat::expect_identical(statements("rendimento_marginal.stan"), c(
    "real y;", "real<lower=0> s;", "real<lower=0> mu;",
    "mu ~ normal(0, 10000);", "y ~ normal(mu, s);",
    "real theta = mu;", "real y_rep = normal_rng(theta, s);"
  ))
  for (nome in c("taxa_marginal.stan", "contagem_marginal.stan",
                 "rendimento_marginal.stan")) {
    x <- readLines(file.path(pasta, nome), warn = FALSE)
    testthat::expect_identical(
      trimws(x[grepl("^[[:space:]]*(data|parameters|model|generated quantities)[[:space:]]*\\{", x)]),
      c("data {", "parameters {", "model {", "generated quantities {")
    )
    testthat::expect_false(any(grepl(
      "^[[:space:]]*(vector|matrix|array|corr_matrix|cholesky_factor_corr)[[:space:]<\\[]", x
    )))
  }
})
testthat::test_that("T04.4 inventaria 13 IDs exatos e um apoio sem falsos 14 ajustes", {
  x <- readLines(file.path("reports", "pnadc_estudo_i", "contratos_e01_e05_T04_2.md"),
                 warn = FALSE)
  d <- x[grepl("^\\|[[:space:]]*`[^`]+`[[:space:]]*\\|[[:space:]]*[A-E] / (principal|apoio)", x)]
  ids <- sub("^\\|[[:space:]]*`([^`]+)`[[:space:]]*\\|.*$", "\\1", d)
  papel <- sub("^\\|[[:space:]]*`[^`]+`[[:space:]]*\\|[[:space:]]*([A-E] / (principal|apoio)).*$", "\\1", d)
  testthat::expect_length(ids, 14L)
  testthat::expect_length(unique(ids), 14L)
  testthat::expect_identical(ids[grepl("apoio", papel, fixed = TRUE)], "populacao_14mais_mil")
  taxa <- c("desocupacao_pct", "nivel_ocupacao_pct", "participacao_pct",
            "subutilizacao_pct", "subocupacao_horas_pct", "desalento_pct",
            "informalidade_pct", "contrib_previdencia_pct")
  contagem <- c("desocupados_mil", "ocupados_mil", "forca_trabalho_mil", "desalentados_mil")
  renda <- "rendimento_real_habitual_rs"
  principais <- ids[grepl("principal", papel, fixed = TRUE)]
  testthat::expect_length(principais, 13L)
  testthat::expect_setequal(principais, c(taxa, contagem, renda))
  testthat::expect_setequal(principais[grepl("_pct$", principais)], taxa)
  testthat::expect_setequal(principais[grepl("_mil$", principais)], contagem)
  testthat::expect_identical(principais[!grepl("_(pct|mil)$", principais)], renda)
})
testthat::test_that("T04.4 confirma filtros, dados Stan, saidas e guardas divergentes", {
  root <- file.path("scripts", "pnadc_estudo_i", "estudo_I", "e07", "R")
  a <- readLines(file.path(root, "ajustar_marginais_e07.R"), warn = FALSE)
  f <- readLines(file.path(root, "funcoes_e07.R"), warn = FALSE)
  p <- readLines(file.path(root, "preflight_e07.R"), warn = FALSE)
  s <- readLines(file.path(root, "reprocessar_resumos_e07.R"), warn = FALSE)
  has <- function(x, fragment) any(grepl(fragment, x, fixed = TRUE))
  testthat::expect_true(has(a, "Periodo == '202609' & Papel == 'principal'"))
  testthat::expect_true(has(a, "if (modo == 'smoke') ref <- subset(ref, IndicadorId == 'desocupacao_pct')"))
  testthat::expect_true(has(a, "s <- abs(y) * cv / 100"))
  testthat::expect_true(has(a, "if (!is.finite(y) || !is.finite(s) || s <= 0)"))
  testthat::expect_true(has(a, "data_stan <- list(y = y, s = s)"))
  testthat::expect_true(has(a, "data_stan$populacao_14mais_mil <- P"))
  testthat::expect_true(has(a, "summary(variables = 'theta')"))
  testthat::expect_true(has(s, "summary(variables = 'theta')"))
  testthat::expect_true(has(f, "nrow(ref) != 13L"))
  testthat::expect_true(has(p, "any(ref$Valor < 0)"))
  testthat::expect_true(has(a, "if (tipo == 'taxa' && (y < 0 || y > 100))"))
  testthat::expect_true(has(a, "if (!is.finite(P) || P <= 0 || y > P)"))
  l1 <- which(grepl("if (tipo == 'contagem')", a, fixed = TRUE))
  l2 <- which(grepl("data_stan$populacao_14mais_mil <- P", a, fixed = TRUE))
  testthat::expect_length(l1, 1L)
  testthat::expect_length(l2, 1L)
  testthat::expect_false(has(a[l1:l2], "y < 0"))
  testthat::expect_false(has(a, "if (tipo == 'rendimento' && y < 0)"))
  testthat::expect_true(has(p, "Nao houve compilacao nem MCMC."))
})
testthat::test_that("T04.4 oraculos: CV, Jacobianos e meia Normal com suporte", {
  # Três sinais/escalas independentes das instrucoes Stan.
  y <- c(50, -20, 1250)
  cv <- c(2, 10, 4)
  testthat::expect_equal(abs(y) * cv / 100, c(1, 2, 50), tolerance = 1e-12)
  for (P in c(100, 180000)) {
    f <- function(theta) dbeta(theta / P, 1, 1) / P
    testthat::expect_equal(integrate(f, 0, P)$value, 1, tolerance = 1e-9)
    testthat::expect_equal(integrate(f, 0, P / 4)$value, 0.25, tolerance = 1e-9)
    testthat::expect_equal(integrate(f, 0, P / 2)$value, 0.5, tolerance = 1e-9)
  }
  dens_half <- function(mu) ifelse(mu >= 0, 2 * dnorm(mu, 0, 10000), 0)
  testthat::expect_equal(integrate(dens_half, 0, Inf)$value, 1, tolerance = 1e-8)
  testthat::expect_equal(dens_half(0), sqrt(2 / pi) / 10000, tolerance = 1e-12)
  testthat::expect_identical(dens_half(-10), 0)
})
testthat::test_that("T04.4 posterior: CDF fechada versus integral da likelihood", {
  cdf <- function(y, s, a, b, x) {
    (pnorm((x - y) / s) - pnorm((a - y) / s)) /
      (pnorm((b - y) / s) - pnorm((a - y) / s))
  }
  numeric_prob <- function(y, s, a, b, x) {
    f <- function(theta) dnorm(y, mean = theta, sd = s)
    integrate(f, a, x, rel.tol = 1e-9)$value /
      integrate(f, a, b, rel.tol = 1e-9)$value
  }
  for (case in list(c(35, 6, 0, 100, 37), c(-2, 3, 0, 100, 3),
                    c(90000, 15000, 0, 180000, 92000))) {
    y <- case[[1]]; s <- case[[2]]; a <- case[[3]]
    b <- case[[4]]; x <- case[[5]]
    testthat::expect_true(cdf(y, s, a, b, x) > 0 &&
                          cdf(y, s, a, b, x) < 1)
    testthat::expect_equal(numeric_prob(y, s, a, b, x),
                           cdf(y, s, a, b, x), tolerance = 1e-7)
  }
  # Replicas Normais podem extrapolar o suporte de theta.
  testthat::expect_gt(pnorm(0, mean = 0.5, sd = 2), 0.35)
  testthat::expect_gt(pnorm(100, mean = 99.5, sd = 2, lower.tail = FALSE), 0.35)
  testthat::expect_gt(dnorm(-2, mean = 1, sd = 3), 0)
})
testthat::test_that("T04.4 matriz tem IDs unicos e nao alega execucao historica", {
  txt <- readLines(file.path("reports", "pnadc_estudo_i",
                             "ponte_e06_e07_T04_4.md"), warn = FALSE)
  linhas <- grep("^\\| [GTCRX][0-9]{2} \\|", txt, value = TRUE)
  ids <- sub("^\\| ([GTCRX][0-9]{2}) \\|.*$", "\\1", linhas)
  testthat::expect_length(ids, 41L)
  testthat::expect_length(unique(ids), 41L)
  testthat::expect_true(all(c("G01", "T04", "C05", "R04", "X07") %in% ids))
  testthat::expect_true(any(grepl("D2-O1", txt, fixed = TRUE)))
  testthat::expect_true(any(grepl("D2-O2", txt, fixed = TRUE)))
  testthat::expect_false(any(grepl("| CONFORME_EXECUCAO |", txt, fixed = TRUE)))
})
