# T04.5 — checks estruturais e casos sinteticos, SEM E04/E07/E08 privados.
ler_t04_5 <- function(path) paste(readLines(path, warn = FALSE,
                                             encoding = "UTF-8"), collapse = "\n")
raiz_t04_5 <- file.path("scripts", "pnadc_estudo_i", "estudo_I")
md_t04_5 <- file.path("reports", "pnadc_estudo_i",
                      "ponte_e08_e09_T04_5.md")
testthat::test_that("T04.5: 26 chaves documentais espelham a E09 descritiva", {
  doc <- readLines(md_t04_5, encoding = "UTF-8", warn = FALSE)
  linhas <- grep("^\\| T45-[0-9]{2} \\|", doc, value = TRUE)
  testthat::expect_length(linhas, 26L)
  campos <- strsplit(linhas, "|", fixed = TRUE)
  colunas <- lapply(campos, function(x) trimws(x[2:length(x)]))
  d <- do.call(rbind, colunas)
  testthat::expect_equal(ncol(d), 9L)
  testthat::expect_identical(d[, 1], sprintf("T45-%02d", 1:26))
  testthat::expect_true(all(d[, 4] %in% c("anual", "recente")))
  chave <- paste(gsub("`", "", d[, 3], fixed = TRUE), d[, 4])
  testthat::expect_equal(length(unique(chave)), 26L)
  testthat::expect_true(all(d[, 9] == "`NAO_VERIFICADO_NUMERICAMENTE`"))
  proj <- file.path("data", "raw", "pnadc_estudo_i",
                    "resultados_auditados", "contrastes_descritos_e09.csv")
  pub <- utils::read.csv(proj, sep = ";", fileEncoding = "UTF-8",
                         check.names = FALSE, stringsAsFactors = FALSE)
  testthat::expect_equal(nrow(pub), 26L)
  testthat::expect_setequal(chave, paste(pub$IndicadorId, pub$Contraste))
  testthat::expect_true(all(pub$PeriodoDestino == 202609L))
  testthat::expect_true(all(pub$PeriodoOrigem ==
     ifelse(pub$Contraste == "anual", 202509L, 202606L)))
  testthat::expect_equal(max(abs(pub$ValorDestino -
                              pub$ValorOrigem - pub$Delta)), 0,
                         tolerance = 1e-7)
  testthat::expect_false(any(c("ProbSentido", "RhoHipotetico",
                               "theta", "y_rep") %in% names(pub)))
})
testthat::test_that("T04.5: E09 nao usa PPC nem vetos de convergencia", {
  src <- function(p) ler_t04_5(file.path(raiz_t04_5, p))
  e081 <- src(file.path("e08", "diagnostico_e081.py"))
  e082 <- src(file.path("e08", "sensibilidade_e082.py"))
  e083 <- src(file.path("e08", "sensibilidade_e083.py"))
  e084 <- src(file.path("e08", "avaliacao_e084.py"))
  e09 <- src(file.path("e09", "consolidar_e09.py"))
  contem <- function(x, y) grepl(y, x, fixed = TRUE)
  for (campo in c("ppc_cdf_obs", "rhat_rank", "ess_bulk",
                  "ess_tail", "mcse_mean", "divergences", "treedepth_hits_10",
                  "ebfmi_min")) {
    testthat::expect_true(contem(e081, campo), info = campo)
    testthat::expect_false(contem(e09, campo), info = campo)
  }
  testthat::expect_true(contem(e081, "(yrep<=y).mean()"))
  testthat::expect_true(contem(e082, "RHOS = (-0.5, 0.0, 0.5, 0.9)"))
  testthat::expect_true(contem(e082, "norm.cdf(delta/sd)"))
  testthat::expect_true(contem(e083, "roots_hermitenorm(101)"))
  testthat::expect_true(contem(e083, "ProbSentido_NormMomentos"))
  testthat::expect_true(contem(e083, "product((-1,1),repeat=4)"))
  testthat::expect_true(contem(e084, "proxy_ids=['desocupacao_pct'"))
  testthat::expect_true(contem(e084, "LimiteInferiorFrechet=max(0.,float(probs.sum())-4.)"))
  testthat::expect_true(contem(e09, "cenarios = carregar('cenarios_contrastes_e082.csv')"))
  testthat::expect_true(contem(e09, "cenarios['RhoHipotetico'] == 0"))
  testthat::expect_true(contem(e09, "ProbMinCenariosRho=('ProbSentido', 'min')"))
  testthat::expect_true(contem(e09, "ProbMaxCenariosRho=('ProbSentido', 'max')"))
  testthat::expect_false(contem(e09, "diagnosticos_e081.csv"))
  testthat::expect_false(contem(e09, "sensibilidade_priors_contrastes_e083.csv"))
  testthat::expect_true(contem(ler_t04_5(md_t04_5), "não revalidado"))
})
testthat::test_that("T04.5: oraculos sinteticos separam PPC, rho e Fréchet", {
  # PPC preditiva marginal: freq de replica <= y, NAO P(Delta favoravel).
  yrep <- c(-2, -1, 0, 2, 3, 5)
  testthat::expect_equal(mean(yrep <= 0), 3/6)
  testthat::expect_equal(mean(yrep <= 2), 4/6)
  # Covariancia escolhida altera ProbSentido sem novos draws Stan.
  s0 <- 2; s1 <- 1; delta <- 3
  rhos <- c(-0.5, 0, 0.5, 0.9)
  variancias <- s0^2+s1^2-2*rhos*s0*s1
  testthat::expect_equal(variancias, c(7, 5, 3, 1.4),
                         tolerance = 1e-12)
  probs <- stats::pnorm(delta/sqrt(variancias))
  testthat::expect_true(all(diff(probs) > 0))
  testthat::expect_equal(probs[2], stats::pnorm(3/sqrt(5)),
                         tolerance = 1e-12)
  testthat::expect_equal(probs + stats::pnorm(-delta/sqrt(variancias)),
                         rep(1, 4), tolerance = 1e-12)
  testthat::expect_equal(min(probs), probs[1])
  testthat::expect_equal(max(probs), probs[4])
  # Cinco proxies: algebra dos limites, jamais modelo da dependencia.
  p <- c(0.9, 0.9, 0.9, 0.9, 0.9)
  lim <- c(max(0, sum(p)-4), min(p))
  testthat::expect_equal(lim, c(0.5, 0.9), tolerance = 1e-12)
  testthat::expect_true(prod(p) >= lim[1] && prod(p) <= lim[2])
  testthat::expect_equal(nrow(expand.grid(rep(list(c(-1,1)), 4))),
                         16L)
})
testthat::test_that("T04.5: faltas historicas nao sao atestados de execucao", {
  doc <- ler_t04_5(md_t04_5)
  for (termo in c("RECONSTRUCAO_EDITORIAL", "NAO_VERIFICADO_NUMERICAMENTE",
                  "PENDENTES DE EVIDÊNCIA", "PPC", "convergência",
                  "52 cadeias", "relato histórico", "T01.3", "Issue #14",
                  "D00-01", "sem artefatos", "não revalidado")) {
    testthat::expect_true(grepl(termo, doc, fixed = TRUE), info = termo)
  }
  testthat::expect_true(grepl("ponte_e08_e09_T04_5.md",
    ler_t04_5(file.path("scripts", "pnadc_estudo_i", "README.md")),
    fixed = TRUE))
})
