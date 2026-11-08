testthat::test_that("T03.3: origem das probabilidades E08/E09 auditada sem draws temporais", {
  relatorio <- file.path("reports", "pnadc_estudo_i",
                         "rastreabilidade_probabilidades_T03_3.md")
  pasta <- file.path("scripts", "pnadc_estudo_i", "estudo_I")
  e07 <- file.path(pasta, "e07", "R", "ajustar_marginais_e07.R")
  e081 <- file.path(pasta, "e08", "diagnostico_e081.py")
  e082 <- file.path(pasta, "e08", "sensibilidade_e082.py")
  e083 <- file.path(pasta, "e08", "sensibilidade_e083.py")
  e084 <- file.path(pasta, "e08", "avaliacao_e084.py")
  e09 <- file.path(pasta, "e09", "consolidar_e09.py")
  arquivo_publico <- file.path("data", "raw", "pnadc_estudo_i",
                                "resultados_auditados",
                                "contrastes_descritos_e09.csv")
  entradas <- c(relatorio, e07, e081, e082, e083, e084, e09,
                arquivo_publico)
  testthat::expect_true(all(file.exists(entradas)))

  ler <- function(path) paste(readLines(path, warn = FALSE,
                                        encoding = "UTF-8"), collapse = "\n")
  md <- ler(relatorio)
  src07 <- ler(e07)
  src081 <- ler(e081)
  src082 <- ler(e082)
  src083 <- ler(e083)
  src084 <- ler(e084)
  src09 <- ler(e09)
  contem <- function(texto, trecho) grepl(trecho, texto, fixed = TRUE)

  # Posteriores de E07: somente theta marginal em 202609;
  # E08.1 le y_rep de cadeias para PPC, nao contrastes temporais.
  testthat::expect_true(contem(src07, "Periodo == '202609' & Papel == 'principal'"))
  testthat::expect_true(contem(src07, "fit$summary(variables = 'theta')"))
  testthat::expect_true(contem(src081, "a['y_rep'].to_numpy()"))
  testthat::expect_true(contem(src081, "(yrep<=y).mean()"))
  testthat::expect_true(contem(md, "PPC marginal"))

  # E08.2 constroi quatro cenario por contraste sem draw de diferencas.
  testthat::expect_true(contem(src082, "RHOS = (-0.5, 0.0, 0.5, 0.9)"))
  testthat::expect_true(contem(src082, "'anual': (202509, 202609)"))
  testthat::expect_true(contem(src082, "'recente': (202606, 202609)"))
  testthat::expect_true(contem(src082, "prob_positive = float(norm.cdf(delta/sd))"))
  testthat::expect_true(contem(src082, "prob_direction = prob_positive if pdir==1 else 1-prob_positive"))
  testthat::expect_true(contem(src082, "Classificacao='CENARIO_CONDICIONAL'"))
  testthat::expect_true(contem(src082, "e07.theta_mean"))
  testthat::expect_true(contem(src082, "e07.theta_sd"))

  # E08.3: quadratura deterministica e CDF normal dos momentos,
  # diferente de calcular a cauda exata ou um contraste MCMC.
  testthat::expect_true(contem(src083, "roots_hermitenorm(101)"))
  testthat::expect_true(contem(src083, "pp=float(norm.cdf(sign*mu/sd))"))
  testthat::expect_true(contem(src083, "not exact tail integrals"))
  testthat::expect_true(contem(src083, "26*len(RHOS)*len(PRIORS)"))
  testthat::expect_true(contem(src083, "repeat=4"))
  testthat::expect_true(contem(md, "16 cantos"))

  # E08.4: apenas limites Frechet para cinco eventos proxy.
  testthat::expect_true(contem(src084, "proxy_ids=['desocupacao_pct'"))
  testthat::expect_true(contem(src084, "LimiteInferiorFrechet=max(0.,float(probs.sum())-4.)"))
  testthat::expect_true(contem(src084, "LimiteSuperiorFrechet=float(probs.min())"))
  testthat::expect_true(contem(md, "não identificada"))

  # E09 apenas seleciona rho=0 e agrega a faixa dos quatro cenarios E08.2.
  testthat::expect_true(contem(src09, "cenarios = carregar('cenarios_contrastes_e082.csv')"))
  testthat::expect_true(contem(src09, "cenarios['RhoHipotetico'] == 0"))
  testthat::expect_true(contem(src09, "ProbMinCenariosRho=('ProbSentido', 'min')"))
  testthat::expect_true(contem(src09, "ProbMaxCenariosRho=('ProbSentido', 'max')"))
  testthat::expect_true(contem(src09, "assert len(saida) == 26"))
  testthat::expect_true(contem(md, "não posterior temporal identificada"))

  # Os 26 contrastes disponiveis ao publico sao descritivos;
  # nenhum campo probabilistico ou draw consta da projecao.
  d <- utils::read.csv(arquivo_publico, sep = ";",
                       fileEncoding = "UTF-8", check.names = FALSE)
  testthat::expect_equal(nrow(d), 26L)
  testthat::expect_equal(length(unique(d$IndicadorId)), 13L)
  testthat::expect_true(all(table(d$IndicadorId) == 2L))
  testthat::expect_setequal(unique(d$Contraste), c("anual", "recente"))
  testthat::expect_false(any(grepl("^Prob|^theta$|^y_rep$", names(d))))
  testthat::expect_true(all(d$PeriodoDestino == 202609L))
  testthat::expect_true(all(d$PeriodoOrigem == ifelse(
    d$Contraste == "anual", 202509L, 202606L)))
  testthat::expect_true(all(vapply(unique(d$IndicadorId),
                            function(id) contem(md, id), logical(1L))))

  # Cenarios sintéticos: conferir atribuicao de probabilidade de sinal,
  # inversao de direcao, rho de E09 e limites de Frechet;
  # nao se le E04, E07, E08 ou E09 local.
  s0 <- 2
  s1 <- 1
  delta <- 3
  rhos <- c(-0.5, 0, 0.5, 0.9)
  sd <- sqrt(s0^2 + s1^2 - 2 * rhos * s0 * s1)
  p_maior <- stats::pnorm(delta / sd)
  p_menor <- 1 - p_maior
  testthat::expect_equal(p_maior + p_menor, rep(1, 4),
                         tolerance = 1e-12)
  testthat::expect_equal(p_maior[[2]], stats::pnorm(3 / sqrt(5)),
                         tolerance = 1e-12)
  testthat::expect_equal(min(p_maior), p_maior[[1]],
                         tolerance = 1e-12)
  testthat::expect_equal(max(p_maior), p_maior[[4]],
                         tolerance = 1e-12)
  cinco <- c(0.7, 0.8, 0.6, 0.9, 0.75)
  frechet_inf <- max(0, sum(cinco) - length(cinco) + 1)
  frechet_sup <- min(cinco)
  testthat::expect_true(frechet_inf <= frechet_sup)
  testthat::expect_equal(frechet_inf, 0,
                         tolerance = 1e-12)
  testthat::expect_equal(frechet_sup, 0.6,
                         tolerance = 1e-12)

  testthat::expect_true(contem(md, "T03.4"))
  testthat::expect_true(contem(md, "Issue #14"))
  testthat::expect_true(contem(md, "independente"))
  testthat::expect_true(contem(md, "Não contém campos probabilísticos"))
})
