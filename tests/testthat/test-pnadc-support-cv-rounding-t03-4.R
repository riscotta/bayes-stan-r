testthat::test_that("T03.4: fontes, suporte e riscos metodologicos documentados", {
  raiz <- file.path("scripts", "pnadc_estudo_i", "estudo_I")
  relatorio <- file.path("reports", "pnadc_estudo_i",
                         "auditoria_suporte_cv_arredondamento_T03_4.md")
  arq <- c(relatorio,
           file.path(raiz, "e04", "construir_base_e04.py"),
           file.path(raiz, "e07", "stan", "taxa_marginal.stan"),
           file.path(raiz, "e07", "stan", "contagem_marginal.stan"),
           file.path(raiz, "e07", "stan", "rendimento_marginal.stan"),
           file.path(raiz, "e08", "sensibilidade_e082.py"),
           file.path(raiz, "e08", "sensibilidade_e083.py"),
           file.path("data", "raw", "pnadc_estudo_i",
                     "resultados_auditados", "contrastes_descritos_e09.csv"))
  testthat::expect_true(all(file.exists(arq)))
  ler <- function(caminho) paste(readLines(caminho, warn = FALSE,
                                           encoding = "UTF-8"), collapse = "\n")
  m <- ler(relatorio)
  e04 <- ler(arq[[2]])
  taxa <- ler(arq[[3]])
  cont <- ler(arq[[4]])
  rend <- ler(arq[[5]])
  e082 <- ler(arq[[6]])
  e083 <- ler(arq[[7]])
  tem <- function(x, s) grepl(s, x, fixed = TRUE)

  # Suportes dos parametros Stan e ausencia de truncamento na Normal E08.2.
  testthat::expect_true(tem(taxa, "real<lower=0, upper=1> p;"))
  testthat::expect_true(tem(taxa, "theta = 100 * p"))
  testthat::expect_true(tem(cont, "real<lower=0, upper=1> q;"))
  testthat::expect_true(tem(cont, "populacao_14mais_mil * q"))
  testthat::expect_true(tem(rend, "real<lower=0> mu;"))
  testthat::expect_true(tem(e082, "prob_positive = float(norm.cdf(delta/sd))"))
  testthat::expect_true(tem(m, "Divergência metodológica"))
  # Apenas possibilidade sintetica de massa fora do suporte, nao resultado PNAD.
  testthat::expect_gt(stats::pnorm(0, mean = 0.2, sd = 1), 0.42)
  testthat::expect_gt(stats::pnorm(150, mean = 148, sd = 4,
                                   lower.tail = FALSE), 0.30)

  # CV observado em escala percentual; nenhum coeficiente de correlacao identificado.
  testthat::expect_true(tem(e04, "CVOriginal"))
  testthat::expect_true(tem(e04, "CVPercentual"))
  testthat::expect_true(tem(e082, "df.CVPercentual>0"))
  testthat::expect_true(tem(e082, "df['s'] = df.Valor.abs()*df.CVPercentual/100"))
  testthat::expect_equal(abs(0) * 8 / 100, 0) # CV>0 nao impede s=0
  testthat::expect_true(tem(e082, "RHOS = (-0.5, 0.0, 0.5, 0.9)"))
  testthat::expect_true(tem(e082, "s0*s0+s1*s1-2*rho*s0*s1"))
  testthat::expect_true(tem(m, "não é estimado"))

  # Arredondamento: largura do intervalo da diferenca de agregados.
  delta_intervalo <- function(y0, y1, h0, h1) {
    d <- y1 - y0
    c(d - h0 - h1, d + h0 + h1)
  }
  testthat::expect_equal(delta_intervalo(5.2, 5.3, 0.05, 0.05),
                         c(0, 0.2), tolerance = 1e-12)
  testthat::expect_equal(delta_intervalo(10, 10, 0.5, 0.5),
                         c(-1, 1), tolerance = 1e-12)
  testthat::expect_true(tem(e083, "repeat=4"))
  testthat::expect_true(tem(e083, "errc=0.05"))
  testthat::expect_true(tem(e083, "erry=0.05 if r1.tipo=='taxa' else 0.5"))
  testthat::expect_true(tem(m, "16 cantos"))

  # Os trimestres sao disjuntos no calendario, nao se testa independencia amostral.
  meses <- function(aaaamm) {
    ano <- as.integer(aaaamm) %/% 100L
    mes <- as.integer(aaaamm) %% 100L
    ano * 12L + mes - (2:0)
  }
  testthat::expect_length(intersect(meses(202509), meses(202609)), 0L)
  testthat::expect_length(intersect(meses(202606), meses(202609)), 0L)
  testthat::expect_equal(length(meses(202609)), 3L)
  testthat::expect_true(tem(e04, "InicioJanela"))
  testthat::expect_true(tem(m, "1-2(5)"))
  testthat::expect_true(tem(m, "não compartilham meses"))

  # A projecao publicada nao fornece CVs nem draws para a auditoria numerica.
  d <- utils::read.csv(arq[[8]], sep = ";", fileEncoding = "UTF-8",
                       check.names = FALSE)
  testthat::expect_equal(nrow(d), 26L)
  testthat::expect_equal(length(unique(d$IndicadorId)), 13L)
  testthat::expect_false(any(c("CVPercentual", "CVOrigem", "CVDestino",
                                "Covariancia", "RhoEstimado", "theta", "y_rep")
                              %in% names(d)))
  testthat::expect_true(tem(m, "T03.5"))
  testthat::expect_true(tem(m, "T03.6"))
  testthat::expect_true(tem(m, "T03.7"))
  testthat::expect_true(tem(m, "Issue #14"))
  testthat::expect_true(tem(m, "independente"))
})
