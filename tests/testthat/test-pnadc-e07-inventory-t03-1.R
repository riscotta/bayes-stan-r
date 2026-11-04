testthat::test_that("T03.1: inventario E07 separa 13 marginais e 26 contrastes", {
  base <- file.path("scripts", "pnadc_estudo_i", "estudo_I", "e07")
  inventario <- file.path("reports", "pnadc_estudo_i", "inventario_estimandos_e07_T03_1.md")
  contrastes <- file.path("data", "raw", "pnadc_estudo_i",
                          "resultados_auditados", "contrastes_descritos_e09.csv")
  testthat::expect_true(file.exists(inventario))
  testthat::expect_true(file.exists(contrastes))
  md <- readLines(inventario, warn = FALSE, encoding = "UTF-8")
  linhas <- grep("^\\| [A-E] \\|", md, value = TRUE)
  partes <- strsplit(linhas, "|", fixed = TRUE)
  testthat::expect_length(partes, 13L)
  id <- vapply(partes, function(p) trimws(gsub(intToUtf8(96), "", p[[3L]], fixed = TRUE)), "")
  eixo <- vapply(partes, function(p) trimws(p[[2L]]), "")
  tipo <- vapply(partes, function(p) trimws(p[[4L]]), "")
  testthat::expect_equal(length(unique(id)), 13L)
  testthat::expect_equal(unname(as.integer(table(factor(eixo, levels = LETTERS[1:5])))),
                         c(2L, 4L, 4L, 2L, 1L))
  testthat::expect_equal(as.integer(table(factor(tipo,
                             levels = c("taxa", "contagem", "rendimento")))),
                         c(8L, 4L, 1L))

  d <- utils::read.csv(contrastes, sep = ";", check.names = FALSE,
                       fileEncoding = "UTF-8")
  testthat::expect_equal(nrow(d), 26L)
  testthat::expect_setequal(id, unique(d$IndicadorId))
  testthat::expect_true(all(table(d$IndicadorId) == 2L))
  testthat::expect_setequal(unique(d$Contraste), c("anual", "recente"))
  testthat::expect_true(all(d$PeriodoDestino == 202609L))
  testthat::expect_true(all(d$PeriodoOrigem ==
                ifelse(d$Contraste == "anual", 202509L, 202606L)))

  expected_type <- ifelse(grepl("_pct$", id), "taxa",
                          ifelse(grepl("_mil$", id), "contagem", "rendimento"))
  testthat::expect_identical(tipo, expected_type)
  testthat::expect_false("populacao_14mais_mil" %in% id)
  r <- readLines(file.path(base, "R", "ajustar_marginais_e07.R"), warn = FALSE)
  testthat::expect_true(any(grepl("Periodo == '202609' & Papel == 'principal'",
                                r, fixed = TRUE)))
  testthat::expect_true(any(grepl("IndicadorId == 'desocupacao_pct'", r, fixed = TRUE)))
  testthat::expect_true(any(grepl("fit$summary(variables = 'theta')", r, fixed = TRUE)))
  testthat::expect_true(any(grepl("resumo_marginais.csv", r, fixed = TRUE)))
  testthat::expect_true(any(grepl("s <- abs(y) * cv / 100", r, fixed = TRUE)))

  for (stan in c("taxa_marginal.stan", "contagem_marginal.stan",
                 "rendimento_marginal.stan")) {
    s <- readLines(file.path(base, "stan", stan), warn = FALSE)
    testthat::expect_true(any(grepl("generated quantities", s, fixed = TRUE)))
    testthat::expect_true(any(grepl("real theta =", s, fixed = TRUE)))
    testthat::expect_true(any(grepl("real y_rep =", s, fixed = TRUE)))
  }
  testthat::expect_true(any(grepl("não", md, fixed = TRUE)))
  testthat::expect_true(any(grepl("T03.2", md, fixed = TRUE)))
  testthat::expect_true(any(grepl("CI verde isolada", md, fixed = TRUE)))
})
