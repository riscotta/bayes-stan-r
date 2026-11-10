# T04.6 — verificações da matriz E10: estrutural/descritiva; sem inferência histórica.
documento_t46 <- file.path("reports", "pnadc_estudo_i",
                           "e10_matriz_evidencias_reproducao_T04_6.md")
arquivo_t46 <- file.path("data", "raw", "pnadc_estudo_i",
                         "resultados_auditados", "contrastes_descritos_e09.csv")
ler_t46 <- function(p) readLines(p, warn = FALSE, encoding = "UTF-8")
texto_t46 <- function(p) paste(ler_t46(p), collapse = "\n")
celulas_t46 <- function(s) {
  x <- trimws(strsplit(s, "|", fixed = TRUE)[[1]])
  gsub("`", "", x[2:7], fixed = TRUE)
}

testthat::test_that("T04.6: rastreabilidade E00–E09 explicita estagios e limites", {
  doc <- ler_t46(documento_t46)
  linhas <- grep("^\\| (E0[0-9]|E08\\.[1-4]) \\|", doc, value = TRUE)
  ids <- vapply(linhas, function(s) trimws(strsplit(s, "|", fixed = TRUE)[[1]][2]),
                character(1))
  testthat::expect_identical(unname(ids),
    c(sprintf("E%02d", 0:7), paste0("E08.", 1:4), "E09"))
  texto <- texto_t46(documento_t46)
  for (nivel in c("N1 — evidência estrutural",
                  "N2 — reprodução descritiva",
                  "N3 — reprodução inferencial",
                  "N4 — reprodução integral")) {
    testthat::expect_true(grepl(nivel, texto, fixed = TRUE), info = nivel)
  }
  for (termo in c("RECONSTRUCAO_EDITORIAL", "N3_BLOQUEADO_INSUMOS",
                  "N4_BLOQUEADO_ORIGEM_GOVERNANCA", "T04.7 não executada",
                  "T01.3–T01.7", "D00-01–D00-06", "T04.4 R1–R4",
                  "T04.5 R1–R3", "QA ☐", "NAO_REPRODUZIDO",
                  "DESCRITIVO_APENAS", "SEM_PROBABILIDADE_VALIDADA",
                  "52 cadeias", "104", "E08.1 → E08.2",
                  "não veto por PPC", "cinco proxies")) {
    testthat::expect_true(grepl(termo, texto, fixed = TRUE), info = termo)
  }
  testthat::expect_true(grepl(
    "python scripts/pnadc_estudo_i/auditar_e10.py", texto, fixed = TRUE))
  testthat::expect_true(grepl("Rscript tests/run_tests.R", texto, fixed = TRUE))
})

testthat::test_that("T04.6: todas as 26 linhas E10 coincidem com CSV historico", {
  doc <- ler_t46(documento_t46)
  linhas <- grep("^\\| T46-[0-9]{2} \\|", doc, value = TRUE)
  testthat::expect_length(linhas, 26L)
  tab <- do.call(rbind, lapply(linhas, celulas_t46))
  testthat::expect_equal(ncol(tab), 6L)
  testthat::expect_identical(tab[, 1], sprintf("T46-%02d", 1:26))
  testthat::expect_equal(length(unique(paste(tab[, 3], tab[, 4]))), 26L)
  pub <- utils::read.csv(arquivo_t46, sep = ";", fileEncoding = "UTF-8",
                         check.names = FALSE, stringsAsFactors = FALSE)
  testthat::expect_equal(nrow(pub), 26L)
  testthat::expect_identical(tab[, 2], pub$Eixo)
  testthat::expect_identical(tab[, 3], pub$IndicadorId)
  testthat::expect_identical(tab[, 4], pub$Contraste)
  testthat::expect_equal(as.numeric(tab[, 5]), pub$Delta, tolerance = 1e-7)
  testthat::expect_identical(tab[, 6], pub$SituacaoOficialIBGE)
  testthat::expect_equal(as.integer(table(pub$Eixo)), c(4L, 8L, 8L, 4L, 2L))
  testthat::expect_identical(as.integer(table(pub$Contraste)), c(13L, 13L))
  testthat::expect_true(all(pub$PeriodoDestino == 202609L))
  testthat::expect_true(all(pub$PeriodoOrigem ==
    ifelse(pub$Contraste == "anual", 202509L, 202606L)))
  testthat::expect_lt(max(abs(pub$ValorDestino - pub$ValorOrigem -
                               pub$Delta)), 1e-7)
  testthat::expect_false(any(c("ProbSentido", "PPC", "RhoHipotetico",
                               "theta", "y_rep", "IC95Inf", "IC95Sup") %in%
                             names(pub)))
})

testthat::test_that("T04.6: fontes e Git blobs vinculados sao identicos", {
  fontes <- c(
    "reports/pnadc_estudo_i/e00_consolidado_T04_1.md" =
      "77141c81a2df955b57b8af5970fc75b2c8f4ba04",
    "reports/pnadc_estudo_i/contratos_e01_e05_T04_2.md" =
      "3e87c809d8e454f212d5f401a52ccdb2f64d9b62",
    "reports/pnadc_estudo_i/e06_especificacao_modelos_T04_3.md" =
      "772b94db51d72d1c8412749c5339569f92a4233c",
    "reports/pnadc_estudo_i/ponte_e06_e07_T04_4.md" =
      "59865b476b987d29d7dd0f746c275d9f27db0726",
    "reports/pnadc_estudo_i/ponte_e08_e09_T04_5.md" =
      "9c412d11551246412fc83fa09d52e0d5ef6eeea9",
    "reports/pnadc_estudo_i/nota_cientifica.md" =
      "7f1dea32b2742d1f38b029bf2e92a9e7e060ef1f",
    "scripts/pnadc_estudo_i/auditar_e10.py" =
      "a8aa628b0cad4321811acd283819203c69b493f7",
    "data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv" =
      "096b9e2d42c8fe1231ac675dc48cfc61dfa294d2"
  )
  doc <- texto_t46(documento_t46)
  for (caminho in names(fontes)) {
    testthat::expect_true(file.exists(caminho), info = caminho)
    testthat::expect_true(grepl(fontes[[caminho]], doc, fixed = TRUE),
                          info = caminho)
    h <- system2("git", c("hash-object", shQuote(caminho)),
                 stdout = TRUE, stderr = TRUE)
    testthat::expect_length(h, 1L)
    testthat::expect_identical(trimws(h), unname(fontes[[caminho]]))
  }
  script <- texto_t46("scripts/pnadc_estudo_i/auditar_e10.py")
  testthat::expect_true(grepl(
    "8bd38ce75917d981431f4c2a9c09ba73648ff1151fd6ac717068637b682fe120",
    script, fixed = TRUE))
  testthat::expect_true(grepl(
    "8bd38ce75917d981431f4c2a9c09ba73648ff1151fd6ac717068637b682fe120",
    doc, fixed = TRUE))
})

testthat::test_that("T04.6: auditoria descritiva Python opcional, quando presente", {
  py <- unname(Sys.which("python3"))
  if (!nzchar(py)) py <- unname(Sys.which("python"))
  if (!nzchar(py)) testthat::skip("Python 3 ausente: somente N1 foi executado")
  saida <- suppressWarnings(system2(
    py, "scripts/pnadc_estudo_i/auditar_e10.py",
    stdout = TRUE, stderr = TRUE))
  status <- attr(saida, "status")
  if (is.null(status)) status <- 0L
  testthat::expect_identical(as.integer(status), 0L)
  testthat::expect_true(any(grepl(
    "AUDITORIA DE SÍNTESE: APROVADA", saida, fixed = TRUE)))
  testthat::expect_true(any(grepl(
    "não reproduz coleta", saida, fixed = TRUE)))
})

testthat::test_that("T04.6: README indexa E10 sem declarar homologacao", {
  readme <- texto_t46(file.path("scripts", "pnadc_estudo_i", "README.md"))
  testthat::expect_true(grepl(
    "e10_matriz_evidencias_reproducao_T04_6.md", readme, fixed = TRUE))
  testthat::expect_true(grepl("T04.6", readme, fixed = TRUE))
  testthat::expect_true(grepl("N1", readme, fixed = TRUE))
  testthat::expect_true(grepl("N4", readme, fixed = TRUE))
})
