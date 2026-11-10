# T05.1: completude documental de scripts e testes, sem executar estudos.
testthat::test_that("T05.1: inventarios cobrem universo estatico por caminho", {
  path_scripts <- "reports/t05_1/inventario_scripts.csv"
  path_tests <- "reports/t05_1/inventario_testes.csv"
  testthat::expect_true(file.exists(path_scripts))
  testthat::expect_true(file.exists(path_tests))
  a <- utils::read.csv(path_scripts, sep = ";", stringsAsFactors = FALSE,
                       check.names = FALSE, fileEncoding = "UTF-8")
  b <- utils::read.csv(path_tests, sep = ";", stringsAsFactors = FALSE,
                       check.names = FALSE, fileEncoding = "UTF-8")
  fonte <- sort(list.files("scripts", pattern = "\\.(R|py|stan|ps1)$",
                            recursive = TRUE, full.names = TRUE))
  suite <- sort(list.files("tests/testthat", pattern = "^test-.*\\.R$",
                            recursive = TRUE, full.names = TRUE))
  testthat::expect_identical(sort(a$caminho), fonte)
  testthat::expect_identical(sort(b$arquivo), suite)
  testthat::expect_false(anyDuplicated(a$caminho) > 0L)
  testthat::expect_false(anyDuplicated(b$arquivo) > 0L)
  testthat::expect_true(all(nzchar(a$linguagem)))
  testthat::expect_true(all(nzchar(a$unidade)))
  testthat::expect_true(all(nzchar(a$estado_CI_referencia_42827479741)))
  testthat::expect_true(all(nzchar(a$dependencia_externa_ou_pendente)))
  testthat::expect_true(all(nzchar(b$tipo_predominante_estatico)))
  testthat::expect_identical(sum(a$linguagem == "Stan"), 20L)
  testthat::expect_identical(sum(a$linguagem == "Python"), 10L)
  testthat::expect_identical(sum(a$linguagem == "PowerShell"), 2L)
  testthat::expect_identical(sum(a$linguagem == "R"), 47L)
  py <- a[a$caminho == "scripts/pnadc_estudo_i/auditar_e10.py", ]
  testthat::expect_identical(nrow(py), 1L)
  testthat::expect_identical(py$estado_CI_referencia_42827479741,
                             "OPCIONAL_SKIP_CI_main_sem_Python3")
  stan <- a[a$linguagem == "Stan", ]
  testthat::expect_true(all(stan$estado_CI_referencia_42827479741 ==
                             "NAO_compilado_na_CI"))
})
