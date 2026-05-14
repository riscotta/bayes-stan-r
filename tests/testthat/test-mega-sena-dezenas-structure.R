testthat::test_that("Mega-Sena: arquivos principais existem", {
  testthat::expect_true(file.exists(file.path("scripts", "mega_sena_dezenas", "mega_sena_dezenas_cmdstanr.R")))
  testthat::expect_true(file.exists(file.path("scripts", "mega_sena_dezenas", "modelo_loglinear_multinomial_fator.stan")))
  testthat::expect_true(file.exists(file.path("scripts", "mega_sena_dezenas", "modelo_multinomial_global.stan")))
  testthat::expect_true(file.exists(file.path("data", "raw", "mega_sena_dezenas", "base_analitica.csv")))
  testthat::expect_true(file.exists(file.path("data", "raw", "mega_sena_dezenas", "tabelas_auditadas", "E08_resumo_fatores_stan.csv")))
  testthat::expect_true(file.exists(file.path("data", "raw", "mega_sena_dezenas", "diagnosticos_auditados", "E08_diagnosticos_mcmc_auditados.csv")))
})

testthat::test_that("Mega-Sena: índices documentam o estudo", {
  readme_root <- readLines("README.md", warn = FALSE, encoding = "UTF-8")
  readme_scripts <- readLines(file.path("scripts", "README.md"), warn = FALSE, encoding = "UTF-8")
  readme_raw <- readLines(file.path("data", "raw", "README.md"), warn = FALSE, encoding = "UTF-8")

  testthat::expect_true(any(grepl("Mega-Sena", readme_root, fixed = TRUE)))
  testthat::expect_true(any(grepl("mega_sena_dezenas_cmdstanr.R", readme_scripts, fixed = TRUE)))
  testthat::expect_true(any(grepl("data/raw/mega_sena_dezenas/", readme_raw, fixed = TRUE)))
})
