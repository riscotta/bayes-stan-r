testthat::test_that("Sherlock Holmes: arquivos principais existem", {
  testthat::expect_true(file.exists(file.path("scripts", "sherlock_holmes_terms", "sherlock_holmes_terms_cmdstanr.R")))
  testthat::expect_true(file.exists(file.path("scripts", "sherlock_holmes_terms", "modelo_operacional_poisson_lognormal_e07.stan")))
  testthat::expect_true(file.exists(file.path("data", "raw", "sherlock_holmes_terms", "12_Sherlock_Holmes_Texts.csv")))
  testthat::expect_true(file.exists(file.path("data", "raw", "sherlock_holmes_terms", "model_inputs", "dados_modelo_obra_termo_e07.csv")))
  testthat::expect_true(file.exists(file.path("data", "raw", "sherlock_holmes_terms", "model_inputs", "dados_stan_e07_entrada.json")))
  testthat::expect_true(file.exists(file.path("data", "raw", "sherlock_holmes_terms", "tabelas_finais_auditadas", "tabela_final_taxas_globais_termos.csv")))
})

testthat::test_that("Sherlock Holmes: índices documentam o estudo", {
  readme_root <- readLines("README.md", warn = FALSE, encoding = "UTF-8")
  readme_scripts <- readLines(file.path("scripts", "README.md"), warn = FALSE, encoding = "UTF-8")
  readme_raw <- readLines(file.path("data", "raw", "README.md"), warn = FALSE, encoding = "UTF-8")

  testthat::expect_true(any(grepl("Sherlock Holmes", readme_root, fixed = TRUE)))
  testthat::expect_true(any(grepl("sherlock_holmes_terms_cmdstanr.R", readme_scripts, fixed = TRUE)))
  testthat::expect_true(any(grepl("data/raw/sherlock_holmes_terms/", readme_raw, fixed = TRUE)))
})
