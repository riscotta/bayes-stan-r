testthat::test_that("Missing Migrants: arquivos principais existem", {
  testthat::expect_true(file.exists(file.path("scripts", "missing_migrants", "missing_migrants_cmdstanr.R")))
  testthat::expect_true(file.exists(file.path("scripts", "missing_migrants", "modelo_nb_hierarquico_rota_mes_estavel.stan")))
  testthat::expect_true(file.exists(file.path("data", "raw", "missing_migrants", "Missing_Migrants_Global_Figures_allData.csv")))
  testthat::expect_true(file.exists(file.path("data", "raw", "missing_migrants", "model_inputs", "E06_dados_modelo_rota_mes.csv")))
  testthat::expect_true(file.exists(file.path("data", "raw", "missing_migrants", "model_inputs", "E06_stan_data_rota_mes.json")))
  testthat::expect_true(file.exists(file.path("data", "raw", "missing_migrants", "resultados_auditados", "E09_sintese_posterior_rotas_cmdstan.csv")))
  testthat::expect_true(file.exists(file.path("data", "raw", "missing_migrants", "resultados_auditados", "E08_resumo_diagnostico_mcmc.csv")))
})

testthat::test_that("Missing Migrants: índices documentam o estudo", {
  readme_root <- readLines("README.md", warn = FALSE, encoding = "UTF-8")
  readme_scripts <- readLines(file.path("scripts", "README.md"), warn = FALSE, encoding = "UTF-8")
  readme_raw <- readLines(file.path("data", "raw", "README.md"), warn = FALSE, encoding = "UTF-8")

  testthat::expect_true(any(grepl("IOM Missing Migrants Project", readme_root, fixed = TRUE)))
  testthat::expect_true(any(grepl("missing_migrants_cmdstanr.R", readme_scripts, fixed = TRUE)))
  testthat::expect_true(any(grepl("data/raw/missing_migrants/", readme_raw, fixed = TRUE)))
})
