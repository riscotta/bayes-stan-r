testthat::test_that("SAT está integrado ao padrão do repositório", {
  testthat::expect_true(file.exists(file.path("scripts", "sat_model_selection", "sat_stan_model_selection_rstan.R")))
  testthat::expect_true(file.exists(file.path("scripts", "sat_model_selection", "README.md")))

  readme_root <- readLines("README.md", warn = FALSE, encoding = "UTF-8")
  readme_scripts <- readLines(file.path("scripts", "README.md"), warn = FALSE, encoding = "UTF-8")
  readme_data <- readLines(file.path("data", "raw", "README.md"), warn = FALSE, encoding = "UTF-8")
  install_deps <- readLines(file.path("scripts", "_setup", "install_deps.R"), warn = FALSE, encoding = "UTF-8")

  testthat::expect_true(any(grepl("SAT com seleção Bayesiana de variáveis", readme_root, fixed = TRUE)))
  testthat::expect_true(any(grepl("sat_stan_model_selection_rstan.R", readme_scripts, fixed = TRUE)))
  testthat::expect_true(any(grepl("SAT.csv", readme_data, fixed = TRUE)))
  testthat::expect_true(any(grepl('"rstan"', install_deps, fixed = TRUE)))
  testthat::expect_true(any(grepl('"bridgesampling"', install_deps, fixed = TRUE)))
})
