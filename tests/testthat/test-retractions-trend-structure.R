testthat::test_that("Tendencia temporal das retratacoes esta integrada ao padrao do repositorio", {
  testthat::expect_true(file.exists(file.path("scripts", "retractions_trend", "retractions_trend_rstan.R")))
  testthat::expect_true(file.exists(file.path("scripts", "retractions_trend", "README.md")))
  testthat::expect_true(file.exists(file.path("data", "raw", "retractions_time_to_retraction", "README.md")))

  readme_root <- readLines("README.md", warn = FALSE, encoding = "UTF-8")
  readme_scripts <- readLines(file.path("scripts", "README.md"), warn = FALSE, encoding = "UTF-8")
  readme_data <- readLines(file.path("data", "raw", "README.md"), warn = FALSE, encoding = "UTF-8")
  install_deps <- readLines(file.path("scripts", "_setup", "install_deps.R"), warn = FALSE, encoding = "UTF-8")

  testthat::expect_true(any(grepl("tendência temporal", tolower(readme_root), fixed = TRUE)))
  testthat::expect_true(any(grepl("retractions_trend_rstan.R", readme_scripts, fixed = TRUE)))
  testthat::expect_true(any(grepl("scripts/retractions_trend/retractions_trend_rstan.R", readme_data, fixed = TRUE)))
  testthat::expect_true(any(grepl('"forcats"', install_deps, fixed = TRUE)))
})
