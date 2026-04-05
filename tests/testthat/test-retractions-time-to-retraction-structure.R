testthat::test_that("Retratacoes cientificas esta integrado ao padrao do repositorio", {
  testthat::expect_true(file.exists(file.path("scripts", "retractions_time_to_retraction", "retractions_time_to_retraction_cmdstanr.R")))
  testthat::expect_true(file.exists(file.path("scripts", "retractions_time_to_retraction", "README.md")))
  testthat::expect_true(file.exists(file.path("data", "raw", "retractions_time_to_retraction", "README.md")))

  readme_root <- readLines("README.md", warn = FALSE, encoding = "UTF-8")
  readme_scripts <- readLines(file.path("scripts", "README.md"), warn = FALSE, encoding = "UTF-8")
  readme_data <- readLines(file.path("data", "raw", "README.md"), warn = FALSE, encoding = "UTF-8")

  testthat::expect_true(any(grepl("retratacoes cientificas", tolower(readme_root), fixed = TRUE)))
  testthat::expect_true(any(grepl("retractions_time_to_retraction_cmdstanr.R", readme_scripts, fixed = TRUE)))
  testthat::expect_true(any(grepl("global_scientific_retractions_1927_2026.csv", readme_data, fixed = TRUE)))
})
