testthat::test_that("Censo Escolar esta integrado ao padrao do repositorio", {
  testthat::expect_true(file.exists(file.path("scripts", "censo_escolar_tempo_integral", "censo_escolar_tempo_integral_cmdstanr.R")))
  testthat::expect_true(file.exists(file.path("scripts", "censo_escolar_tempo_integral", "README.md")))
  testthat::expect_true(file.exists(file.path("data", "raw", "censo_escolar", ".gitkeep")))

  readme_root <- readLines("README.md", warn = FALSE, encoding = "UTF-8")
  readme_scripts <- readLines(file.path("scripts", "README.md"), warn = FALSE, encoding = "UTF-8")
  readme_data <- readLines(file.path("data", "raw", "README.md"), warn = FALSE, encoding = "UTF-8")
  gitignore <- readLines(".gitignore", warn = FALSE, encoding = "UTF-8")

  testthat::expect_true(any(grepl("Censo Escolar 2021-2025", readme_root, fixed = TRUE)))
  testthat::expect_true(any(grepl("censo_escolar_tempo_integral_cmdstanr.R", readme_scripts, fixed = TRUE)))
  testthat::expect_true(any(grepl("data/raw/censo_escolar/", readme_data, fixed = TRUE)))
  testthat::expect_true(any(grepl("data/raw/censo_escolar/\*\*", gitignore)))
})
