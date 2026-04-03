testthat::test_that("PMS servicos esta integrada ao padrao do repositorio", {
  testthat::expect_true(file.exists(file.path("scripts", "pms_servicos", "pms_servicos_rstan.R")))
  testthat::expect_true(file.exists(file.path("scripts", "pms_servicos", "README.md")))
  testthat::expect_true(file.exists(file.path("data", "raw", "pms_servicos", "pms_base_analitica_stan.csv")))
  testthat::expect_true(file.exists(file.path("data", "raw", "pms_servicos", "README.md")))

  readme_root <- readLines("README.md", warn = FALSE, encoding = "UTF-8")
  readme_scripts <- readLines(file.path("scripts", "README.md"), warn = FALSE, encoding = "UTF-8")
  readme_data <- readLines(file.path("data", "raw", "README.md"), warn = FALSE, encoding = "UTF-8")
  gitignore <- readLines(".gitignore", warn = FALSE, encoding = "UTF-8")

  testthat::expect_true(any(grepl("PMS / Serviços em janeiro de 2026", readme_root, fixed = TRUE)))
  testthat::expect_true(any(grepl("pms_servicos_rstan.R", readme_scripts, fixed = TRUE)))
  testthat::expect_true(any(grepl("data/raw/pms_servicos/", readme_data, fixed = TRUE)))
  testthat::expect_false(any(grepl("data/raw/pms_servicos/\*\*", gitignore)))

  csv_header <- readLines(
    file.path("data", "raw", "pms_servicos", "pms_base_analitica_stan.csv"),
    n = 1L,
    warn = FALSE,
    encoding = "UTF-8"
  )

  testthat::expect_true(grepl("serie_id", csv_header, fixed = TRUE))
  testthat::expect_true(grepl("indice_sa", csv_header, fixed = TRUE))
  testthat::expect_true(grepl("variacao_mm1_sa", csv_header, fixed = TRUE))
})
