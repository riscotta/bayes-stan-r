testthat::test_that("DETER mensal está integrado ao padrão do repositório", {
  testthat::expect_true(file.exists(file.path("scripts", "deter_mensal_bioma_uf", "deter_mensal_bioma_uf_cmdstanr.R")))
  testthat::expect_true(file.exists(file.path("scripts", "deter_mensal_bioma_uf", "README.md")))

  readme_root <- readLines("README.md", warn = FALSE, encoding = "UTF-8")
  readme_scripts <- readLines(file.path("scripts", "README.md"), warn = FALSE, encoding = "UTF-8")
  readme_data <- readLines(file.path("data", "raw", "README.md"), warn = FALSE, encoding = "UTF-8")

  testthat::expect_true(any(grepl("DETER mensal por bioma-UF", readme_root, fixed = TRUE)))
  testthat::expect_true(any(grepl("deter_mensal_bioma_uf_cmdstanr.R", readme_scripts, fixed = TRUE)))
  testthat::expect_true(any(grepl("deter_mensal_bioma_uf.csv", readme_data, fixed = TRUE)))
})
