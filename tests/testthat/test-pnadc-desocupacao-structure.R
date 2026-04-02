testthat::test_that("PNADC desocupacao esta integrada ao padrao do repositorio", {
  testthat::expect_true(file.exists(file.path("scripts", "pnadc_desocupacao", "pnadc_desocupacao_rstan.R")))
  testthat::expect_true(file.exists(file.path("scripts", "pnadc_desocupacao", "README.md")))
  testthat::expect_true(file.exists(file.path("data", "raw", "pnadc_desocupacao", ".gitkeep")))
  testthat::expect_true(file.exists(file.path("data", "raw", "pnadc_desocupacao", "pnadc_mensal_taxa_desocupacao_6381.csv")))
  testthat::expect_true(file.exists(file.path("data", "raw", "pnadc_desocupacao", "pnadc_trimestral_taxa_desocupacao_por_sexo_4093.csv")))
  testthat::expect_true(file.exists(file.path("data", "raw", "pnadc_desocupacao", "pnadc_mensal_taxa_desocupacao_jan_2026.csv")))

  readme_root <- readLines("README.md", warn = FALSE, encoding = "UTF-8")
  readme_scripts <- readLines(file.path("scripts", "README.md"), warn = FALSE, encoding = "UTF-8")
  readme_data <- readLines(file.path("data", "raw", "README.md"), warn = FALSE, encoding = "UTF-8")
  gitignore <- readLines(".gitignore", warn = FALSE, encoding = "UTF-8")

  testthat::expect_true(any(grepl("PNAD Contínua — desocupação", readme_root, fixed = TRUE)))
  testthat::expect_true(any(grepl("pnadc_desocupacao_rstan.R", readme_scripts, fixed = TRUE)))
  testthat::expect_true(any(grepl("data/raw/pnadc_desocupacao/", readme_data, fixed = TRUE)))
  testthat::expect_false(any(grepl("data/raw/pnadc_desocupacao/\*\*", gitignore)))

  mens_header <- readLines(file.path("data", "raw", "pnadc_desocupacao", "pnadc_mensal_taxa_desocupacao_6381.csv"), n = 1L, warn = FALSE, encoding = "UTF-8")
  sexo_header <- readLines(file.path("data", "raw", "pnadc_desocupacao", "pnadc_trimestral_taxa_desocupacao_por_sexo_4093.csv"), n = 1L, warn = FALSE, encoding = "UTF-8")

  testthat::expect_true(grepl("periodo_codigo", mens_header, fixed = TRUE))
  testthat::expect_true(grepl("taxa_desocupacao", mens_header, fixed = TRUE))
  testthat::expect_true(grepl("sexo", sexo_header, fixed = TRUE))
})
