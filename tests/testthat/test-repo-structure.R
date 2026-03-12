testthat::test_that("arquivos e diretórios principais existem", {
  testthat::expect_true(file.exists("README.md"))
  testthat::expect_true(dir.exists("scripts"))
  testthat::expect_true(dir.exists("data/raw"))
  testthat::expect_true(dir.exists("outputs"))
  testthat::expect_true(file.exists("tests/run_tests.R"))
})

testthat::test_that("índices principais documentam RS Seguro", {
  readme_root <- readLines("README.md", warn = FALSE, encoding = "UTF-8")
  readme_scripts <- readLines(file.path("scripts", "README.md"), warn = FALSE, encoding = "UTF-8")

  testthat::expect_true(any(grepl("RS Seguro", readme_root, fixed = TRUE)))
  testthat::expect_true(any(grepl("RS Seguro", readme_scripts, fixed = TRUE)))
  testthat::expect_true(any(grepl("rs_month_macrocrime.csv", readme_root, fixed = TRUE)))
  testthat::expect_true(any(grepl("rs_month_macrocrime_profile_v1_1vict.csv", readme_root, fixed = TRUE)))
})
