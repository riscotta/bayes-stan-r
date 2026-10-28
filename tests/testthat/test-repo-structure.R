testthat::test_that("arquivos e diretórios principais existem", {
  testthat::expect_true(file.exists("README.md"))
  testthat::expect_true(dir.exists("scripts"))
  testthat::expect_true(dir.exists("data/raw"))
  testthat::expect_true(dir.exists("outputs"))
  testthat::expect_true(file.exists("tests/run_tests.R"))
})

testthat::test_that("o catálogo documenta RS Seguro e os dados correspondentes", {
  root <- paste(readLines("README.md", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  scripts <- paste(readLines("scripts/README.md", warn = FALSE, encoding = "UTF-8"),
                   collapse = "\n")
  testthat::expect_match(root, "scripts/README.md", fixed = TRUE)
  testthat::expect_match(scripts, "RS Seguro", fixed = TRUE)
  testthat::expect_match(scripts, "rs_month_macrocrime.csv", fixed = TRUE)
  testthat::expect_match(scripts, "rs_month_macrocrime_profile_v1_1vict.csv", fixed = TRUE)
  testthat::expect_true(file.exists("data/raw/rs_seguro/rs_month_macrocrime.csv"))
  testthat::expect_true(file.exists("data/raw/rs_seguro/rs_month_macrocrime_profile_v1_1vict.csv"))
})
