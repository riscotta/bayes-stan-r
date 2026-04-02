testthat::test_that("FruitFlies esta integrado ao padrao do repositorio", {
  testthat::expect_true(file.exists(file.path("scripts", "fruitflies_aft", "fruitflies_aft_lognormal_cmdstanr.R")))
  testthat::expect_true(file.exists(file.path("scripts", "fruitflies_aft", "README.md")))

  readme_root <- readLines("README.md", warn = FALSE, encoding = "UTF-8")
  readme_scripts <- readLines(file.path("scripts", "README.md"), warn = FALSE, encoding = "UTF-8")
  deps_setup <- readLines(file.path("scripts", "_setup", "install_deps.R"), warn = FALSE, encoding = "UTF-8")

  testthat::expect_true(any(grepl("FruitFlies", readme_root, fixed = TRUE)))
  testthat::expect_true(any(grepl("fruitflies_aft_lognormal_cmdstanr.R", readme_scripts, fixed = TRUE)))
  testthat::expect_true(any(grepl("Stat2Data", deps_setup, fixed = TRUE)))
})
