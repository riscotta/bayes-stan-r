testthat::test_that("Exercises Dataset: arquivos principais existem", {
  testthat::expect_true(file.exists(file.path("scripts", "exercises_dataset", "exercises_dataset_cmdstanr.R")))
  testthat::expect_true(file.exists(file.path("scripts", "exercises_dataset", "modelo_ordinal_dificuldade_sem_equipamento.stan")))
  testthat::expect_true(file.exists(file.path("scripts", "exercises_dataset", "modelo_ordinal_dificuldade_com_equipamento_diagnostico.stan")))
  testthat::expect_true(file.exists(file.path("scripts", "exercises_dataset", "modelo_softmax_dificuldade_sensibilidade.stan")))
  testthat::expect_true(file.exists(file.path("data", "raw", "exercises_dataset", "final_exercise_dataset.csv")))
})

testthat::test_that("Exercises Dataset: índices documentam o estudo", {
  readme_root <- readLines("README.md", warn = FALSE, encoding = "UTF-8")
  readme_scripts <- readLines(file.path("scripts", "README.md"), warn = FALSE, encoding = "UTF-8")
  readme_raw <- readLines(file.path("data", "raw", "README.md"), warn = FALSE, encoding = "UTF-8")

  testthat::expect_true(any(grepl("Exercises Dataset", readme_root, fixed = TRUE)))
  testthat::expect_true(any(grepl("exercises_dataset_cmdstanr.R", readme_scripts, fixed = TRUE)))
  testthat::expect_true(any(grepl("data/raw/exercises_dataset/", readme_raw, fixed = TRUE)))
})
