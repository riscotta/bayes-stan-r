testthat::test_that("datasets versionados do RS Seguro existem", {
  testthat::expect_true(file.exists(file.path("data", "raw", "rs_seguro", "rs_month_macrocrime.csv")))
  testthat::expect_true(file.exists(file.path("data", "raw", "rs_seguro", "rs_month_macrocrime_profile_v1_1vict.csv")))
})

testthat::test_that("dataset macrocrime tem colunas mínimas esperadas", {
  x <- utils::read.csv(
    file.path("data", "raw", "rs_seguro", "rs_month_macrocrime.csv"),
    stringsAsFactors = FALSE
  )

  testthat::expect_true(nrow(x) > 0)
  testthat::expect_true(all(c("ym", "crime", "occ", "vit") %in% names(x)))
})

testthat::test_that("dataset de perfil da vítima tem colunas mínimas esperadas", {
  x <- utils::read.csv(
    file.path("data", "raw", "rs_seguro", "rs_month_macrocrime_profile_v1_1vict.csv"),
    stringsAsFactors = FALSE
  )

  testthat::expect_true(nrow(x) > 0)
  testthat::expect_true(all(c("ym", "crime", "sexo", "faixa", "occ_1_vitima") %in% names(x)))
})

testthat::test_that("catálogo raw documenta arquivos do RS Seguro e o input opcional", {
  raw_readme <- readLines(file.path("data", "raw", "README.md"), warn = FALSE, encoding = "UTF-8")

  testthat::expect_true(any(grepl("rs_month_macrocrime.csv", raw_readme, fixed = TRUE)))
  testthat::expect_true(any(grepl("rs_month_macrocrime_profile_v1_1vict.csv", raw_readme, fixed = TRUE)))
  testthat::expect_true(any(grepl("rs_month_crime.csv", raw_readme, fixed = TRUE)))
  testthat::expect_true(any(grepl("não está no repositório atual", raw_readme, fixed = TRUE)))
})
