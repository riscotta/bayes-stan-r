testthat::test_that("catalogo de scripts tem numeracao sequencial", {
  path <- file.path("scripts", "README.md")
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")

  heading_idx <- grep("^### [0-9]+\\)", lines)
  headings <- lines[heading_idx]
  numbers <- as.integer(sub("^### ([0-9]+)\\).*", "\\1", headings))

  testthat::expect_gt(length(numbers), 0L)
  testthat::expect_equal(numbers, seq_along(numbers))

  conventions_idx <- grep("^## Convenções$", lines)

  testthat::expect_length(conventions_idx, 1L)
  testthat::expect_gt(conventions_idx, max(heading_idx))
})

testthat::test_that("catalogo referencia todas as pastas de estudo", {
  lines <- readLines(
    file.path("scripts", "README.md"),
    warn = FALSE,
    encoding = "UTF-8"
  )

  study_dirs <- list.dirs("scripts", recursive = FALSE, full.names = FALSE)
  study_dirs <- sort(setdiff(study_dirs, "_setup"))

  documented <- vapply(
    study_dirs,
    function(study) {
      needle <- paste0("scripts/", study, "/")
      any(grepl(needle, lines, fixed = TRUE))
    },
    logical(1)
  )

  if (any(!documented)) {
    testthat::fail(
      paste(
        "Pastas de estudo ausentes do catalogo:",
        paste(study_dirs[!documented], collapse = ", ")
      )
    )
  }

  testthat::expect_true(all(documented))
})
