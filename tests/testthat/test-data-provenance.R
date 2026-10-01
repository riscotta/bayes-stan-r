testthat::test_that("inventario de proveniencia cobre data/raw", {
  inventory_path <- file.path("data", "DATA_PROVENANCE.csv")

  testthat::expect_true(file.exists(inventory_path))

  inventory <- utils::read.csv(
    inventory_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  required_columns <- c(
    "dataset_id",
    "path_scope",
    "versioned",
    "source_name",
    "publisher",
    "source_url",
    "license_status",
    "license_or_terms",
    "redistribution_status",
    "review_priority",
    "notes"
  )

  testthat::expect_true(all(required_columns %in% names(inventory)))
  testthat::expect_false(anyDuplicated(inventory$dataset_id) > 0L)
  testthat::expect_true(all(inventory$versioned %in% c("yes", "no")))
  testthat::expect_true(all(nzchar(inventory$dataset_id)))
  testthat::expect_true(all(nzchar(inventory$path_scope)))
  testthat::expect_true(all(nzchar(inventory$license_status)))
  testthat::expect_true(all(nzchar(inventory$redistribution_status)))

  raw_entries <- list.files(
    file.path("data", "raw"),
    all.files = TRUE,
    no.. = TRUE,
    recursive = FALSE,
    full.names = FALSE
  )
  raw_entries <- setdiff(raw_entries, c(".gitkeep", "README.md"))

  uncovered <- vapply(
    raw_entries,
    function(entry) {
      prefix <- file.path("data", "raw", entry)
      !any(startsWith(inventory$path_scope, prefix))
    },
    logical(1)
  )

  if (any(uncovered)) {
    testthat::fail(
      paste(
        "Itens de data/raw sem registro em data/DATA_PROVENANCE.csv:",
        paste(raw_entries[uncovered], collapse = ", ")
      )
    )
  }

  tracked <- inventory[inventory$versioned == "yes", , drop = FALSE]
  missing_tracked <- tracked$path_scope[!file.exists(tracked$path_scope)]

  if (length(missing_tracked)) {
    testthat::fail(
      paste(
        "Inventario marca como versionado um caminho inexistente:",
        paste(missing_tracked, collapse = ", ")
      )
    )
  }

  third_party_mit_mistakes <- inventory$dataset_id %in% c(
    "therapeutic_touch",
    "batting_average"
  ) & grepl(
    "MIT",
    inventory$license_or_terms,
    ignore.case = TRUE
  )

  testthat::expect_false(any(third_party_mit_mistakes))
})
