testthat::test_that("outputs usam o estudo como primeiro nivel", {
  legacy_top_level <- file.path("outputs", c("figures", "tables", "models"))
  testthat::expect_false(
    any(dir.exists(legacy_top_level)),
    info = paste(
      "Diretorios legados encontrados:",
      paste(legacy_top_level[dir.exists(legacy_top_level)], collapse = ", ")
    )
  )

  files <- list.files(
    "scripts",
    pattern = "\\.(R|md)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  patterns <- c(
    "outputs/(figures|tables|models)/",
    "file\\.path\\(\\s*[\"']outputs[\"']\\s*,\\s*[\"'](figures|tables|models)[\"']",
    "here::here\\(\\s*[\"']outputs[\"']\\s*,\\s*[\"'](figures|tables|models)[\"']"
  )

  violations <- character()

  for (path in files) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    for (pattern in patterns) {
      hits <- grep(pattern, lines, perl = TRUE)
      if (length(hits)) {
        violations <- c(
          violations,
          sprintf("%s:%s", path, paste(hits, collapse = ","))
        )
      }
    }
  }

  if (length(violations)) {
    testthat::fail(
      paste(
        "Use outputs/<estudo>/<tipo>/; referencias legadas:",
        paste(unique(violations), collapse = "; ")
      )
    )
  }

  testthat::expect_length(violations, 0L)
})
