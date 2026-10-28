# Impede regressão de links quebrados e delimitadores LaTeX não suportados no GitHub.
testthat::test_that("Caderno E01-E10 contém links relativos válidos e equações Markdown", {
  file <- file.path("reports", "mortalidade_sp_0_74", "caderno_E01_E10.md")
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  matches <- regmatches(lines, gregexpr("\\]\\([^)]*\\)", lines, perl = TRUE))
  links <- unlist(matches, use.names = FALSE)
  targets <- sub("^\\]\\(([^)]*)\\)$", "\\1", links)
  local_links <- targets[!grepl("^(https?://|mailto:|#)", targets)]
  testthat::expect_gte(length(local_links), 6L)
  for (target in local_links) {
    local_file <- file.path(dirname(file), target)
    testthat::expect_true(file.exists(local_file),
                          info = paste("Link local quebrado:", target))
  }
  testthat::expect_equal(sum(trimws(lines) == "$$"), 12L)
  testthat::expect_false(any(trimws(lines) %in% c("\\[", "\\]")))
  figures <- list.files(file.path(dirname(file), "figures"), pattern = "[.]svg$", full.names = TRUE)
  testthat::expect_length(figures, 4L)
  for (svg_file in figures) {
    svg <- paste(readLines(svg_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    testthat::expect_true(startsWith(svg, "<svg xmlns="))
    testthat::expect_true(grepl("</svg>$", svg))
  }
})
