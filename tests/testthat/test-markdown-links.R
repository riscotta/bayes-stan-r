source(file.path("tests", "helpers", "markdown_links.R"), local = TRUE)

testthat::test_that("todos os links Markdown relativos apontam para destinos existentes", {
  links <- markdown_local_links()
  testthat::expect_gte(nrow(links), 15L)
  broken <- links[!links$exists, , drop = FALSE]
  testthat::expect_equal(nrow(broken), 0L,
    info = paste(sprintf("%s:%d -> %s", broken$source, broken$line,
                         broken$destination), collapse = "; "))
})

testthat::test_that("README de mortalidade SP mostra o bloco de comandos", {
  lines <- readLines("scripts/mortalidade_sp_0_74/README.md",
                     warn = FALSE, encoding = "UTF-8")
  testthat::expect_true(any(trimws(lines) == "```bash"))
  testthat::expect_false(any(grepl("\\`\\`\\`bash", lines, fixed = TRUE)))
})
