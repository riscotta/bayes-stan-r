testthat::test_that("manifesto cobre exatamente os arquivos de dados versionados", {
  if (!file.exists(".git")) testthat::skip("Sem metadados Git para auditar blobs versionados")
  manifest <- read.csv("data/TRACKED_DATA_MANIFEST.csv", stringsAsFactors = FALSE)
  inventory <- read.csv("data/DATA_PROVENANCE.csv", stringsAsFactors = FALSE)
  testthat::expect_identical(names(manifest),
    c("path", "dataset_id", "git_blob_sha", "bytes"))
  testthat::expect_gt(nrow(manifest), 100L)
  testthat::expect_false(anyDuplicated(manifest$path) > 0L)
  testthat::expect_true(all(grepl("^[a-f0-9]{40}$", manifest$git_blob_sha)))

  listing <- suppressWarnings(system2(
    "git", c("-c", "safe.directory=*", "ls-files", "--stage", "--", "data/raw"), stdout = TRUE, stderr = TRUE))
  status <- attr(listing, "status")
  if (!is.null(status) && status != 0L) {
    testthat::fail(paste0("Não foi possível consultar o índice Git (status ", status,
                         "): ", paste(head(listing, 3L), collapse = "; ")))
  }
  matched <- regmatches(listing,
    regexec("^[0-7]{6} ([[:xdigit:]]{40}) 0\\t(.+)$", listing, perl = TRUE))
  records <- Filter(function(x) length(x) == 3L, matched)
  paths <- vapply(records, function(x) x[[3]], character(1))
  shas <- tolower(vapply(records, function(x) x[[2]], character(1)))
  names(shas) <- paths
  paths <- paths[!grepl("/(README\\.md|\\.gitkeep)$", paths)]
  testthat::expect_setequal(manifest$path, paths)

  for (i in seq_len(nrow(manifest))) {
    m <- manifest[i, ]
    testthat::expect_identical(unname(shas[m$path]), m$git_blob_sha,
      info = paste("Blob modificado sem revisar manifesto:", m$path))
    testthat::expect_equal(as.numeric(file.info(m$path)$size), m$bytes,
      info = paste("Tamanho modificado:", m$path))

    row <- inventory[inventory$dataset_id == m$dataset_id, ]
    testthat::expect_equal(nrow(row), 1L, info = m$path)
    testthat::expect_identical(row$versioned, "yes", info = m$path)
    scope <- row$path_scope
    in_scope <- m$path == scope ||
      startsWith(m$path, paste0(scope, if (endsWith(scope, "/")) "" else "/"))
    testthat::expect_true(in_scope, info = m$path)
  }
})

testthat::test_that("riscos de licença não são marcados como homologados sem evidência", {
  inventory <- read.csv("data/DATA_PROVENANCE.csv", stringsAsFactors = FALSE)
  critical <- c("isus_sia", "rs_seguro_macrocrime",
                "rs_seguro_macrocrime_profile", "mortality")
  observed <- inventory[inventory$dataset_id %in% critical, ]
  testthat::expect_setequal(observed$dataset_id, critical)
  testthat::expect_true(all(observed$redistribution_status == "review_required"))
  testthat::expect_true(all(observed$review_priority == "high"))
  testthat::expect_true(file.exists("data/RAW_DATA_REVIEW_2026-10-28.md"))
})
