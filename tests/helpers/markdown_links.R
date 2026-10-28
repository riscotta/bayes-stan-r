# Links Markdown relativos, verificacao em R base.
markdown_local_links <- function() {
  roots <- c(".", "data", "scripts", "reports", "tests")
  paths <- sort(unique(unlist(lapply(roots, function(r) {
    list.files(r, pattern = "\\.(md|qmd)$", recursive = r != ".",
               full.names = TRUE)
  }), use.names = FALSE)))
  found <- list()
  n <- 0L
  for (path in paths) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    fenced <- FALSE
    for (i in seq_along(lines)) {
      line <- lines[[i]]
      if (grepl("^\\s*(`{3,}|~{3,})", line, perl = TRUE)) {
        fenced <- !fenced
        next
      }
      if (fenced) next
      hits <- regmatches(line, gregexpr("!?\\[[^]\\r\\n]*\\]\\([^)]*\\)",
                                         line, perl = TRUE))[[1L]]
      for (hit in hits) {
        url <- trimws(sub("^!?\\[[^]]*\\]\\(([^)]*)\\)$", "\\1", hit, perl = TRUE))
        url <- sub("^<([^>]+)>$", "\\1", url)
        url <- sub('[[:space:]]+"[^"]*"$', "", url)
        if (!nzchar(url) || grepl("^(https?://|mailto:|tel:|data:|#)",
                                 url, ignore.case = TRUE)) next
        target <- utils::URLdecode(strsplit(url, "#", fixed = TRUE)[[1L]][1L])
        target <- strsplit(target, "?", fixed = TRUE)[[1L]][1L]
        if (!nzchar(target)) next
        resolved <- if (startsWith(target, "/")) {
          file.path(".", sub("^/+", "", target))
        } else file.path(dirname(path), target)
        n <- n + 1L
        found[[n]] <- data.frame(source = path, line = i, destination = url,
                                 exists = file.exists(resolved),
                                 stringsAsFactors = FALSE)
      }
    }
  }
  if (!length(found)) return(data.frame(source = character(), line = integer(),
                                        destination = character(), exists = logical()))
  do.call(rbind, found)
}
