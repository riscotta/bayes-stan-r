# Conjuntos de dependencias R compartilhados pelos scripts de setup.
# Este arquivo nao instala pacotes; apenas centraliza a declaracao.

pkgs_min <- c(
  "data.table",
  "cmdstanr",
  "rstan",
  "posterior",
  "bayesplot",
  "loo",
  "bridgesampling",
  "ggplot2",
  "scales",
  "dplyr",
  "tidyr",
  "purrr",
  "jsonlite",
  "readr",
  "readxl",
  "tibble",
  "digest",
  "stringr",
  "forcats",
  "stringi",
  "janitor",
  "lubridate",
  "sidrar",
  "here",
  "quantmod",
  "xts",
  "zoo",
  "rbcb",
  "survival",
  "Stat2Data"
)

pkgs_all <- unique(c(
  pkgs_min,
  "brms",
  "fs",
  "glue",
  "cli",
  "checkmate",
  "withr",
  "testthat",
  "knitr",
  "rmarkdown",
  "yaml"
))
