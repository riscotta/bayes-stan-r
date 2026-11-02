options(warn = 1)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop('Uso: Rscript R/reprocessar_resumos_e07.R CAMINHO_PASTA_DA_EXECUCAO')
pasta <- normalizePath(args[[1L]], mustWork = TRUE)
if (!requireNamespace('cmdstanr', quietly = TRUE)) stop('cmdstanr indisponivel')
script_flag <- grep('^--file=', commandArgs(FALSE), value = TRUE)
root <- normalizePath(file.path(dirname(sub('^--file=', '', script_flag[[1L]])), '..'), mustWork = TRUE)
source(file.path(root, 'R', 'funcoes_e07.R'), local = TRUE)
subpastas <- list.dirs(pasta, full.names = TRUE, recursive = FALSE)
resumos <- list()
for (dir_fit in subpastas) {
  arq_entrada <- file.path(dir_fit, 'entrada.csv')
  if (!file.exists(arq_entrada)) next
  cadeia <- list.files(dir_fit, pattern = '^chains_.*\\.csv$', full.names = TRUE)
  if (!length(cadeia)) {
    cat('Sem CSVs de amostragem: ', dir_fit, '\n', sep = '')
    next
  }
  fit <- cmdstanr::as_cmdstan_fit(cadeia, check_diagnostics = FALSE)
  sfit <- fit$summary(variables = 'theta')
  gravar_csv(sfit, file.path(dir_fit, 'resumo_theta.csv'))
  entrada <- read.csv(arq_entrada, sep = ';', dec = '.', stringsAsFactors = FALSE)
  resumos[[length(resumos) + 1L]] <- cbind(entrada, sfit[, setdiff(names(sfit), 'variable'), drop = FALSE])
  cat('Resumo reconstruido SEM amostragem: ', dir_fit, '\n', sep = '')
}
if (!length(resumos)) stop('Nenhum resultado de cadeias aproveitavel localizado')
gravar_csv(do.call(rbind, resumos), file.path(pasta, 'resumo_marginais.csv'))
