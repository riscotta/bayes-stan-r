carregar_e_validar_e04 <- function(root) {
  arquivo <- file.path(root, 'dados', 'base_analitica_e04_longa.csv')
  if (!file.exists(arquivo)) stop('Base E04 nao encontrada: ', arquivo)
  d <- read.csv(arquivo, sep = ';', dec = '.', stringsAsFactors = FALSE,
                check.names = FALSE, na.strings = c('', 'NA'))
  requeridas <- c('Eixo', 'IndicadorId', 'Indicador', 'Papel', 'Periodo',
                  'Valor', 'CVPercentual', 'EstadoValor', 'EstadoCV', 'UnidadeAnalitica')
  ausentes <- setdiff(requeridas, names(d))
  if (length(ausentes)) stop('Colunas faltantes: ', paste(ausentes, collapse = ', '))
  d$Periodo <- sprintf('%06d', as.integer(d$Periodo))
  if (anyNA(d$Periodo) || anyDuplicated(d[c('IndicadorId', 'Periodo')])) {
    stop('Chave indicador-periodo invalida')
  }
  ref <- subset(d, Periodo == '202609' & Papel == 'principal')
  if (nrow(ref) != 13L || length(unique(ref$IndicadorId)) != 13L ||
      !setequal(ref$Eixo, LETTERS[1:5])) stop('13 indicadores e cinco eixos nao confirmados')
  if (any(ref$EstadoValor != 'PUBLICADO_NUMERICO') ||
      any(ref$EstadoCV != 'PUBLICADO_NUMERICO') ||
      any(!is.finite(ref$Valor)) || any(!is.finite(ref$CVPercentual)) ||
      any(ref$CVPercentual <= 0)) stop('Valor ou CV principal faltante/sem precisao em 202609')
  d
}
verificar_ambiente <- function(root) {
  saida <- file.path(root, 'resultados')
  if (!dir.exists(saida) && !dir.create(saida, recursive = TRUE)) stop('Sem saida gravavel')
  arq_teste <- tempfile(tmpdir = saida)
  if (!file.create(arq_teste)) stop('Sem permissao de escrita na saida')
  unlink(arq_teste)
  for (p in c('cmdstanr', 'posterior')) {
    if (!requireNamespace(p, quietly = TRUE)) stop('Pacote nao instalado: ', p)
  }
  tryCatch(cmdstanr::cmdstan_path(), error = function(e) stop('CmdStan indisponivel: ', conditionMessage(e)))
  tryCatch(cmdstanr::check_cmdstan_toolchain(quiet = TRUE),
           error = function(e) stop('Toolchain nao verificado: ', conditionMessage(e)))
  invisible(TRUE)
}
gravar_csv <- function(x, arquivo) {
  write.table(x, arquivo, sep = ';', dec = '.', row.names = FALSE,
              col.names = TRUE, quote = TRUE, na = '')
}
