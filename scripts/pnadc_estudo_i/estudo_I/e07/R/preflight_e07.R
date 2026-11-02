options(warn = 1)
script_flag <- grep('^--file=', commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_flag) != 1L) stop('Execute este arquivo com Rscript, nao via source().')
script_path <- normalizePath(sub('^--file=', '', script_flag[[1L]]), mustWork = TRUE)
root <- normalizePath(file.path(dirname(script_path), '..'), mustWork = TRUE)
base_path <- file.path(root, 'dados', 'base_analitica_e04_longa.csv')
cat('E07 | preflight local | ', format(Sys.time(), '%Y-%m-%d %H:%M:%S %Z'), '\n', sep = '')
cat('Diretorio: ', root, '\n', 'R: ', R.version.string, '\n', sep = '')
if (!file.exists(base_path)) stop('Base E04 ausente: ', base_path)
base <- read.csv(base_path, sep = ';', dec = '.', stringsAsFactors = FALSE,
                 check.names = FALSE, na.strings = c('', 'NA'))
cols <- c('Eixo', 'IndicadorId', 'Indicador', 'Papel', 'Periodo', 'Valor',
          'CVPercentual', 'EstadoValor', 'EstadoCV', 'UnidadeAnalitica')
faltantes <- setdiff(cols, names(base))
if (length(faltantes)) stop('Colunas E04 ausentes: ', paste(faltantes, collapse = ', '))
base$Periodo <- sprintf('%06d', as.integer(base$Periodo))
ref <- subset(base, Periodo == '202609' & Papel == 'principal')
if (nrow(ref) != 13L || length(unique(ref$IndicadorId)) != 13L ||
    !setequal(unique(ref$Eixo), LETTERS[1:5])) {
  stop('Referencia 202609 deve conter 13 indicadores principais distintos dos eixos A-E')
}
if (anyNA(ref$Valor) || anyNA(ref$CVPercentual) ||
    any(ref$CVPercentual <= 0) || any(ref$Valor < 0)) {
  stop('Valores/CVs principais invalidos em 202609; nao substituir CV zero artificialmente')
}
if (!any(base$Periodo == '202609' & base$IndicadorId == 'populacao_14mais_mil')) {
  stop('Denominador de apoio P14+ ausente')
}
if (anyDuplicated(base[c('IndicadorId', 'Periodo')])) stop('Chave indicador-periodo duplicada')
cat('Base E04: ', nrow(base), ' linhas; 13 principais, cinco eixos presentes em 202609\n', sep = '')
for (arq in c('taxa_marginal.stan', 'contagem_marginal.stan', 'rendimento_marginal.stan')) {
  arquivo <- file.path(root, 'stan', arq)
  if (!file.exists(arquivo)) stop('Modelo Stan ausente: ', arquivo)
}
dir_saida <- file.path(root, 'resultados')
if (!dir.exists(dir_saida) && !dir.create(dir_saida, recursive = TRUE)) {
  stop('Nao foi possivel criar o diretorio de saida: ', dir_saida)
}
teste <- tempfile(tmpdir = dir_saida)
if (!file.create(teste)) stop('Sem permissao de escrita: ', dir_saida)
unlink(teste)
cat('Diretorio de saida gravavel: ', dir_saida, '\n', sep = '')
necessarios <- c('cmdstanr', 'posterior')
ausentes <- necessarios[!vapply(necessarios, requireNamespace, logical(1), quietly = TRUE)]
if (length(ausentes)) stop('Pacotes R ausentes: ', paste(ausentes, collapse = ', '),
                           '. Nenhuma instalacao automatica sera realizada.')
cat('cmdstanr: ', as.character(utils::packageVersion('cmdstanr')),
    '; posterior: ', as.character(utils::packageVersion('posterior')), '\n', sep = '')
path <- tryCatch(cmdstanr::cmdstan_path(), error = function(e) stop('CmdStan nao configurado: ', conditionMessage(e)))
cat('CmdStan: ', path, '; versao ', cmdstanr::cmdstan_version(), '\n', sep = '')
tryCatch(cmdstanr::check_cmdstan_toolchain(quiet = TRUE),
         error = function(e) stop('Toolchain C++ reprovado: ', conditionMessage(e)))
cat('Toolchain C++: OK; preflight aprovado. Nao houve compilacao nem MCMC.\n')
