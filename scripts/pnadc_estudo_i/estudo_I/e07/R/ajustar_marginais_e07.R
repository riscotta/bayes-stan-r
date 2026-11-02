options(warn = 1)
script_flag <- grep('^--file=', commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_flag) != 1L) stop('Execute com Rscript.')
script_path <- normalizePath(sub('^--file=', '', script_flag[[1L]]), mustWork = TRUE)
root <- normalizePath(file.path(dirname(script_path), '..'), mustWork = TRUE)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !args[[1L]] %in% c('smoke', 'completo')) {
  stop('Uso: Rscript R/ajustar_marginais_e07.R smoke|completo')
}
modo <- args[[1L]]
source(file.path(root, 'R', 'funcoes_e07.R'), local = TRUE)
base <- carregar_e_validar_e04(root)
verificar_ambiente(root)
ref <- subset(base, Periodo == '202609' & Papel == 'principal')
ref <- ref[order(ref$Eixo, ref$IndicadorId), , drop = FALSE]
if (modo == 'smoke') ref <- subset(ref, IndicadorId == 'desocupacao_pct')
if (!nrow(ref)) stop('Nenhum indicador selecionado.')
carimbo <- format(Sys.time(), '%Y%m%d_%H%M%S')
dir_saida <- file.path(root, 'resultados', paste0(modo, '_', carimbo, '_', Sys.getpid()))
dir.create(dir_saida, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(dir_saida)) stop('Nao foi possivel criar pasta de resultados: ', dir_saida)
cat('Modo: ', modo, ' | saida: ', dir_saida, '\n', sep = '')
cat('MODELOS: taxas, totais e rendimento rodados SEPARADAMENTE; nenhuma posterior conjunta.\n')
modelos <- list()
resumos <- list()
for (i in seq_len(nrow(ref))) {
  x <- ref[i, , drop = FALSE]
  id <- x$IndicadorId[[1L]]
  periodo <- x$Periodo[[1L]]
  tipo <- if (grepl('_pct$', id)) 'taxa' else
          if (grepl('_mil$', id)) 'contagem' else
          if (id == 'rendimento_real_habitual_rs') 'rendimento' else stop('Tipo nao previsto: ', id)
  stan_nome <- switch(tipo, taxa = 'taxa_marginal.stan',
                      contagem = 'contagem_marginal.stan',
                      rendimento = 'rendimento_marginal.stan')
  y <- as.numeric(x$Valor[[1L]])
  cv <- as.numeric(x$CVPercentual[[1L]])
  s <- abs(y) * cv / 100
  if (!is.finite(y) || !is.finite(s) || s <= 0) stop('y/CV invalidos: ', id)
  data_stan <- list(y = y, s = s)
  if (tipo == 'taxa' && (y < 0 || y > 100)) stop('Taxa fora do suporte: ', id)
  if (tipo == 'contagem') {
    p14 <- subset(base, Periodo == periodo & IndicadorId == 'populacao_14mais_mil')
    if (nrow(p14) != 1L) stop('P14+ unico ausente em ', periodo)
    P <- as.numeric(p14$Valor[[1L]])
    if (!is.finite(P) || P <= 0 || y > P) stop('Contagem incompativel com P14+: ', id)
    data_stan$populacao_14mais_mil <- P
  }
  dir_fit <- file.path(dir_saida, paste0(x$Eixo[[1L]], '_', id, '_', periodo))
  dir.create(dir_fit)
  reg <- data.frame(Eixo = x$Eixo, IndicadorId = id, Periodo = periodo,
                    y = y, CVPercentual = cv, s = s, Unidade = x$UnidadeAnalitica,
                    Tipo = tipo, Prior = switch(tipo, taxa = 'p~Beta(1,1)',
                                               contagem = 'q~Beta(1,1); P14fixa',
                                               rendimento = 'mu~HalfNormal(10000)'),
                    P14Fixa = if (tipo == 'contagem') data_stan$populacao_14mais_mil else NA_real_)
  gravar_csv(reg, file.path(dir_fit, 'entrada.csv'))
  if (is.null(modelos[[tipo]])) {
    cat('Compilando: ', stan_nome, '\n', sep = '')
    modelos[[tipo]] <- cmdstanr::cmdstan_model(file.path(root, 'stan', stan_nome), quiet = TRUE)
  }
  cat('Amostrando ', id, ' (', tipo, '), ', periodo, ', y=', y, ', s=', s, '\n', sep = '')
  fit <- modelos[[tipo]]$sample(
    data = data_stan,
    seed = 20261102L + i,
    chains = if (modo == 'smoke') 2L else 4L,
    parallel_chains = if (modo == 'smoke') 2L else 4L,
    iter_warmup = if (modo == 'smoke') 100L else 750L,
    iter_sampling = if (modo == 'smoke') 100L else 750L,
    refresh = 0,
    output_dir = dir_fit,
    output_basename = paste0('chains_', id),
    sig_figs = 12,
    save_warmup = FALSE
  )
  if (any(fit$return_codes() != 0)) stop('Cadeias com falha em ', id, '; arquivos preservados em ', dir_fit)
  fit$save_object(file = file.path(dir_fit, 'fit.rds'))
  sfit <- fit$summary(variables = 'theta')
  gravar_csv(sfit, file.path(dir_fit, 'resumo_theta.csv'))
  resumos[[length(resumos) + 1L]] <- cbind(reg, sfit[, setdiff(names(sfit), 'variable'), drop = FALSE])
  writeLines(c('MODELO MARGINAL CONDICIONAL, NAO CONTRASTE',
               paste('R:', R.version.string),
               paste('cmdstanr:', as.character(utils::packageVersion('cmdstanr'))),
               paste('CmdStan:', cmdstanr::cmdstan_version()),
               paste('Stan:', stan_nome),
               paste('seed:', 20261102L + i),
               paste('Cadeias:', if (modo == 'smoke') 2L else 4L),
               paste('Warmup/amostra por cadeia:', if (modo == 'smoke') 100L else 750L),
               paste('Arquivos brutos:', paste(fit$output_files(), collapse = '; '))),
             file.path(dir_fit, 'execucao.txt'))
  cat('Salvos CSVs CmdStan, fit.rds e resumo theta em ', dir_fit, '\n', sep = '')
}
if (length(resumos)) gravar_csv(do.call(rbind, resumos), file.path(dir_saida, 'resumo_marginais.csv'))
cat('CONCLUIDO ', modo, ': ', length(resumos), ' ajuste(s) marginal(is), SEM contraste conjunto.\n', sep = '')
