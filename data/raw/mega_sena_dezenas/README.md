# Mega-Sena — dados e artefatos auditáveis

Esta pasta contém o recorte versionado necessário para reproduzir o script final do estudo `mega_sena_dezenas`.

## Arquivos principais

- `base_analitica.csv`: base longa, uma linha por dezena sorteada em cada concurso.
- `concursos_analiticos.csv`: base no nível do concurso.
- `dicionario_base_analitica.csv`: dicionário da base analítica.
- `tabela_resumo_dados.csv`: resumo básico dos dados usados no estudo.
- `tabelas_auditadas/`: tabelas finais e intermediárias usadas para recompor a síntese.
- `diagnosticos_auditados/`: diagnósticos MCMC, checagem preditiva posterior e configuração de amostragem.

## Observação

Os CSVs brutos completos do CmdStan não foram versionados aqui porque são artefatos pesados de execução. O script permite reexecutar a amostragem com `--run_stan=1`, desde que `cmdstanr` e CmdStan estejam configurados.
