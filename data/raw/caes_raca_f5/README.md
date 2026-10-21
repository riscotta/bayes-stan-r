# Dados — Estudo dos Cães F5

Origem: Morrill et al. (2022), Darwin's Ark, [Dryad DOI 10.5061/dryad.g4f4qrfr0](https://doi.org/10.5061/dryad.g4f4qrfr0). A escala `f5_score_norm` deriva dos questionários publicados; este projeto restringe-se a 2.084 cães completos, 196 raças **declaradas**, e não representa o conjunto total original. Os documentos E04/E09 descrevem a seleção e construção do desfecho.

Publicamos apenas tabelas agregadas já auditadas: `metricas_full_E08_MP.csv`, `metricas_por_fold_auditadas.csv`, `contrastes_por_fold_auditados.csv`, `bootstrap_independente_2000.csv` e `auditoria_estratos_suporte.csv`. Não são observações independentes adicionais e **não** permitem reconstruir os 2.084 escores individuais ou reexecutar modelos completos.

A base analítica individual `base_F5_principal_E04.csv` **não é distribuída neste repositório**. A reamostragem (`--run_stan=1`) exige obtê-la das etapas locais originais e copiá-la manualmente para esta pasta, sem versioná-la. SHA-256 exigido: `9db17e9d53edd88b7603591a89fae521f93d1a83b82a851a4156527f3408bad9`. A base analítica não é idêntica ao ZIP bruto do Dryad: para sua reconstrução é necessário o pipeline E04 original e seus critérios de elegibilidade e transformações.

Os 120 CSVs brutos do CmdStan e os ZIPs de retorno full E08_MP também não são publicados (tamanho/custo). Permanecem preservados no arquivo de auditoria do estudo. **Não trate a falta desses arquivos como evidência de reexecução dos fits neste repositório.**

Licenciamento: não se redistribui a base por animal; consulte as condições da fonte no Dryad antes de redistribuir quaisquer microdados. A licença MIT do código autoral não concede direitos sobre dados de terceiros. Os CSVs agregados foram gerados durante o estudo e não revelam dog_id, tutores ou dados individualizados.
