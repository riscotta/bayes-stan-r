# Inventário de arquivos, versões e proveniência — T01.1 (03/11/2026)

**Estado:** inventário técnico para revisão independente (QA PENDENTE). Não representa homologação de licença, inspeção de privacidade, decisão de retirada ou autorização de publicação.

## 1. Identidade e método

- Repositório: `riscotta/bayes-stan-r`; branch de origem: `main`; commit-base: `444a6a68a28d31c9dacdd84d36de7034504823c7`; árvore Git: `6891df2782b01ea50f6603623b498860f49b0bab`.
- Fonte de classificação por dataset: [DATA_PROVENANCE.csv](DATA_PROVENANCE.csv) (`git_blob_sha: 97cfe122f903b8c1b52c57ed0a6c287695f33ba3`).
- Fonte do inventário arquivo a arquivo: [TRACKED_DATA_MANIFEST.csv](TRACKED_DATA_MANIFEST.csv) (`git_blob_sha: ed99382fbe6764a00567d7ec534268398a580145`), contendo caminho relativo, `dataset_id`, SHA do blob Git e bytes. Consultar este CSV para **cada um dos 117 caminhos e suas versões binárias**; evitar duplicação divergente de manifestos.
- Revisão preliminar histórica de 28/10: [RAW_DATA_REVIEW_2026-10-28.md](RAW_DATA_REVIEW_2026-10-28.md). Seus totais **24/116** retratam aquele momento e não foram alterados retroativamente.
- Referência de governança: [Issue #14](https://github.com/riscotta/bayes-stan-r/issues/14), ainda aberta.
- Método de verificação: leitura de metadados e texto documental na API GitHub, sem abrir conteúdo tabular de dados brutos, registros individuais, células XLSX ou ZIP institucional.

**Distinção de versão:** o SHA do blob Git fixa a versão dos **bytes versionados**, não a data da coleta, o vintage estatístico, a edição do corpus ou a licença da fonte. Onde a data/versão da fonte não é informada explicitamente nos documentos citados, seu estado permanece **não comprovado**. Identificadores como SIDRA 6381/4093/5906 e DOI Dryad distinguem fontes, mas não provam a versão exata de cada extrato.

## 2. Reconciliação do inventário em `main`

| Indicador | Resultado |
| --- | ---: |
| Registros de dataset em `DATA_PROVENANCE.csv` | 25 |
| `review_required` | 14 |
| `not_versioned` | 7 |
| `aggregates_only` | 4 |
| Arquivos de dados em `TRACKED_DATA_MANIFEST.csv` | 117 |
| Grupos (`dataset_id`) efetivamente versionados | 18 |
| Arquivos em `data/raw/`, excluindo .md e .gitkeep | 117 |
| Entradas do manifesto sem blob Git | 0 |
| SHA ou tamanho divergente | 0 |
| Blobs de dados sem linha no manifesto | 0 |

**Diferença frente a 28/10:** adição de `pnadc_estudo_i` (`aggregates_only`) e do arquivo `data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv`, presente no snapshot da `main` de 02/11/2026. Esta reconciliação vale exclusivamente para o commit-base acima; não estabelece custódia do histórico nem verificação de dados pessoais.

## 3. Matriz por conjunto de dados

**Leitura:** a coluna “Arquivos” é a contagem no manifesto; zero significa que o dataset original não está versionado (eventuais README ou `.gitkeep` não contam). A coluna “Cadeia de transformações” descreve apenas o que os arquivos de metadados registram ou apontam; **não equivale a execução ou auditoria de código**. A versão/data exata dos **dados de origem** está pendente de documentação específica, salvo identificadores explicitamente citados.

| Dataset | Arquivos | Publicação preliminar | Origem registrada (não homologada) | Cadeia de transformações: evidência/pendência | Estado de licença | Prioridade |
| --- | ---: | --- | --- | --- | --- | --- |
| `therapeutic_touch` | 1 | review_required | [JAMA article / dataset reconstruction](https://doi.org/10.1001/jama.279.13.1005) (JAMA / original authors) | Reconstrução/transcrição referida; procedimento e versão do CSV não rastreados | not_identified | high |
| `batting_average` | 1 | review_required | MLB 2012 batting data as used by Kruschke / ESPN (ESPN / MLB / secondary compilation) | Compilação MLB/ESPN citada; proveniência e transformações por coluna não rastreadas | not_identified | high |
| `mortality` | 1 | review_required | Source not documented (unknown) | Transformações e extração original não documentadas | not_identified | high |
| `isus_sia` | 1 | review_required | ISUS/SIA project extract (unknown) | Extrato institucional ZIP; processo de geração e classificação não documentados | not_identified | high |
| `saheart` | 0 | not_versioned | [Elements of Statistical Learning SAheart dataset](https://hastie.su.domains/ElemStatLearn/datasets/SAheart.data) (Hastie Tibshirani Friedman) | Download externo quando necessário; sem transformação versionada | not_identified | low |
| `rs_seguro_macrocrime` | 1 | review_required | RS Seguro operational dataset (unknown) | Extrato operacional agregado por macrocrime; vínculo com fonte oficial não comprovado | not_identified | high |
| `rs_seguro_macrocrime_profile` | 1 | review_required | RS Seguro operational dataset (unknown) | Extrato operacional por perfil de vítima; geração e fonte oficial não comprovadas | not_identified | high |
| `rs_seguro_crime_optional` | 0 | not_versioned | RS Seguro optional crime-layer dataset (unknown) | Camada opcional ausente do Git; transformação não verificada | not_identified | medium |
| `deter_mensal_bioma_uf` | 0 | not_versioned | DETER monthly biome/UF extract (likely INPE / TerraBrasilis) | Extrato mensal opcional ausente; endpoint e método não documentados | not_identified | medium |
| `consumer_shopping_trends` | 0 | not_versioned | [Consumer Shopping Trends Analysis](https://www.kaggle.com/datasets/minahilfatima12328/consumer-shopping-trends-analysis) (Kaggle contributor) | Arquivo Kaggle ausente; sem pipeline versionado de entrada | page_specific_unverified | low |
| `mega_sena_dezenas` | 20 | review_required | Mega-Sena historical results prepared for the study (CAIXA / project derivation) | Base analítica e diagnósticos derivados presentes; origem e cálculos não rastreados integralmente | not_identified | medium |
| `censo_escolar` | 0 | not_versioned | [Microdados do Censo Escolar](https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-escolar) (INEP) | ZIPs oficiais mantidos fora do Git; cache local | public_source_no_explicit_dataset_license | low |
| `sat` | 0 | not_versioned | [Rdatasets / mosaicData SAT dataset](https://vincentarelbundock.github.io/Rdatasets/csv/mosaicData/SAT.csv) (Rdatasets / mosaicData) | Consumo via URL externa; cópia não versionada | not_identified | low |
| `pnadc_desocupacao` | 3 | review_required | [PNAD Contínua / SIDRA tables 6381 and 4093](https://sidra.ibge.gov.br/) (IBGE) | Extratos e recortes SIDRA (6381/4093); data e parâmetros da extração não comprovados | public_source_no_explicit_dataset_license | medium |
| `pms_servicos` | 1 | review_required | [PMS / SIDRA table 5906 analytical extract](https://sidra.ibge.gov.br/) (IBGE) | Consolidação analítica de extrato SIDRA 5906; transformação integral não auditada | public_source_no_explicit_dataset_license | medium |
| `retractions_time_to_retraction` | 0 | not_versioned | [Global Scientific Retractions 1927-2026](https://www.kaggle.com/datasets/kanchana1990/global-scientific-retractions-19272026) (Kaggle contributor) | Arquivo Kaggle ausente; nenhuma transformação versionada | page_specific_unverified | low |
| `importacoes_world_bank` | 1 | review_required | [World Bank annual import data enriched in the project](https://datacatalog.worldbank.org/public-licenses) (World Bank) | Indicador do World Bank com colunas enriquecidas; proveniência completa das colunas não demonstrada | partial_verified | medium |
| `exercises_dataset` | 1 | review_required | Exercises Dataset (Kaggle contributor) | Cópia Kaggle com origem URL exata e transformações não rastreadas | page_specific_unverified | high |
| `sherlock_holmes_terms` | 16 | review_required | Sherlock Holmes full-text dataset / analytical package (unknown) | Corpus compilado, entradas Stan e tabelas derivadas; fonte da compilação não documentada | not_identified | high |
| `missing_migrants` | 16 | review_required | [Missing Migrants Project](https://missingmigrants.iom.int/downloads) (IOM) | CSV IOM com entradas de modelo e diagnósticos derivados; versão de aquisição não rastreada | not_identified | high |
| `sobrevivencia_cadastral_poa` | 15 | aggregates_only | CNPJ + Porto Alegre permits aggregated for modeling (Receita Federal / Prefeitura de Porto Alegre) | Somente modelos agregados e resumos; extrações individuais excluídas do Git | source_terms_unverified | medium |
| `prisons_prisoners_unodc` | 19 | review_required | [UNODC Research Data Portal - Prisons and Prisoners](https://dataunodc.un.org/dp-prisons-persons-held-regional) (UNODC) | Base nacional analítica, entradas Stan e diagnósticos; versão do portal não comprovada | attribution_documented_license_unverified | medium |
| `caes_raca_f5` | 5 | aggregates_only | [Darwin's Ark F5 derived audit summaries](https://doi.org/10.5061/dryad.g4f4qrfr0) (Morrill et al. / Dryad) | Somente resultados agregados auditados; registros por animal excluídos | source_terms_verified | medium |
| `mortalidade_sp_0_74` | 13 | aggregates_only | APVP/PRO-AIM mortality counts and SEADE population estimates (Prefeitura de Sao Paulo / Fundacao SEADE) | Somente indicadores E07-E09 agregados; base E04 celular excluída | source_terms_unverified | high |
| `pnadc_estudo_i` | 1 | aggregates_only | [PNAD Continua SIDRA aggregated indicators and derived 26-row E09 descriptive summary](https://sidra.ibge.gov.br/) (IBGE) | Somente resumo descritivo E09 derivado (26 linhas, conforme inventário); snapshot SIDRA e cadeias E07 não versionados | public_source_no_explicit_dataset_license | medium |

## 4. Rastreabilidade e pendências sem extrapolação

1. **Versões dos arquivos:** caminhos, hashes Git por blob e tamanhos estão no manifesto acima, confrontado integralmente com a árvore `main`. O hash não identifica a versão remota do dataset nem sua licença.
2. **Fonte original:** nomes, provedores e links são os registrados em `DATA_PROVENANCE.csv`, não confirmados novamente na fonte neste ciclo. URLs ausentes continuam ausentes; links genéricos não substituem a página da versão extraída.
3. **Transformações:** a matriz distingue entradas brutas, agregados, entradas Stan e saídas auditadas conforme documentação existente. Não foi reexecutada qualquer transformação; linhagem coluna-a-coluna, scripts, parâmetros e datas de extração permanecem a provar onde não documentados.
4. **Licenças:** apenas o dataset Dryad possui termos de origem CC0 relatados como verificados na auditoria anterior, sem extensão automática a outros materiais. World Bank tem cobertura parcial CC BY para dados de origem, **não** para enriquecimentos. Nenhum `review_required` foi promovido a autorizado neste ciclo. MIT do código não alcança automaticamente dados de terceiros.
5. **Privacidade e permissão institucional:** prioritários em T01.2 e T01.3, especialmente ISUS/SIA, RS Seguro, mortalidade e incidentes IOM. Não houve abertura de arquivos sensíveis nem testes de anonimização; ausência de achado não é conclusão de segurança.
6. **Decisões finais, substituição, restrição e histórico Git:** fora do escopo de T01.1 (reservados a T01.4–T01.7). Não executar alterações destrutivas com base neste inventário.

## 5. Verificações executadas e limitações

- PASS — 25 registros de proveniência identificados; todos os 18 datasets com arquivos rastreados têm entrada no inventário.
- PASS — 117/117 arquivos de dados em `data/raw/` correspondem aos 117 caminhos do manifesto; 0 faltantes, 0 excedentes, 0 divergências de blob SHA/tamanho.
- PASS — estados preliminares contabilizados como 14 `review_required`, 7 `not_versioned` e 4 `aggregates_only`; nenhuma alteração de categoria.
- NÃO EXECUTADO — testes R/Python/Stan, leitura de registros de dados, validação jurídica, exame de privacidade, checagem direta de cada licença ou execução científica. Não aplicáveis ao inventário documental ou dependentes de acesso institucional.
- Risco residual: metadados preexistentes incompletos quanto a versões/vintages, transformações e termos. A `main` pode avançar após o snapshot. Este documento é uma fotografia verificável, **não** homologação final.

**Referências fixas:** [árvore Git do commit-base](https://api.github.com/repos/riscotta/bayes-stan-r/git/trees/444a6a68a28d31c9dacdd84d36de7034504823c7?recursive=1) · [Issue #14](https://github.com/riscotta/bayes-stan-r/issues/14) · [proveniência no SHA](https://github.com/riscotta/bayes-stan-r/blob/444a6a68a28d31c9dacdd84d36de7034504823c7/data/DATA_PROVENANCE.csv) · [manifesto no SHA](https://github.com/riscotta/bayes-stan-r/blob/444a6a68a28d31c9dacdd84d36de7034504823c7/data/TRACKED_DATA_MANIFEST.csv).

**Controle de QA:** execução T01.1 documentada; revisão independente, decisão e eventual merge ainda pendentes. A alteração adiciona somente este arquivo Markdown e preserva os inventários históricos e os bytes dos datasets.
