# Decisões preliminares de publicação dos dados — 28/10/2026

**Escopo:** 24 registros em `data/DATA_PROVENANCE.csv`; 116 arquivos de dados brutos, intermediários e auditados versionados (aproximadamente 39,1 MB). Todas as decisões deste relatório são **preliminares**. O objetivo é separar três perguntas: (1) de onde vieram os dados, (2) os termos da fonte permitem uso e redistribuição, (3) a **cópia exata neste repositório** pode ser publicada, considerando transformações e sigilo?

## Estado da auditoria

| Classe | Quantidade | Conduta |
| --- | ---: | --- |
| `review_required` | 14 | Não declarar redistribuição irrestrita sem autorização e rastreabilidade |
| `not_versioned` | 7 | Manter dados externos fora do repositório até documentar fonte e termos |
| `aggregates_only` | 3 | Preservar apenas a política de artefatos agregados, sem dados individualizados |

### Triagem prioritária

| Dataset | Risco pendente | Providência verificável |
| --- | --- | --- |
| `isus_sia` | **Crítico a conferir**: ZIP de extração institucional, conteúdo e autorização desconhecidos | Obter classificação institucional e permissão escrita; verificar estrutura e dados pessoais localmente; avaliar suspensão da publicação e limpeza do histórico *após* a avaliação |
| `rs_seguro_macrocrime` e `rs_seguro_macrocrime_profile` | Cópias operacionais não rastreadas até uma publicação oficial específica | Documentar sistema, extração, agregação e autorização; comparar com [dados abertos SSP/RS](https://ssp.rs.gov.br/dados-abertos); não equiparar automaticamente as duas fontes |
| `mortality` | XLSX de origem e licença desconhecidas | Identificar instituição produtora, período, identificação territorial e autorização |
| `exercises_dataset` | Dataset Kaggle não identificado por URL exata ou licença | Localizar fonte original do arquivo e sua licença, ou trocar por fonte documentada em novo PR |
| `sherlock_holmes_terms` | Corpus compilado sem origem documental | Confirmar edição, digitalização, curadoria e direitos da compilação antes de declarar domínio público sobre o CSV |
| `missing_migrants` | Dados de incidentes da IOM; licença da cópia exata não ligada à fonte primária | Registrar link permanente, data da extração, atribuição e termos primários; verificar se o arquivo contém campos sensíveis ou relatos identificáveis |
| `therapeutic_touch` | Reconstrução do experimento publicada sem licença do dataset | Identificar roteiro de reconstrução e licença da transcrição; distinguir fatos experimentais de tabelas publicadas |
| `batting_average` | Série MLB/ESPN compilada por terceiros | Identificar fonte de cada coluna e condições de redistribuição |

### Outros conjuntos e decisão

| Dataset | Situação e etapa seguinte |
| --- | --- |
| `saheart` | Não versionado; conservar download externo e checar termos antes de incorporar cópia |
| `rs_seguro_crime_optional` | Não versionado; não adicionar até autorização institucional |
| `deter_mensal_bioma_uf` | Não versionado; localizar endpoint oficial e versão da extração |
| `consumer_shopping_trends` | Não versionado; obter licença específica da página Kaggle |
| `mega_sena_dezenas` | Versionado: confirmar origem do histórico CAIXA e documentar cálculos derivados e licença |
| `censo_escolar` | Não versionado; preservar cache fora do Git e limites de privacidade do INEP |
| `sat` | Não versionado; verificar condições do pacote/dados originais |
| `pnadc_desocupacao` | Versionado: rastrear tabelas SIDRA 6381/4093, data da consulta e atribuição |
| `pms_servicos` | Versionado: rastrear tabela SIDRA 5906, transformação e atribuição |
| `retractions_time_to_retraction` | Não versionado; verificar licença Kaggle da fonte específica |
| `importacoes_world_bank` | Versionado e enriquecido: rastrear origem de **todas** as colunas; não atribuir CC BY a terceiros por analogia |
| `sobrevivencia_cadastral_poa` | Só agregados publicados; manter regra de ausência de CNPJ/endereço e registrar APIs/fontes |
| `prisons_prisoners_unodc` | Versionado: preservar citação e verificar termos exatos da série do portal UNODC |
| `caes_raca_f5` | Apenas agregados publicados; [fonte Dryad](https://datadryad.org/dataset/doi%3A10.5061/dryad.g4f4qrfr0) com licença CC0 verificada na [política do repositório](https://datadryad.org/help/guides/reuse); não incluir registros por animal |
| `mortalidade_sp_0_74` | Só artefatos agregados; documentar licença dos extratos de origem APVP/PRO-AIM e SEADE e comparabilidade territorial |

## Constatações verificáveis

- O [JAMA original](https://jamanetwork.com/journals/jama/fullarticle/187390) confirma a referência do experimento *Therapeutic Touch*, **não** uma licença sobre o CSV transcrito.
- Os [termos World Bank](https://data.worldbank.org/summary-terms-of-use) indicam CC BY 4.0 para dados da instituição salvo metadados; não estabelecem direitos dos enriquecimentos privados.
- A [página SSP/RS](https://ssp.rs.gov.br/dados-abertos) oferece arquivos oficiais; o simples fato de os dois CSVs locais conterem séries por crime, sexo e idade **não** demonstra identidade com a publicação pública.
- O [ICRC descreve o Missing Migrants Project](https://missingpersons.icrc.org/directory/iom-missing-migrants-project) como CC BY 4.0; a auditoria não encontrou prova primária da versão e dos termos aplicáveis ao **arquivo local específico**, que continua `review_required`.
- A origem exata de `final_exercise_dataset.csv` e `12_Sherlock_Holmes_Texts.csv` não foi encontrada por nome de arquivo em pesquisa pública. Isso não demonstra irregularidade; significa **proveniência não estabelecida**.

## Instrumento de controle e limites

`data/TRACKED_DATA_MANIFEST.csv` registra o arquivo, o `dataset_id`, o SHA do blob Git e seu tamanho. A suíte `tests/testthat/test-data-file-manifest.R` detecta mudanças nos dados versionados que não forem acompanhadas por uma atualização explícita desse registro. **Um hash não concede direitos de redistribuição.** Atualizar um manifesto também não constitui aprovação institucional.

O controle é preventivo em futuras propostas de alteração; ele não revoga o acesso público aos arquivos que já estão no histórico Git.

## Regra para resolução da Issue #14

Para cada linha `review_required`, exigir: fonte original identificável, versão/data de extração, termos ou autorização aplicáveis, processo de transformação, checagem de dados pessoais/sigilo, decisão documentada de publicação. Para os extratos institucionais, a decisão final depende da autoridade responsável. Não fazer limpeza destrutiva do histórico sem antes avaliar dependências, cópias e procedimento de resposta a incidentes.
