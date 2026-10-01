# Proveniência e licenciamento dos dados

Este documento é o inventário canônico de proveniência e condições de reutilização dos dados usados pelo repositório.

A versão estruturada, destinada a validação automática, está em:

`data/DATA_PROVENANCE.csv`

**Data da auditoria:** 2026-10-01.

## Regra principal

A licença MIT do arquivo `LICENSE` cobre o software e a documentação autoral do repositório. Ela **não deve ser interpretada como licença automática para datasets de terceiros** armazenados em `data/raw/`.

Um dataset só deve ser descrito como aberto ou redistribuível quando houver base documental específica para isso.

## Estados usados no inventário

- `partial_verified`: há termos oficiais verificáveis para parte da origem, mas o arquivo local contém enriquecimentos ou componentes cuja licença ainda precisa ser rastreada.
- `public_source_no_explicit_dataset_license`: a fonte oficial publica os dados, mas a auditoria não localizou uma licença explícita de reutilização aplicável ao dataset/extrato.
- `attribution_documented_license_unverified`: a fonte fornece forma de citação, porém isso não é suficiente para afirmar uma licença ampla de redistribuição.
- `page_specific_unverified`: a plataforma usa licenças específicas por dataset e a licença exata precisa ser verificada na página da base.
- `source_terms_unverified`: a fonte é conhecida, mas os termos aplicáveis ao recorte/derivado ainda não foram confirmados.
- `not_identified`: não há licença suficientemente documentada no repositório ou nas fontes verificadas.

Para redistribuição:

- `review_required`: o arquivo já está versionado, mas sua redistribuição não deve ser presumida livre.
- `not_versioned`: a fonte é externa e o repositório não redistribui o dado bruto.
- `aggregates_only`: somente agregados/artefatos não individualizados são publicados.

## Fontes com termos verificados parcialmente

### World Bank

O World Bank Data Catalog informa que datasets produzidos pelo próprio Banco Mundial e distribuídos como open data usam, por padrão, **CC BY 4.0**, salvo indicação diferente no metadado do dataset.

Referências oficiais:

- https://datacatalog.worldbank.org/public-licenses
- https://data.worldbank.org/summary-terms-of-use

O arquivo local `world_bank_Import_Usd_enriched.csv` é enriquecido pelo projeto. Por isso, a licença do arquivo inteiro não é declarada como CC BY 4.0 até que a origem de todos os campos adicionados seja confirmada.

### INEP — Censo Escolar

O INEP publica os microdados do Censo Escolar como dados abertos e mantém downloads oficiais por ano:

- https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-escolar

A página geral de microdados documenta ainda os controles de privacidade e adequação à LGPD:

- https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados

Nesta auditoria não foi localizada, nessas páginas, uma licença explícita de reutilização comparável a CC BY. Por isso, os ZIPs continuam como cache local não versionado e o inventário não afirma uma licença que não esteja expressamente documentada.

### IBGE / SIDRA

Os estudos de PNAD Contínua e PMS usam fontes oficiais do IBGE/SIDRA. O IBGE mantém política de dados abertos e termos de uso de seus portais, mas a auditoria não identificou uma licença específica inequívoca aplicável a cada extrato versionado.

Referências institucionais:

- https://sidra.ibge.gov.br/
- https://www.ibge.gov.br/acesso-informacao/acoes-e-programas.html

Os arquivos versionados devem manter identificação da tabela/fonte e atribuição ao IBGE.

### UNODC — Prisons and Prisoners

O metadado oficial do dataset informa a origem dos dados e fornece forma sugerida de citação:

- https://dataunodc.un.org/dp-prisons-persons-held-regional
- https://data.unodc.org/sites/dataportal.unodc.org/files/2025-10/metadata_prisons_and_prisoners.pdf

A existência de uma citação sugerida não é tratada como prova de uma licença geral de redistribuição. O inventário, portanto, mantém `review_required`.

Também não se transferem para este dataset as regras de produtos UNODC diferentes, especialmente microdados/repositórios de acesso restrito.

### IOM — Missing Migrants Project

O Missing Migrants Project disponibiliza dados para download e documenta sua metodologia:

- https://missingmigrants.iom.int/downloads
- https://missingmigrants.iom.int/sites/g/files/tmzbdl601/files/publication/file/MMP%2520data%2520collection%2520guidelines_EN.pdf

Nesta auditoria não foi localizada uma licença específica inequívoca para o CSV baixável. As licenças das publicações da IOM não são automaticamente aplicadas ao dataset. A cópia versionada permanece marcada para revisão de redistribuição.

### Kaggle

No Kaggle, a licença é definida por dataset. A licença de um notebook que usa uma base **não é** a licença da base.

Documentação da plataforma:

- https://www.kaggle.com/docs/datasets
- https://www.kaggle.com/page/license-disclaimer

Consequentemente, bases Kaggle sem licença explicitamente registrada no repositório permanecem como `page_specific_unverified`.

## Itens versionados com revisão prioritária

Os seguintes dados já estão no Git e ainda não possuem uma cadeia de licença/proveniência suficientemente completa:

- `data/raw/TherapeuticTouchData.csv`
- `data/raw/BattingAverage.csv`
- `data/raw/mortality/Dados_Mortalidade.xlsx`
- `data/raw/isus_sia/ISUS_SIA_PARS.zip`
- `data/raw/rs_seguro/rs_month_macrocrime.csv`
- `data/raw/rs_seguro/rs_month_macrocrime_profile_v1_1vict.csv`
- `data/raw/exercises_dataset/final_exercise_dataset.csv`
- `data/raw/sherlock_holmes_terms/`
- `data/raw/missing_migrants/`

O inventário não determina que esses arquivos estejam irregularmente publicados. Ele registra que **não há evidência suficiente para afirmar redistribuição irrestrita**.

## Dados individualizados e dados sensíveis

O estudo `sobrevivencia_cadastral_poa` mantém a base individual com CNPJ/endereço fora do Git. Somente entradas agregadas e artefatos auditáveis são versionados.

O arquivo `ISUS_SIA_PARS.zip` exige revisão específica de origem, classificação e permissões antes de qualquer ampliação de distribuição, pois o repositório atualmente não documenta suficientemente sua cadeia de proveniência.

## Procedimento para novos datasets

Antes de adicionar um novo dado bruto a `data/raw/`:

1. registrar o dataset em `data/DATA_PROVENANCE.csv`;
2. informar fonte e publisher;
3. guardar o URL original ou identificador persistente;
4. registrar licença/termos aplicáveis, sem inferir licença a partir da licença do código;
5. definir se o arquivo pode ser versionado, deve ficar externo ou deve ser publicado apenas de forma agregada;
6. quando aplicável, documentar data de extração, versão, tabela/API e hash.

O CI verifica que todo item de primeiro nível versionado em `data/raw/` esteja coberto pelo inventário.
