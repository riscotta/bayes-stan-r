# PNAD Contínua — desocupação

Script Bayesiano em **Stan** via **rstan** para analisar a taxa oficial de desocupação da **PNAD Contínua**, combinando a série mensal total e a série trimestral por sexo.

O script:

- lê os CSVs exportados da PNAD Contínua já no formato tabular
- valida coerência entre série mensal e trimestral
- trata o arquivo específico de jan/2026 como checagem opcional de consistência
- ajusta um modelo temporal em Stan na escala logito
- imprime diagnóstico MCMC e relatório final apenas no console

## Arquivos

- Script: `scripts/pnadc_desocupacao/pnadc_desocupacao_rstan.R`
- Entrada padrão: `data/raw/pnadc_desocupacao/`
- Arquivos esperados:
  - `pnadc_mensal_taxa_desocupacao_6381.csv`
  - `pnadc_trimestral_taxa_desocupacao_por_sexo_4093.csv`
  - `pnadc_mensal_taxa_desocupacao_jan_2026.csv` *(opcional; usado só para checagem)*

## Como rodar

A partir do **root** do repositório:

```bash
Rscript scripts/pnadc_desocupacao/pnadc_desocupacao_rstan.R
```

Modo final (mais estável e mais lento):

```bash
Rscript scripts/pnadc_desocupacao/pnadc_desocupacao_rstan.R --modo_debug=0
```

Com diretório de entrada explícito:

```bash
Rscript scripts/pnadc_desocupacao/pnadc_desocupacao_rstan.R --input_dir=data/raw/pnadc_desocupacao
```

## Opções principais

- `--input_dir=data/raw/pnadc_desocupacao`
- `--file_mensal=pnadc_mensal_taxa_desocupacao_6381.csv`
- `--file_sexo=pnadc_trimestral_taxa_desocupacao_por_sexo_4093.csv`
- `--file_jan26=pnadc_mensal_taxa_desocupacao_jan_2026.csv`
- `--modo_debug=1` *(default; valida pipeline mais rápido)*
- `--modo_debug=0` *(ajuste final mais estável)*
- `--seed_model=20260402`
- `--chains=...`
- `--iter=...`
- `--warmup=...`
- `--adapt_delta=...`
- `--max_treedepth=...`
- `--cores=...`
- `--refresh=...`

## Dependências

Instale dependências R:

```bash
Rscript scripts/_setup/install_deps.R --all
```

> Este exemplo usa **rstan**. Não depende de `cmdstanr`.

## Observações

- Sem `setwd()`.
- O script deve ser rodado a partir da raiz do repositório.
- Por padrão, ele **não grava** saídas analíticas em disco.
- Os CSVs de entrada **não são versionados** por padrão; coloque-os localmente em `data/raw/pnadc_desocupacao/`.
