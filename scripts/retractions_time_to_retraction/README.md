# Tempo ate retratacao de artigos cientificos

Modelo Bayesiano em **Stan** via **cmdstanr** para analisar o tempo ate retratacao de artigos cientificos com base na serie **Global Scientific Retractions 1927–2026**.

O estudo:

- le um CSV bruto com retratacoes globais
- valida datas e consistencia do lag entre publicacao original e retratacao
- agrega categorias raras para manter viabilidade computacional
- ajusta um modelo Bayesiano com efeitos fixos e efeito aleatorio por editora
- produz diagnosticos, sumarios posteriores, tabelas e graficos

## Arquivos

- Script: `scripts/retractions_time_to_retraction/retractions_time_to_retraction_cmdstanr.R`
- Entrada padrao: `data/raw/retractions_time_to_retraction/global_scientific_retractions_1927_2026.csv`
- Saidas padrao:
  - `outputs/tables/retractions_time_to_retraction/*.csv`
  - `outputs/tables/retractions_time_to_retraction/retractions_time_to_retraction_report.txt`
  - `outputs/figures/retractions_time_to_retraction/*.png`
  - `outputs/models/retractions_time_to_retraction/*.rds` *(opcional, via flag)*

## Base de dados

O CSV bruto **nao e versionado no repositorio**.

Baixe a base em:

`https://www.kaggle.com/datasets/kanchana1990/global-scientific-retractions-19272026`

Depois salve o arquivo como:

`data/raw/retractions_time_to_retraction/global_scientific_retractions_1927_2026.csv`

Ou informe um caminho explicito via `--input_csv=...`.

## Como rodar

A partir da **raiz** do repositorio:

```bash
Rscript scripts/retractions_time_to_retraction/retractions_time_to_retraction_cmdstanr.R
```

Com caminho explicito para o CSV:

```bash
Rscript scripts/retractions_time_to_retraction/retractions_time_to_retraction_cmdstanr.R --input_csv=data/raw/retractions_time_to_retraction/global_scientific_retractions_1927_2026.csv
```

Versao mais leve:

```bash
Rscript scripts/retractions_time_to_retraction/retractions_time_to_retraction_cmdstanr.R --chains=2 --parallel_chains=2 --iter_warmup=500 --iter_sampling=500
```

Sem exportar artefatos:

```bash
Rscript scripts/retractions_time_to_retraction/retractions_time_to_retraction_cmdstanr.R --save_tables=0 --save_plots=0 --save_report=0
```

## Opcoes principais

- `--input_csv=...`
- `--cutoff_original_year=2021`
- `--publisher_min_n=500`
- `--subject_min_n=1000`
- `--reason_min_n=1000`
- `--type_min_n=500`
- `--chains=2`
- `--parallel_chains=2`
- `--iter_warmup=1000`
- `--iter_sampling=1000`
- `--adapt_delta=0.95`
- `--max_treedepth=11`
- `--refresh=50`
- `--save_tables=1`
- `--save_plots=1`
- `--save_report=1`
- `--save_fit_object=0`

## Dependencias

Instale dependencias R:

```bash
Rscript scripts/_setup/install_deps.R --all
```

Instale o CmdStan (uma vez por maquina):

```bash
Rscript scripts/_setup/install_cmdstan.R
```

## Observacoes

- Sem `setwd()`.
- O script deve ser rodado a partir da raiz do repo.
- O arquivo bruto fica fora do git por ser uma base externa grande.
- As saidas vao para `outputs/`, que ja esta ignorado no versionamento.
