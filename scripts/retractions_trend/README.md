# Tendencia temporal das retratacoes cientificas

Modelo Bayesiano em **Stan** via **rstan** para analisar a **tendencia temporal** das retratacoes cientificas globais com base na serie **Global Scientific Retractions 1927–2026**.

O estudo:

- le o CSV bruto de retratacoes globais
- filtra anos completos para evitar distorcao do ano ainda em curso
- agrega grupos raros em `Outros` para manter estabilidade computacional
- ajusta uma binomial negativa hierarquica com **random walk de ordem 1 (RW1)**
- imprime diagnosticos, contrastes e curvas posteriores no console

## Arquivos

- Script: `scripts/retractions_trend/retractions_trend_rstan.R`
- Entrada padrao: `data/raw/retractions_time_to_retraction/global_scientific_retractions_1927_2026.csv`
- Saida padrao: **console-only** (nao grava artefatos do estudo por padrao)

## Base de dados

O CSV bruto **nao e versionado no repositorio**.

Baixe a base em:

`https://www.kaggle.com/datasets/kanchana1990/global-scientific-retractions-19272026`

Depois salve o arquivo como:

`data/raw/retractions_time_to_retraction/global_scientific_retractions_1927_2026.csv`

## Como rodar

A partir da **raiz** do repositorio:

```bash
Rscript scripts/retractions_trend/retractions_trend_rstan.R
```

Com parametros explicitos:

```bash
Rscript scripts/retractions_trend/retractions_trend_rstan.R --group_var=publisher --top_n_groups=15 --min_group_total=100
```

Alternando o agrupamento principal:

```bash
Rscript scripts/retractions_trend/retractions_trend_rstan.R --group_var=country
```

## Opcoes principais

- `--input_csv=...`
- `--group_var=publisher` *(ou `country`)*
- `--top_n_groups=15`
- `--min_group_total=100`
- `--start_year=NA`
- `--end_year_complete=2025`
- `--recent_incomplete_years=2026`
- `--chains=4`
- `--iter_warmup=1000`
- `--iter_sampling=1000`
- `--adapt_delta=0.95`
- `--max_treedepth=12`
- `--seed=20260407`

## Dependencias

Instale dependencias R:

```bash
Rscript scripts/_setup/install_deps.R --all
```

## Observacoes

- Sem `setwd()`.
- O script deve ser rodado a partir da raiz do repo.
- O arquivo bruto fica fora do git por ser uma base externa grande.
- O backend do **rstan** pode criar artefatos temporarios internos de compilacao, mas o script nao grava resultados do estudo em disco por design.
