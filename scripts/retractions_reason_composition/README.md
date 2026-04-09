# Composição dos motivos de retratação científica

Modelo Bayesiano em **Stan** via **rstan** para analisar a **composição dos macrogrupos de motivos** de retratação científica com base na série **Global Scientific Retractions 1927–2026**.

O estudo:

- lê o CSV bruto de retratações globais
- valida consistência temporal e campos essenciais
- explode os motivos multi-rótulo por registro
- agrega os motivos em uma taxonomia analítica de macrogrupos
- ajusta um modelo **Dirichlet-multinomial hierárquico** com pooling parcial por editora, área, país e período
- imprime diagnósticos, PPCs, composições posteriores e contrastes temporais no console

## Arquivos

- Script: `scripts/retractions_reason_composition/retractions_reason_composition_rstan.R`
- Entrada padrão: `data/raw/retractions_time_to_retraction/global_scientific_retractions_1927_2026.csv`
- Saída padrão: **console-only** (não grava artefatos do estudo por padrão)

## Base de dados

O CSV bruto **não é versionado no repositório**.

Baixe a base em:

`https://www.kaggle.com/datasets/kanchana1990/global-scientific-retractions-19272026`

Depois salve o arquivo como:

`data/raw/retractions_time_to_retraction/global_scientific_retractions_1927_2026.csv`

## Como rodar

A partir da **raiz** do repositório:

```bash
Rscript scripts/retractions_reason_composition/retractions_reason_composition_rstan.R
```

Com parâmetros explícitos:

```bash
Rscript scripts/retractions_reason_composition/retractions_reason_composition_rstan.R --top_publishers_n=10 --top_countries_n=12 --period_width=10 --min_total_mentions_per_stratum=20
```

Se a base externa trouxer motivos novos ainda não cobertos pela taxonomia analítica, o script para por padrão e lista os tokens não mapeados. Para seguir descartando esses tokens da modelagem:

```bash
Rscript scripts/retractions_reason_composition/retractions_reason_composition_rstan.R --fail_on_unmapped=0
```

## Opções principais

- `--input_csv=...`
- `--seed=20260407`
- `--top_publishers_n=10`
- `--top_countries_n=12`
- `--period_width=10`
- `--min_total_mentions_per_stratum=20`
- `--chains=4`
- `--iter_warmup=1000`
- `--iter_sampling=1000`
- `--adapt_delta=0.97`
- `--max_treedepth=13`
- `--refresh=100`
- `--cores=...`
- `--fail_on_unmapped=1`

## Dependências

Instale dependências R:

```bash
Rscript scripts/_setup/install_deps.R --all
```

> Este exemplo usa **rstan**. Não depende de `cmdstanr`.

## Observações

- Sem `setwd()`.
- O script deve ser rodado a partir da raiz do repositório.
- O arquivo bruto fica fora do git por ser uma base externa grande.
- Por padrão, ele **não grava** saídas analíticas em disco.
- O modelo usa uma implementação explícita de `dirichlet_multinomial_lpmf` em Stan, evitando dependência de distribuição não nativa.
