# Consumer Shopping Trends — modelo Beta Bayesiano

Script Bayesiano em **Stan** via **rstan** para analisar uma variável de gasto do dataset **Consumer Shopping Trends Analysis**, com foco padrão em `avg_online_spend`.

O estudo:

- lê um CSV bruto do Kaggle já salvo localmente
- valida a variável-alvo e seus limites substantivos
- reescala o alvo para `(0,1)`
- ajusta um modelo **Beta(alpha, beta)** com priors lognormais
- produz diagnósticos MCMC, sumários posteriores, PPCs e gráficos comparativos
- mantém o modelo Stan apenas em memória, sem gravar arquivo `.stan`

## Arquivos

- Script: `scripts/consumer_shopping_trends_beta/consumer_shopping_trends_beta_rstan.R`
- Entrada padrão: `data/raw/consumer_shopping_trends/Consumer_Shopping_Trends_2026 (6).csv`
- Saídas padrão:
  - `outputs/tables/consumer_shopping_trends_beta/*.csv`
  - `outputs/tables/consumer_shopping_trends_beta/*.txt`
  - `outputs/figures/consumer_shopping_trends_beta/*.png`
  - `outputs/models/consumer_shopping_trends_beta/*.rds`

## Base de dados

O CSV bruto **não é versionado no repositório**.

Baixe a base em:

`https://www.kaggle.com/datasets/minahilfatima12328/consumer-shopping-trends-analysis`

Depois salve o arquivo em:

`data/raw/consumer_shopping_trends/Consumer_Shopping_Trends_2026 (6).csv`

Se o nome local for diferente, informe explicitamente via `--input_csv=...`.

## Como rodar

A partir da **raiz** do repositório:

```bash
Rscript scripts/consumer_shopping_trends_beta/consumer_shopping_trends_beta_rstan.R
```

Trocar a variável-alvo para gasto em loja:

```bash
Rscript scripts/consumer_shopping_trends_beta/consumer_shopping_trends_beta_rstan.R --target_var=avg_store_spend
```

Rodar uma versão mais leve:

```bash
Rscript scripts/consumer_shopping_trends_beta/consumer_shopping_trends_beta_rstan.R --chains=2 --iter=1000 --warmup=500
```

Sem exportar artefatos:

```bash
Rscript scripts/consumer_shopping_trends_beta/consumer_shopping_trends_beta_rstan.R --save_tables=0 --save_plots=0 --save_report=0 --save_fit_object=0
```

## Opções principais

- `--input_dir=data/raw/consumer_shopping_trends`
- `--input_csv=...`
- `--preferred_csv=Consumer_Shopping_Trends_2026 (6).csv`
- `--target_var=avg_online_spend`
- `--target_var=avg_store_spend`
- `--lower_bound=0`
- `--upper_bound=150000`
- `--seed=20260410`
- `--chains=4`
- `--iter=2000`
- `--warmup=1000`
- `--adapt_delta=0.95`
- `--max_treedepth=12`
- `--cores=...`
- `--refresh=100`
- `--save_tables=1`
- `--save_plots=1`
- `--save_report=1`
- `--save_fit_object=1`

## Dependências

Instale dependências R:

```bash
Rscript scripts/_setup/install_deps.R --all
```

> Este exemplo usa **rstan**. Não depende de `cmdstanr`.

## Observações

- Sem `setwd()`.
- O script deve ser rodado a partir da raiz do repo.
- A base bruta fica fora do git por ser uma base externa do Kaggle.
- As saídas vão para `outputs/`, que já está ignorado no versionamento.
