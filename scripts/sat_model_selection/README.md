# SAT — seleção Bayesiana de variáveis

Este exemplo ajusta e compara **todos os subconjuntos** dos quatro preditores clássicos da base **SAT** usando **Stan** via **rstan**.

O script:

- lê a base SAT diretamente de uma URL pública do **Rdatasets**
- padroniza resposta e preditores para o ajuste
- compara os `2^K` modelos possíveis (`K = 4`)
- calcula evidência marginal com **bridge sampling**
- calcula desempenho preditivo com **PSIS-LOO**
- resume diagnósticos **HMC**, **PPC** e **Bayesian Model Averaging (BMA)**
- imprime tudo apenas no console

> **Script:** `scripts/sat_model_selection/sat_stan_model_selection_rstan.R`
> **Dados:** lidos online de `https://vincentarelbundock.github.io/Rdatasets/csv/mosaicData/SAT.csv`

---

## Pré-requisitos

No diretório raiz do repositório:

```bash
Rscript scripts/_setup/install_deps.R --all
```

> Este exemplo usa **rstan** e **bridgesampling**. Não depende de `cmdstanr`.

---

## Como rodar

A partir do root do repo:

```bash
Rscript scripts/sat_model_selection/sat_stan_model_selection_rstan.R
```

Exemplo com parâmetros explícitos:

```bash
Rscript scripts/sat_model_selection/sat_stan_model_selection_rstan.R \
  --seed=42 --chains=4 --iter=4000 --warmup=2000 \
  --adapt_delta=0.99 --max_treedepth=14 --prior_incl=0.50
```

---

## Opções (CLI)

Este script aceita opções no formato `--chave=valor`.

### Dados
- `--data_url=https://vincentarelbundock.github.io/Rdatasets/csv/mosaicData/SAT.csv`

### Priors / espaço de modelos
- `--prior_incl=0.50` *(probabilidade a priori de inclusão de cada preditor)*

### MCMC
- `--seed=42`
- `--chains=4`
- `--iter=4000`
- `--warmup=2000`
- `--adapt_delta=0.99`
- `--max_treedepth=14`

---

## O que o script entrega

Ao final, o relatório no console traz:

- resumo da base analítica
- tabela de diagnósticos MCMC/HMC por modelo
- PPC resumido por modelo
- comparação por evidência marginal e Bayes factors
- comparação por PSIS-LOO
- probabilidades posteriores de inclusão
- coeficientes via **Bayesian Model Averaging** na escala original
- identificação do melhor modelo por evidência e por LOO

Este exemplo é **console-only**: não grava artefatos em `outputs/`.
