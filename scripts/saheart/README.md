# SAheart (exemplo)

Este exemplo ajusta uma **regressão logística Bayesiana** para o dataset **SAheart** (Elemental Statistics Learning), usando **Stan** via `cmdstanr`.

- **Modelo:** `chd ~ Bernoulli_logit(alpha + X * beta)`
- **Console-only:** não grava artefatos (resultados são impressos no console)
- **Stan inline:** o modelo é definido no script e compilado via `cmdstanr::write_stan_file()`

> **Script:** `scripts/saheart/saheart_logistic_cmdstanr.R`  
> **Dados (preferencial):** `data/raw/SAheart.data` *(CSV com `,` e `row.names` na 1ª coluna, como no ESL)*  

---

## Pré-requisitos

No diretório raiz do repositório (root), rode:

1) Dependências R:

```bash
Rscript scripts/_setup/install_deps.R --all
```

2) CmdStan (necessário para `cmdstanr`):

```bash
Rscript scripts/_setup/install_cmdstan.R
```

---

## Como rodar

A partir do root do repo:

```bash
Rscript scripts/saheart/saheart_logistic_cmdstanr.R
```

### Observação sobre os dados (online/offline)

- Se `data/raw/SAheart.data` **existir**, o script lê do disco.
- Caso **não exista**, o script tenta baixar automaticamente do URL do ESL.

Para rodar **offline**, baixe o dataset e coloque em `data/raw/SAheart.data` (ou passe `--data=...`).

---

## Opções (CLI)

Este script aceita opções no formato `--chave=valor`.

Exemplo típico:

```bash
Rscript scripts/saheart/saheart_logistic_cmdstanr.R \
  --chains=4 --iter_warmup=1000 --iter_sampling=1000 \
  --adapt_delta=0.99 --max_treedepth=12 --seed=123
```

Opções aceitas hoje (veja também o cabeçalho do script):

### Dados
- `--data=data/raw/SAheart.data`
- `--data_url=https://hastie.su.domains/ElemStatLearn/datasets/SAheart.data`
- `--scale_predictors=0|1` *(padrão: 1)*
- `--prior_only=0|1` *(padrão: 0)*

### Priors
- `--alpha_scale=1.5` *(alpha ~ Normal(0, alpha_scale))*
- `--beta_scale=1.0` *(beta  ~ Normal(0, beta_scale))*

### MCMC
- `--seed=123`
- `--chains=4`
- `--iter_warmup=1000`
- `--iter_sampling=1000`
- `--adapt_delta=0.95`
- `--max_treedepth=12`
- `--refresh=0`

---

## O que você deve ver ao final

Em geral, o script:

- lê e prepara o dataset (inclui `famhist` como binário)
- (opcionalmente) padroniza preditores (`--scale_predictors=1`)
- compila e ajusta o modelo em Stan via `cmdstanr`
- executa diagnósticos MCMC (Rhat/ESS, divergências, treedepth, BFMI)
- imprime no console:
  - sumário de `alpha` e `beta`
  - PPC básico (prevalência replicada, Brier)
  - AUC (via ranking)
  - calibração por bins (ECE)

Este exemplo é **console-only** (não grava arquivos por padrão).

---

## Estrutura do exemplo

```
scripts/saheart/
  saheart_logistic_cmdstanr.R
  README.md
data/raw/
  SAheart.data   (opcional; caso não exista, o script tenta baixar)
```

---

## Fonte / referência do dataset

- ESL (Elemental Statistics Learning) — dataset **SAheart** (Hastie, Tibshirani, Friedman).

