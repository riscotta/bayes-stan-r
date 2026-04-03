# PMS / Serviços em janeiro de 2026

Script Bayesiano em **Stan** via **rstan** para reproduzir a manchete da **Pesquisa Mensal de Serviços (PMS)** com base analítica já preparada e versionada no repositório.

O script:

- lê a base analítica consolidada da PMS
- valida a estrutura esperada da base
- seleciona a série **Brasil / Total** com índice sazonalmente ajustado
- verifica aritmeticamente a manchete de jan/2026
- ajusta um modelo Bayesiano de **nível ancorado + tendência AR(1)**
- imprime diagnóstico MCMC e relatório final apenas no console

## Arquivos

- Script: `scripts/pms_servicos/pms_servicos_rstan.R`
- Entrada padrão: `data/raw/pms_servicos/`
- Arquivo esperado:
  - `pms_base_analitica_stan.csv`

## Como rodar

A partir do **root** do repositório:

```bash
Rscript scripts/pms_servicos/pms_servicos_rstan.R
```

Com argumentos explícitos:

```bash
Rscript scripts/pms_servicos/pms_servicos_rstan.R   --input_dir=data/raw/pms_servicos   --input_csv=pms_base_analitica_stan.csv   --refresh=50
```

## Opções principais

- `--input_dir=data/raw/pms_servicos`
- `--input_csv=pms_base_analitica_stan.csv`
- `--seed=20260403`
- `--iter_warmup=1500`
- `--iter_sampling=1500`
- `--chains=4`
- `--adapt_delta=0.995`
- `--max_treedepth=13`
- `--cores=...`
- `--refresh=100`
- `--tol_recorde_pct=0.05`

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
- O CSV de entrada deste estudo **está versionado** em `data/raw/pms_servicos/`, o que permite rodar o exemplo logo após o clone do repositório.
