# Mortality — Poisson hierárquico com offset (cmdstanr)

Este exemplo ajusta um modelo Bayesiano hierárquico Poisson para contagens (`y`) com **offset de população** (`log(pop)`), estimando uma taxa por município com *shrinkage*.

## Modelo (resumo)

- Observação: `y_i ~ Poisson(pop_i * rate_i)`
- Link: `log(rate_i) = alpha + eta_i`
- Efeito aleatório: `eta_i ~ Normal(0, sigma)`
- Parametrização **não-centrada**: `eta_i = sigma * eta_raw_i`, `eta_raw_i ~ Normal(0,1)`
- `alpha` tem prior centrado na *crude rate* observada (`sum(y)/sum(pop)`), com escala configurável

Além do ajuste, o script calcula:
- Diagnósticos MCMC: Rhat/ESS + divergências + treedepth + BFMI
- Benchmark Bayesiano: `P(rate_i > state_mean_draw)` onde `state_mean_draw = exp(alpha + 0.5*sigma^2)`
- PPC por município (intervalo preditivo 95%, p-values) e PPC global (total e proporção de zeros)
- Rankings de municípios por “excesso” e por PPC “estranho”

## Como rodar

No estado atual do script, ele lê um Excel (ver `cfg$excel_path`) e roda o MCMC.

```r
Rscript scripts/mortality/mortality_poisson_offset_cmdstanr_v2.R
```

> Observação: o script **não grava outputs** (sem CSV/plots exportados). O CmdStan pode usar arquivos temporários para compilar/rodar.

## Entradas esperadas (Excel)

O Excel deve conter (no `sheet` configurado) as colunas:

- contagem de eventos: `Contagem` (cfg$col_y)
- população/exposição: `POPULACAO` (cfg$col_pop)
- identificador do município: `CODMUNRES` (cfg$col_mun)

Se houver município repetido, o script agrega por município (soma `pop` e `y`).

## Dependências

- R packages: `cmdstanr`, `posterior`, `dplyr`, `tidyr`, `ggplot2`, `readxl`
- CmdStan instalado e configurado para o `cmdstanr`

Veja:
- `scripts/_setup/install_deps.R`
- `scripts/_setup/install_cmdstan.R`

## Saídas no console

O script imprime, em blocos:

1. Resumo dos dados
2. Diagnósticos do ajuste (MCMC)
3. Resumo “Estado” / benchmarks
4. Flags de excesso
5. Top municípios por excesso
6. PPC global
7. Top “estranhos” no PPC
8. Tabela completa de resultados (ou parcial)
9. Plots (no device gráfico)
