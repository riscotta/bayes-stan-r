# DETER mensal por bioma-UF

Modelo Bayesiano em **Stan** via **cmdstanr** para comparar dois ciclos de alertas do **DETER** agregados por **bioma::UF** e mês.

O script:

- lê um CSV mensal por bioma-UF
- padroniza colunas e nomes de meses
- reconstrói `ciclo` quando a coluna não existe
- filtra os meses **Ago, Set, Out, Nov, Dez e Jan**
- ajusta um modelo hierárquico com efeito global de ciclo, efeito por estrato e efeito mensal
- produz diagnósticos, cobertura preditiva, PIT e gráficos de PPC

## Arquivos

- Script: `scripts/deter_mensal_bioma_uf/deter_mensal_bioma_uf_cmdstanr.R`
- Entrada padrão: `data/raw/deter_mensal_bioma_uf.csv`
- Saídas opcionais:
  - `outputs/tables/deter_mensal_bioma_uf/deter_mensal_bioma_uf_report.txt`
  - `outputs/figures/deter_mensal_bioma_uf/deter_mensal_bioma_uf_plots.pdf`

## Como rodar

A partir do **root** do repositório:

```bash
Rscript scripts/deter_mensal_bioma_uf/deter_mensal_bioma_uf_cmdstanr.R
```

Com arquivo de entrada explícito:

```bash
Rscript scripts/deter_mensal_bioma_uf/deter_mensal_bioma_uf_cmdstanr.R --input_csv=data/raw/deter_mensal_bioma_uf.csv
```

Com exportação de artefatos:

```bash
Rscript scripts/deter_mensal_bioma_uf/deter_mensal_bioma_uf_cmdstanr.R   --target_cycles=2024/25,2025/26   --save_plots=1   --save_report=1
```

## Colunas esperadas no CSV

O script aceita nomes alternativos, mas espera equivalentes para:

- `bioma`
- `uf`
- `ano`
- `mes`
- `area_alerta_km2`
- `ciclo` *(opcional; se ausente, o script reconstrói a partir de ano+mês)*

## Opções principais

- `--input_csv=...`
- `--target_cycles=2024/25,2025/26`
- `--seed=20260316`
- `--chains=4`
- `--parallel_chains=4`
- `--iter_warmup=1500`
- `--iter_sampling=1500`
- `--adapt_delta=0.99`
- `--max_treedepth=15`
- `--run_prior_predictive=1`
- `--prior_draws=1000`
- `--posterior_draws_for_ppc=300`
- `--save_plots=1`
- `--save_report=1`
- `--keep_temp_files=1`

## Observações

- Sem `setwd()`.
- O script deve ser rodado a partir da raiz do repo.
- Por padrão, ele imprime o relatório no console e **não salva** artefatos finais.
- Os CSVs temporários do CmdStan são limpos ao final, a menos que `--keep_temp_files=1`.
- Se o CSV não estiver versionado no repo, coloque-o em `data/raw/` localmente ou informe `--input_csv=...`.
