# RS Seguro

Scripts para o caso **RS Seguro**, ajustados ao padrão do repositório.

## Arquivos

### 1) M1 — série mensal por crime / macrocrime

- Script: `scripts/rs_seguro/rs_seguro_m1_dual_layer_cmdstanr.R`
- Entrada padrão: `data/raw/rs_seguro/rs_month_macrocrime.csv`
- Modo alternativo: `--analysis_layer=crime` requer `data/raw/rs_seguro/rs_month_crime.csv` (arquivo **não versionado** no repo atual) ou `--input_csv=...`
- Saídas padrão:
  - `outputs/tables/rs_seguro/rs_seguro_m1_macrocrime_*.csv`
  - `outputs/figures/rs_seguro/rs_seguro_m1_macrocrime_plots.pdf`

Exemplos:

```bash
Rscript scripts/rs_seguro/rs_seguro_m1_dual_layer_cmdstanr.R
```

```bash
Rscript scripts/rs_seguro/rs_seguro_m1_dual_layer_cmdstanr.R --analysis_layer=crime --input_csv=/caminho/para/rs_month_crime.csv
```

### 2) M2 — perfil da vítima condicionado a macrocrimes-alvo

- Script: `scripts/rs_seguro/rs_seguro_m2_macrocrime_conditional_cmdstanr.R`
- Entrada padrão: `data/raw/rs_seguro/rs_month_macrocrime_profile_v1_1vict.csv`
- Seleção dos macrocrimes-alvo:
  - via `--targets=A,B,C`, ou
  - via `--m1_targets_csv=outputs/tables/rs_seguro/rs_seguro_m1_macrocrime_crime_trend.csv`
- Saídas padrão:
  - `outputs/tables/rs_seguro/m2_macrocrime_conditional/*.csv`
  - `outputs/figures/rs_seguro/m2_macrocrime_conditional/*.pdf`

Exemplos:

```bash
Rscript scripts/rs_seguro/rs_seguro_m2_macrocrime_conditional_cmdstanr.R --targets=VULNERAVEIS_E_CUIDADO,ORDEM_PUBLICA_E_OUTROS
```

ou

```bash
Rscript scripts/rs_seguro/rs_seguro_m2_macrocrime_conditional_cmdstanr.R --m1_targets_csv=outputs/tables/rs_seguro/rs_seguro_m1_macrocrime_crime_trend.csv
```

## Observações

- Sem `setwd()`.
- Entradas ficam em `data/raw/rs_seguro/`.
- Saídas regeneráveis ficam em `outputs/`.
- O M1 roda por padrão em `macrocrime`; o nível `crime` depende de arquivo adicional ainda não versionado.
- Para instalar dependências:

```bash
Rscript scripts/_setup/install_deps.R
Rscript scripts/_setup/install_cmdstan.R
```
