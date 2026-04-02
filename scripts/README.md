# Scripts

Aqui ficam os scripts “executáveis” do repositório.

**Regra geral:** rode a partir do **root** do repo (sem `setwd()`):

```bash
Rscript scripts/<pasta>/<script>.R
```

## Setup (uma vez por máquina)

- Instalar dependências R:

```bash
Rscript scripts/_setup/install_deps.R
```

- Se você pretende rodar o exemplo **INHALER** usando `brms::inhaler`, ou usar testes/relatórios, prefira:

```bash
Rscript scripts/_setup/install_deps.R --all
```

- Instalar CmdStan (necessário para `cmdstanr`):

```bash
Rscript scripts/_setup/install_cmdstan.R
```

## Exemplos disponíveis

### 1) Therapeutic Touch

- Pasta: `scripts/therapeutic_touch/`
- Entrada: `scripts/therapeutic_touch/therapeutic_touch.R`
- Dados: `data/raw/TherapeuticTouchData.csv`
- Saídas (quando rodado via `Rscript`):
  - `outputs/figures/therapeutic_touch_plots.pdf`
  - `outputs/tables/therapeutic_touch_report.txt`

### 2) Hierárquico (Baseball: 3 níveis)

- Pasta: `scripts/hierarchical/`
- Entrada: `scripts/hierarchical/baseball_batting_by_position_3level_cmdstanr.R`
- Dados: `data/raw/BattingAverage.csv`
- Observação: *console-only* (não grava arquivos por padrão)


### 3) Mortalidade (Poisson hierárquico com offset)

- Pasta: `scripts/mortality/`
- Entrada: `scripts/mortality/mortality_poisson_offset_cmdstanr_v2.R`
- Dados: `data/raw/mortality/Dados_Mortalidade.xlsx` *(ou ajuste via `--excel_path=...`)*
- Observação: *console-only* (não grava arquivos por padrão)


### 4) ISUS / SIA — Monte Carlo robusto (cluster bootstrap por CNES)

- Pasta: `scripts/isus_sia/`
- Entrada: `scripts/isus_sia/mc_isus_sia.R`
- Dados: `data/raw/isus_sia/ISUS_SIA_PARS.zip` *(contém `ISUS_SIA_PARS.csv`)*
- Observação: *console-only* (não grava arquivos por design)

### 5) SAheart (regressão logística Bayesiana)

- Pasta: `scripts/saheart/`
- Entrada: `scripts/saheart/saheart_logistic_cmdstanr.R`
- Dados (opcional): `data/raw/SAheart.data` *(se não existir, o script tenta baixar automaticamente)*
- Observação: *console-only* (não grava arquivos por padrão)

### 6) Deck — Quantas partidas para chegar em r vitórias?

- Pasta: `scripts/deck_15wins/`
- Entrada: `scripts/deck_15wins/deck_15wins_negbin_beta_cmdstanr.R`
- Dados: (simulados no próprio script)
- Observação: *console-only* (não grava arquivos por padrão)


### 7) Hidrômetro — degradação/erro de medição (hierárquico)

- Pasta: `scripts/hidrometro_degradacao/`
- Entrada: `scripts/hidrometro_degradacao/hidrometro_degradacao_cmdstanr.R`
- Dados: (simulados no próprio script)
- Observação: *console-only* por padrão; opcionalmente salva PDF/TXT em `outputs/` via `--save_plots=1` e `--save_report=1`

### 8) Média vs Ruído (μ) — “o ruído se cancela” com N

- Pasta: `scripts/media_ruido_mu/`
- Entrada: `scripts/media_ruido_mu/media_ruido_mu_cmdstanr.R`
- Dados: (simulados no próprio script)
- Saídas (quando rodado via `Rscript`):
  - `outputs/figures/media_ruido_mu_plots.pdf`
  - `outputs/tables/media_ruido_mu_sim_summary.csv`
  - `outputs/tables/media_ruido_mu_posterior_summary.csv`


### 9) Coortes de Aposentadoria (Brasil) — 30 anos (retornos reais)

- Pasta: `scripts/coortes_aposentadoria/`
- Entrada: `scripts/coortes_aposentadoria/coortes_aposentadoria_console_only_v2.R`
- Dados: (baixados automaticamente via Yahoo Finance + BCB/SGS)
- Observação: *console-only* (não grava arquivos por design)

### 10) Kidney — survival (lognormal com censura à direita)

- Pasta: `scripts/kidney_survival/`
- Entrada: `scripts/kidney_survival/kidney_lognormal_survival_cmdstanr.R`
- Dados: `survival::kidney` (embutido no pacote)
- Observação: *console-only* (não grava arquivos por padrão; usa tempdir)

### 11) INHALER — ordinal crossover (cutpoints via softplus)

- Pasta: `scripts/inhaler_ordinal/`
- Entrada: `scripts/inhaler_ordinal/inhaler_ordinal_softplus_cmdstanr.R`
- Dados:
  - `brms::inhaler` (embutido no pacote **brms**) **ou**
  - `data/raw/inhaler.csv` (opcional, via `--data_path=...`)
- Saídas (padrão):
  - `outputs/figures/inhaler_ordinal_plots.pdf`
  - `outputs/tables/inhaler_ordinal_report.txt`

### 12) RS Seguro — séries mensais e perfil da vítima

- Pasta: `scripts/rs_seguro/`
- Entradas principais:
  - `scripts/rs_seguro/rs_seguro_m1_dual_layer_cmdstanr.R`
  - `scripts/rs_seguro/rs_seguro_m2_macrocrime_conditional_cmdstanr.R`
- Dados:
  - `data/raw/rs_seguro/rs_month_macrocrime.csv`
  - `data/raw/rs_seguro/rs_month_macrocrime_profile_v1_1vict.csv`
  - `data/raw/rs_seguro/rs_month_crime.csv` *(opcional / não versionado no repo atual; exigido apenas se `--analysis_layer=crime` no M1)*
- Saídas (quando rodado via `Rscript`):
  - `outputs/tables/rs_seguro/*.csv`
  - `outputs/figures/rs_seguro/*.pdf`


### 16) SAT — seleção Bayesiana de variáveis

- Pasta: `scripts/sat_model_selection/`
- Entrada: `scripts/sat_model_selection/sat_stan_model_selection_rstan.R`
- Dados: consulta online ao Rdatasets (`mosaicData::SAT`)
- Observação: *console-only* (não grava arquivos por padrão); usa **rstan** + **bridgesampling**

## Convenções

- **1 pasta = 1 tema/exemplo** (com `README.md` curto)
- preferir nomes descritivos (e, se houver pipeline, prefixos `01_`, `02_`...)
- artefatos regeneráveis vão em `outputs/`

### 13) DETER mensal por bioma-UF

- Pasta: `scripts/deter_mensal_bioma_uf/`
- Entrada: `scripts/deter_mensal_bioma_uf/deter_mensal_bioma_uf_cmdstanr.R`
- Dados: `data/raw/deter_mensal_bioma_uf.csv` *(opcional / não versionado no repo atual)*
- Observação: imprime relatório no console; opcionalmente salva PDF/TXT em `outputs/` via `--save_plots=1` e `--save_report=1`

### 14) IPCA / SIDRA 1419 -> Stan

- Pasta: `scripts/ipca_sidra_1419/`
- Entrada: `scripts/ipca_sidra_1419/ipca_sidra_1419_cmdstanr.R`
- Dados: consulta online ao SIDRA/IBGE *(tabelas 1737 e 7060)*
- Saídas opcionais:
  - `outputs/tables/ipca_sidra_1419/ipca_sidra_1419_report.txt`
  - `outputs/tables/ipca_sidra_1419/*.csv`

### 15) Censo Escolar 2021-2025 -> tempo integral na rede publica

- Pasta: `scripts/censo_escolar_tempo_integral/`
- Entrada: `scripts/censo_escolar_tempo_integral/censo_escolar_tempo_integral_cmdstanr.R`
- Dados: downloads oficiais do INEP com cache local em `data/raw/censo_escolar/` *(ignorado no git)*
- Saidas opcionais:
  - `outputs/tables/censo_escolar_tempo_integral/censo_escolar_tempo_integral_report.txt`
  - `outputs/tables/censo_escolar_tempo_integral/*.csv`

### 17) FruitFlies — replicação AFT log-normal

- Pasta: `scripts/fruitflies_aft/`
- Entrada: `scripts/fruitflies_aft/fruitflies_aft_lognormal_cmdstanr.R`
- Dados: `Stat2Data::FruitFlies`
- Saídas opcionais:
  - `outputs/figures/fruitflies_aft/fruitflies_aft_plots.pdf`
  - `outputs/tables/fruitflies_aft/fruitflies_aft_report.txt`
