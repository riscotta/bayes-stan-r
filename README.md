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

## Convenções

- **1 pasta = 1 exemplo** (com `README.md` curto)
- preferir nomes descritivos (e, se houver pipeline, prefixos `01_`, `02_`...)
- artefatos regeneráveis vão em `outputs/`
