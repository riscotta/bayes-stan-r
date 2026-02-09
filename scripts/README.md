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
- Entrada: `scripts/isus_sia/MC_patched.R`
- Dados: `data/raw/isus_sia/ISUS_SIA_PARS.csv`
- Observação: *console-only* (não grava arquivos por design)


## Convenções

- **1 pasta = 1 exemplo** (com `README.md` curto)
- preferir nomes descritivos (e, se houver pipeline, prefixos `01_`, `02_`...)
- artefatos regeneráveis vão em `outputs/`
