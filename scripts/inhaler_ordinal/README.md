# INHALER — ordinal crossover (cutpoints via softplus)

Este exemplo ajusta um **modelo ordinal (ordered logistic)** para um **crossover 2×2** (dois tratamentos, dois períodos) com:

- **random intercept por sujeito**
- **cutpoints** parametrizados como `c1 + gaps` com **softplus** (estável, sem *hard-bounds*)
- checagens no **holdout** (PIT randomizado, Dunn–Smyth, calibração por thresholds, métricas)

> **Onde está o script:** `scripts/inhaler_ordinal/inhaler_ordinal_softplus_cmdstanr.R`

---

## Pré-requisitos

No **root** do repositório:

```bash
Rscript scripts/_setup/install_deps.R
Rscript scripts/_setup/install_cmdstan.R
```

### Sobre os dados (`inhaler`)

Por padrão, o script tenta carregar o dataset **`inhaler`** do pacote **`brms`**.

- Se você **não** tem `brms`, instale:

```r
install.packages("brms")
```

- Alternativa: coloque um CSV em `data/raw/inhaler.csv` (mesmas colunas do dataset) e rode com:

```bash
Rscript scripts/inhaler_ordinal/inhaler_ordinal_softplus_cmdstanr.R --data_path=data/raw/inhaler.csv
```

---

## Como rodar

A partir do root do repo:

```bash
Rscript scripts/inhaler_ordinal/inhaler_ordinal_softplus_cmdstanr.R
```

### Opções (formato `--chave=valor`)

- Amostragem:
  - `--seed=20260302`
  - `--chains=4`
  - `--iter_warmup=1000`
  - `--iter_sampling=1000`
  - `--adapt_delta=0.995`
  - `--max_treedepth=15`
  - `--refresh=0`

- Holdout / avaliação:
  - `--prob_draws=1000`  (subamostra de draws para calcular probabilidades no teste)

- Saídas:
  - `--save_plots=1` | `0`
  - `--plots_path=outputs/figures/inhaler_ordinal_plots.pdf`
  - `--save_report=1` | `0`
  - `--report_path=outputs/tables/inhaler_ordinal_report.txt`

- Dados:
  - `--data_path=data/raw/inhaler.csv`  (se existir, usa esse CSV)

---

## Saídas

Quando `--save_plots=1` e `--save_report=1`, o exemplo salva:

- `outputs/figures/inhaler_ordinal_plots.pdf`
- `outputs/tables/inhaler_ordinal_report.txt`

---

## Estrutura

```
scripts/inhaler_ordinal/
  inhaler_ordinal_softplus_cmdstanr.R
  README.md
outputs/
  figures/inhaler_ordinal_plots.pdf
  tables/inhaler_ordinal_report.txt
```
