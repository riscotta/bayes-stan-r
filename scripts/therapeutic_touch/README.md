# Therapeutic Touch (exemplo)

Este exemplo reproduz/análise o dataset clássico de **Therapeutic Touch** usando modelos bayesianos no **Stan** via `cmdstanr`.  
A ideia é comparar abordagens **pooled / unpooled / hierarchical** (dependendo do script) e avaliar ajuste com **diagnósticos** (ex.: PPC e LOO).

> **Onde está o script:** `scripts/therapeutic_touch/therapeutic_touch.R`  
> **Dados:** `data/raw/TherapeuticTouchData.csv`

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
Rscript scripts/therapeutic_touch/therapeutic_touch.R
```

Por padrão, quando você roda via `Rscript` (não-interativo), o script salva:

- `outputs/figures/therapeutic_touch_plots.pdf`
- `outputs/tables/therapeutic_touch_report.txt`

Se você não quiser salvar artefatos e preferir somente console:

```bash
Rscript scripts/therapeutic_touch/therapeutic_touch.R --no-save
```

---

## O que você deve ver ao final

Em geral, o script:

- lê o dataset de `data/raw/TherapeuticTouchData.csv`
- ajusta o(s) modelo(s) em Stan via `cmdstanr`
- imprime resumos e diagnósticos no console
- gera gráficos (dependendo do trecho habilitado no script)

Quando salva artefatos (via `Rscript`), este exemplo usa:

- `outputs/figures/therapeutic_touch_plots.pdf`
- `outputs/tables/therapeutic_touch_report.txt`

---

## Estrutura do exemplo

```
scripts/therapeutic_touch/
  therapeutic_touch.R
  README.md
data/raw/
  TherapeuticTouchData.csv
outputs/
  figures/therapeutic_touch_plots.pdf
  tables/therapeutic_touch_report.txt
```

---

## Dicas práticas

- Rode sempre a partir do **root** do repositório (evite `setwd()`).
- Se você pretende transformar isso em “exemplo padrão” do repo, o próximo passo natural é:
  - padronizar `seed`
  - criar um “entrypoint” `run.R` (opcional) que só chama as etapas em ordem
  - mover funções utilitárias para `R/` (ex.: checagens, plots, helpers)

---

## Troubleshooting

### `CmdStan não encontrado` / erro de compilação
- Rode:

```bash
Rscript scripts/_setup/install_cmdstan.R
```

- Em Windows, confirme que o **RTools** está instalado e no PATH.

### Pacotes R faltando
- Rode:

```bash
Rscript scripts/_setup/install_deps.R --all
```

---

## Referências

- `cmdstanr`: interface R para CmdStan/Stan
- Dataset “Therapeutic Touch”: usado como exemplo didático em modelagem/avaliação bayesiana
