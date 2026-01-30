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

---

## O que você deve ver ao final

Em geral, o script:

- lê o dataset de `data/raw/TherapeuticTouchData.csv`
- ajusta o(s) modelo(s) em Stan via `cmdstanr`
- imprime resumos e diagnósticos no console
- gera gráficos (dependendo do trecho habilitado no script)

Se você estiver salvando saídas, a convenção recomendada é:

- **Figuras / tabelas finais:** `outputs/therapeutic_touch/`
- **Relatórios (se houver):** `reports/`

> Se hoje o seu script ainda não está salvando nada, está tudo bem: eu recomendo evoluir para salvar figuras em `outputs/therapeutic_touch/` quando você começar a publicar/compartilhar resultados.

---

## Estrutura do exemplo

```
scripts/therapeutic_touch/
  therapeutic_touch.R
  README.md
data/raw/
  TherapeuticTouchData.csv
outputs/
  (opcional) therapeutic_touch/
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
