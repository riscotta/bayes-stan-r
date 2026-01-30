# Hierarchical models (exemplo)

Este exemplo foca em **modelos hierárquicos (multinível)** usando **Stan** via `cmdstanr`, com um caso didático de **baseball batting average** em **3 níveis**:

1) **Global (liga)**  
2) **Por posição**  
3) **Por jogador**

O objetivo é mostrar como “partial pooling” melhora estimativas quando há poucos dados por jogador, mantendo variação real entre grupos.

> **Script:** `scripts/hierarchical/baseball_batting_by_position_3level_cmdstanr.R`  
> **Dados:** `raw/BattingAverage.csv`

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
Rscript scripts/hierarchical/baseball_batting_by_position_3level_cmdstanr.R
```

---

## Opções (CLI)

Este script aceita opções no formato `--chave=valor` (ex.: número de cadeias, iterações, controle de adaptação).  
Exemplo típico:

```bash
Rscript scripts/hierarchical/baseball_batting_by_position_3level_cmdstanr.R \
  --chains=4 --iter_warmup=1000 --iter_sampling=1000 --adapt_delta=0.99
```

> Dica: se você quiser que eu documente **todas as opções aceitas**, eu extraio direto do `parse_args()` do script e coloco aqui “bonitinho”.

---

## O que você deve ver ao final

Em geral, o script:

- lê o dataset de `raw/BattingAverage.csv`
- prepara índices (posição/jogador) para o modelo hierárquico
- compila e ajusta o modelo em Stan via `cmdstanr`
- executa diagnósticos (ex.: resumos, QC, e pode incluir LOO)
- imprime resultados no console e (dependendo do trecho habilitado) gera gráficos

Se você estiver salvando saídas, a convenção recomendada é:

- **Figuras / tabelas finais:** `outputs/hierarchical/`
- **Relatórios (se houver):** `reports/`

---

## Estrutura do exemplo

```
scripts/hierarchical/
  baseball_batting_by_position_3level_cmdstanr.R
  README.md
raw/
  BattingAverage.csv
outputs/
  (opcional) hierarchical/
```

---

## Dicas práticas

- Rode sempre a partir do **root** do repositório (evite `setwd()`).
- Quando você criar novos exemplos hierárquicos (Eight Schools, Radon etc.), recomendo:
  - manter **1 pasta = 1 exemplo**, com um `README.md` curto
  - padronizar nomes `01_...`, `02_...` se tiver pipeline em múltiplos scripts
  - mover funções utilitárias para `R/` (ex.: plots, checagens, helpers de indexação)

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
