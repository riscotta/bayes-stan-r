# Datasets (raw)

Esta pasta contém dados **brutos** usados pelos exemplos em `scripts/`.

A ideia é: **não editar manualmente** os arquivos aqui.  
Se precisar limpeza, transformação ou feature engineering, gere um arquivo derivado em `data/processed/` (ou salve em `outputs/` se for algo específico do experimento).

---

## Arquivos

### `TherapeuticTouchData.csv`

- **Usado em:** `scripts/therapeutic_touch/therapeutic_touch.R`
- **Descrição:** dataset do experimento “Therapeutic Touch”, usado como exemplo didático em modelagem bayesiana.
- **Fonte:** *Rosa L, Rosa E, Sarner L, Barrett S. A Close Look at Therapeutic Touch. JAMA. 1998;279(13):1005–1010.*
- **Licença:** *MIT License*
- **Observações:**
  - mantenha este arquivo como “raw”
  - se houver uma versão limpa/padronizada, crie em `data/processed/therapeutic_touch/`

---

### `BattingAverage.csv`

- **Usado em:** `scripts/hierarchical/baseball_batting_by_position_3level_cmdstanr.R`
- **Descrição:** dataset para batting average (jogador/posição), usado para demonstrar modelo hierárquico em 3 níveis.
- **Fonte:** *Dados de batting da MLB (temporada regular 2012) conforme usados por Kruschke; coletados do ESPN (conforme indicado pelo autor em post técnico).*
- **Licença:** *MIT License*
- **Observações:**
  - mantenha este arquivo como “raw”
  - se houver uma versão processada (ex.: filtros/recortes), gerar em `data/processed/baseball/`

---


### `mortality/Dados_Mortalidade.xlsx`

- **Usado em:** `scripts/mortality/mortality_poisson_offset_cmdstanr_v2.R`
- **Descrição:** planilha com contagem de eventos e população por município (offset), usada para demonstrar modelo Poisson hierárquico com *shrinkage*.
- **Fonte:** *(não documentada no repo — adicione quando possível)*
- **Licença:** *(não documentada no repo — adicione quando possível)*
- **Observações:**
  - manter este arquivo como “raw”
  - o script espera (no `sheet` configurado) as colunas `Contagem`, `POPULACAO`, `CODMUNRES`
  - se houver padronização/limpeza/recortes, gerar um derivado em `data/processed/mortality/`

---


## Convenções recomendadas

- **Nomes de arquivos:** `CamelCase` ou `snake_case` (mas manter consistente no repo)
- **Encoding:** UTF-8
- **Separador:** vírgula (CSV)
- **Header:** primeira linha com nomes de colunas
- **Tamanho:** se um dataset crescer muito, prefira um script de download em `scripts/_setup/get_data.R` e ignore o arquivo grande no git.
