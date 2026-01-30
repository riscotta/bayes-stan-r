# Datasets (raw)

Esta pasta contém dados **brutos** usados pelos exemplos em `scripts/`.

A ideia é: **não editar manualmente** os arquivos aqui.  
Se precisar limpeza, transformação ou feature engineering, gere um arquivo derivado em `data/processed/` (ou salve em `outputs/` se for algo específico do experimento).

---

## Arquivos

### `TherapeuticTouchData.csv`

- **Usado em:** `scripts/therapeutic_touch/therapeutic_touch.R`
- **Descrição:** dataset do experimento “Therapeutic Touch”, usado como exemplo didático em modelagem bayesiana.
- **Fonte:** *(preencher com a referência original / link de onde foi obtido)*
- **Licença:** *(preencher; importante para redistribuição pública)*
- **Observações:**
  - mantenha este arquivo como “raw”
  - se houver uma versão limpa/padronizada, crie em `data/processed/therapeutic_touch/`

---

### `BattingAverage.csv`

- **Usado em:** `scripts/hierarchical/baseball_batting_by_position_3level_cmdstanr.R`
- **Descrição:** dataset para batting average (jogador/posição), usado para demonstrar modelo hierárquico em 3 níveis.
- **Fonte:** *(preencher com a referência original / link de onde foi obtido)*
- **Licença:** *(preencher; importante para redistribuição pública)*
- **Observações:**
  - mantenha este arquivo como “raw”
  - se houver uma versão processada (ex.: filtros/recortes), gerar em `data/processed/baseball/`

---

## Convenções recomendadas

- **Nomes de arquivos:** `CamelCase` ou `snake_case` (mas manter consistente no repo)
- **Encoding:** UTF-8
- **Separador:** vírgula (CSV)
- **Header:** primeira linha com nomes de colunas
- **Tamanho:** se um dataset crescer muito, prefira um script de download em `scripts/_setup/get_data.R` e ignore o arquivo grande no git.

