# Datasets (raw)

Esta pasta contém dados **brutos** usados pelos exemplos em `scripts/`.

A ideia é: **não editar manualmente** os arquivos aqui. Se precisar limpeza, transformação ou feature engineering, gere um arquivo derivado em `data/processed/` (ou salve em `outputs/` se for algo específico do experimento).

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


### `isus_sia/ISUS_SIA_PARS.zip`

- **Usado em:** `scripts/isus_sia/mc_isus_sia.R`
- **Descrição:** dataset (ZIP contendo `ISUS_SIA_PARS.csv` com separador `;`) para análise de precificação via Monte Carlo robusto (cluster bootstrap por **CNES**) no contexto ISUS/SIA.
- **Fonte:** *(não documentada no repo — adicione quando possível)*
- **Licença / restrições:** *(não documentada no repo — verifique permissões e LGPD antes de publicar)*
- **Observações:**
  - mantenha este arquivo como “raw”
  - o script aceita CSV direto (`--csv_path=...`) ou ZIP (`--zip_path=...`) e extrai para um diretório temporário
  - se houver limpeza/padronização, gerar derivado em `data/processed/isus_sia/`
  - se o dataset for sensível ou privado, prefira **não versionar** e substitua por um script de ingestão/download em `scripts/_setup/`

---

### `SAheart.data` (opcional)

- **Usado em:** `scripts/saheart/saheart_logistic_cmdstanr.R`
- **Descrição:** dataset **SAheart** (Elemental Statistics Learning) para regressão logística Bayesiana (Stan/cmdstanr).
- **Fonte:** *Hastie, Tibshirani, Friedman (ESL) — arquivo SAheart.data (download público).*
- **Licença / restrições:** *(verificar termos da fonte; por padrão este repo não versiona este arquivo)*
- **Observações:**
  - este arquivo é **opcional** e não vem no repo por padrão
  - se ele **não existir**, o script tenta baixar automaticamente
  - para rodar offline, baixe e coloque em `data/raw/SAheart.data`
  - para evitar commits acidentais, este caminho é ignorado no `.gitignore`

---

### `rs_seguro/rs_month_macrocrime.csv`

- **Usado em:** `scripts/rs_seguro/rs_seguro_m1_dual_layer_cmdstanr.R`
- **Descrição:** série mensal agregada por **macrocrime**, com contagens de ocorrências (`occ`) e vítimas (`vit`).
- **Fonte:** *(dataset operacional do projeto RS Seguro; detalhe a origem/sistema e permissões de uso quando possível)*
- **Licença / restrições:** *(verificar política institucional antes de redistribuir dados operacionais)*
- **Observações:**
  - este é o input padrão do M1 com `--analysis_layer=macrocrime`
  - colunas esperadas: `ym`, `crime`, `occ`, `vit`
  - mantenha este arquivo como “raw”

---

### `rs_seguro/rs_month_macrocrime_profile_v1_1vict.csv`

- **Usado em:** `scripts/rs_seguro/rs_seguro_m2_macrocrime_conditional_cmdstanr.R`
- **Descrição:** série mensal com perfil da vítima por macrocrime, sexo e faixa etária, usando contagem `occ_1_vitima`.
- **Fonte:** *(dataset operacional do projeto RS Seguro; detalhe a origem/sistema e permissões de uso quando possível)*
- **Licença / restrições:** *(verificar política institucional antes de redistribuir dados operacionais)*
- **Observações:**
  - este é o input padrão do M2
  - colunas esperadas: `ym`, `crime`, `sexo`, `faixa`, `occ_1_vitima`
  - mantenha este arquivo como “raw”

---

### `rs_seguro/rs_month_crime.csv` (opcional / não versionado)

- **Usado em:** `scripts/rs_seguro/rs_seguro_m1_dual_layer_cmdstanr.R` quando `--analysis_layer=crime`
- **Descrição:** série mensal no nível de crime detalhado, alternativa ao arquivo agregado por macrocrime.
- **Fonte:** *(não versionado no repo atual; documente a origem quando disponibilizar)*
- **Licença / restrições:** *(verificar política institucional antes de versionar/publicar)*
- **Observações:**
  - **este arquivo não está no repositório atual**
  - para usar o modo `crime`, forneça `--input_csv=...` ou adicione o arquivo em `data/raw/rs_seguro/`
  - mantenha o mesmo contrato mínimo de colunas: `ym`, `crime`, `occ`, `vit`



### `deter_mensal_bioma_uf.csv` (opcional / não versionado)

- **Usado em:** `scripts/deter_mensal_bioma_uf/deter_mensal_bioma_uf_cmdstanr.R`
- **Descrição:** série mensal de alertas DETER agregada por `bioma`, `uf`, `ano`, `mes` e área de alerta em km².
- **Fonte:** *(documente a origem exata do extrato quando possível)*
- **Licença / restrições:** *(verifique a política de redistribuição dos dados antes de versionar no repo público)*
- **Observações:**
  - este arquivo é **opcional** e não vem no repo por padrão
  - o script aceita nomes alternativos de colunas e reconstrói `ciclo` quando a coluna não existe
  - o recorte analítico usa, por padrão, os meses `Ago, Set, Out, Nov, Dez, Jan`
  - para rodar localmente, coloque o CSV em `data/raw/deter_mensal_bioma_uf.csv` ou informe `--input_csv=...`

## Convenções recomendadas

- **Nomes de arquivos:** `CamelCase` ou `snake_case` (mas manter consistente no repo)
- **Encoding:** UTF-8
- **Separador:** vírgula (preferencial); quando necessário `;` (padrão PT-BR)
- **Header:** primeira linha com nomes de colunas
- **Tamanho:** se um dataset crescer muito, prefira um script de download em `scripts/_setup/get_data.R` e ignore o arquivo grande no git.


---

### `data/raw/censo_escolar/` (cache local opcional / ignorado no git)

- **Usado em:** `scripts/censo_escolar_tempo_integral/censo_escolar_tempo_integral_cmdstanr.R`
- **Descrição:** cache local dos ZIPs oficiais do **Censo Escolar** baixados automaticamente pelo script.
- **Fonte:** página oficial de microdados do INEP — `https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-escolar`
- **Licença / restrições:** verificar termos e avisos publicados pelo INEP para redistribuição e uso dos microdados.
- **Observações:**
  - esta pasta existe apenas para **cache local**
  - os arquivos grandes baixados aqui **não devem ser versionados**
  - por padrão o script reaproveita os ZIPs já baixados; use `--force_download=1` para renovar o cache
