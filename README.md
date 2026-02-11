# bayes-stan-r

Repositório **simples** de scripts em **R** (com alguns modelos **Stan** via `cmdstanr`) para análises bayesianas, simulação, modelagem e diagnósticos (PPC/LOO/QC).

**Filosofia do repo:**

- não é um pacote R (sem `DESCRIPTION`, sem instalação)
- a pessoa baixa/clona e roda com `Rscript ...` a partir do **root** do repositório
- dados pequenos e exemplos didáticos ficam versionados em `data/raw/`

---

## O que tem hoje

Exemplos principais:

1) **Therapeutic Touch** — pooled vs hierárquico, prior/posterior predictive + LOO
   - Script: `scripts/therapeutic_touch/therapeutic_touch.R`
   - Dados: `data/raw/TherapeuticTouchData.csv`

2) **Baseball (3 níveis)** — liga → posição → jogador (binomial-logit hierárquico)
   - Script: `scripts/hierarchical/baseball_batting_by_position_3level_cmdstanr.R`
   - Dados: `data/raw/BattingAverage.csv`


3) **Mortalidade** — Poisson hierárquico com offset (população)
   - Script: `scripts/mortality/mortality_poisson_offset_cmdstanr_v2.R`
   - Dados: `data/raw/mortality/Dados_Mortalidade.xlsx` *(ou ajuste via `--excel_path=...`)*

4) **ISUS / SIA** — Monte Carlo robusto (cluster bootstrap por CNES) para precificação (SIGTAP)
   - Script: `scripts/isus_sia/mc_isus_sia.R`
   - Dados: `data/raw/isus_sia/ISUS_SIA_PARS.zip` *(contém `ISUS_SIA_PARS.csv`)*



---

## Requisitos

- **R** (recomendado: versão recente)
- Pacotes R (instalados pelo script de setup)
- Para rodar Stan via `cmdstanr`: **toolchain C++** + **CmdStan**

---

## Quickstart

Recomendação: rode sempre a partir do diretório raiz (root) do repositório.

### 1) Instalar dependências R

Conjunto mínimo (suficiente para os exemplos atuais):

```bash
Rscript scripts/_setup/install_deps.R
```

Conjunto mais amplo (diagnósticos/relatórios):

```bash
Rscript scripts/_setup/install_deps.R --all
```

### 2) (Opcional) Instalar CmdStan (para rodar Stan via cmdstanr)

```bash
Rscript scripts/_setup/install_cmdstan.R
```

---

## Rodar os exemplos

### Therapeutic Touch

```bash
Rscript scripts/therapeutic_touch/therapeutic_touch.R
```

Por padrão, quando você roda via `Rscript`, o script salva artefatos em:

- `outputs/figures/therapeutic_touch_plots.pdf`
- `outputs/tables/therapeutic_touch_report.txt`

### Baseball (3 níveis)

```bash
Rscript scripts/hierarchical/baseball_batting_by_position_3level_cmdstanr.R
```

Esse script é **console-only** (não grava arquivos) e aceita opções no formato `--chave=valor`, por exemplo:

```bash
Rscript scripts/hierarchical/baseball_batting_by_position_3level_cmdstanr.R \
  --chains=4 --iter_warmup=1000 --iter_sampling=1000 --adapt_delta=0.99
```



### Mortalidade (Poisson hierárquico com offset)

```bash
Rscript scripts/mortality/mortality_poisson_offset_cmdstanr_v2.R
```

Esse script lê um Excel e é **console-only** (não grava arquivos por padrão).  
Colunas esperadas no `sheet` configurado: `Contagem` (y), `POPULACAO` (offset) e `CODMUNRES` (município).

Exemplo especificando o caminho do Excel:

```bash
Rscript scripts/mortality/mortality_poisson_offset_cmdstanr_v2.R \
  --excel_path=data/raw/mortality/Dados_Mortalidade.xlsx --sheet_name=Resumo
```


### ISUS / SIA (Monte Carlo robusto)

```bash
Rscript scripts/isus_sia/mc_isus_sia.R
```

Opcionalmente, informe o caminho do CSV ou do ZIP:

```bash
Rscript scripts/isus_sia/mc_isus_sia.R --csv_path=caminho/para/ISUS_SIA_PARS.csv

Rscript scripts/isus_sia/mc_isus_sia.R --zip_path=caminho/para/ISUS_SIA_PARS.zip
```

Esse script é **console-only** (não grava arquivos) e assume separador `;` no CSV.


---

## Estrutura do repositório

- `scripts/` — scripts executáveis e seus READMEs
  - `scripts/_setup/` — instalação de dependências e CmdStan
- `data/` — dados pequenos e/ou públicos
  - `data/raw/` — dados brutos usados pelos exemplos
  - `data/interim/` e `data/processed/` — *placeholders* para dados derivados
- `outputs/` — saídas geradas (regeneráveis)
- `reports/` — *placeholder* para relatórios (Quarto/Rmd), quando aplicável
- `tests/` — *placeholder* para testes (opcional)

---

## Convenções

- Evito `setwd()`; os paths são pensados para rodar a partir do root.
- Scripts devem, sempre que possível:
  - fixar `seed` quando houver aleatoriedade
  - validar entrada (arquivos/colunas) e falhar com mensagem útil
  - escrever artefatos em `outputs/` quando fizer sentido (ex.: relatórios/plots)

---

## Autor

Ricardo  
LinkedIn: https://www.linkedin.com/in/ricardo-scotta/

---

## Licença

MIT — veja `LICENSE`.
