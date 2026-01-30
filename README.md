# bayes-stan-r

Scripts em **R** e modelos em **Stan** para análises bayesianas, simulação, modelagem e diagnósticos (PPC, checagens e visualizações).

## Uso principal

- Portfólio público
- Suporte para artigos no LinkedIn (com dados públicos quando aplicável)
- Base de referência para meu futuro livro sobre R e Stan

## Status do repositório

Este repositório está em crescimento. Hoje existem **2 exemplos principais**:

- `scripts/therapeutic_touch/therapeutic_touch.R`
- `scripts/hierarchical/baseball_batting_by_position_3level_cmdstanr.R`

Conforme o repo crescer, vou adicionando novos exemplos por tema (veja o *Roadmap*).

---

## Requisitos

- **R** (recomendado: versão recente)
- Pacotes R (instalados via script de setup abaixo)
- Para rodar modelos Stan via `cmdstanr`: **toolchain C++** + **CmdStan**

---

## Quickstart

Recomendação: rode sempre a partir do diretório raiz (root) do repositório.

### 1) Clonar

```bash
git clone https://github.com/riscotta/bayes-stan-r.git
cd bayes-stan-r
```

### 2) Instalar dependências R

Instala o conjunto mínimo (suficiente para os exemplos atuais):

```bash
Rscript scripts/_setup/install_deps.R
```

Se quiser instalar um conjunto mais amplo (diagnósticos/relatórios):

```bash
Rscript scripts/_setup/install_deps.R --all
```

### 3) (Opcional) Instalar CmdStan (para rodar Stan via cmdstanr)

```bash
Rscript scripts/_setup/install_cmdstan.R
```

Exemplo com versão e núcleos:

```bash
Rscript scripts/_setup/install_cmdstan.R --version=2.35.0 --cores=4
```

---

## Rodar exemplos (que existem hoje no repo)

### Therapeutic Touch (hierárquico vs pooled + PPC/LOO)

```bash
Rscript scripts/therapeutic_touch/therapeutic_touch.R
```

### Baseball (3 níveis: geral → posição → jogador)

```bash
Rscript scripts/hierarchical/baseball_batting_by_position_3level_cmdstanr.R
```

Você também pode passar opções (formato `--chave=valor`), por exemplo:

```bash
Rscript scripts/hierarchical/baseball_batting_by_position_3level_cmdstanr.R \
  --chains=4 --iter_warmup=1000 --iter_sampling=1000 --adapt_delta=0.99
```

Dica: para ver tudo que existe em `scripts/`:

```bash
Rscript -e "cat(list.files('scripts', recursive=TRUE, pattern='\\\\.R$', full.names=TRUE), sep='\\n')"
```

---

## Testes (quando aplicável)

Se existir o runner `tests/run_tests.R`:

```bash
Rscript tests/run_tests.R
```

---

## Estrutura atual

- `scripts/therapeutic_touch/` — exemplo published
- `scripts/hierarchical/` — exemplo published
- `scripts/_setup/` — instalação de dependências e CmdStan
- `R/` — helpers reutilizáveis (à medida que o repo amadurece)
- `data/` — dados pequenos e/ou públicos
- `outputs/` — saídas geradas (em geral, regeneráveis)
- `reports/` — relatórios (Quarto/Rmd)
- `tests/testthat/` — testes (quando aplicável)
- `config/` — configurações auxiliares

---

## Roadmap (organização por temas — a preencher)

- `scripts/bayes-basics/` — priors, posterior, checagens iniciais
- `scripts/regression/` — regressão linear/logística (Bayes)
- `scripts/glm/` — Poisson, NegBin, zero-inflated/hurdle (quando aplicável)
- `scripts/hierarchical/` — multiníveis/hierárquicos (ex.: Eight Schools)
- `scripts/stan/` — arquivos `.stan` e wrappers em R
- `scripts/diagnostics/` — R-hat, ESS, divergences, treedepth, rank plots
- `scripts/ppc/` — posterior predictive checks e calibração
- `scripts/simulation/` — Monte Carlo, cenários e sensibilidade
- `scripts/viz/` — utilitários de visualização

---

## Convenções (para o repo crescer sem virar bagunça)

- Evito `setwd()`; os paths são pensados para rodar a partir do root.
- Scripts devem, sempre que possível:
  - fixar `seed` quando houver aleatoriedade
  - escrever outputs em `outputs/` com nomes descritivos
  - declarar dependências e suposições no topo do arquivo (comentário curto)

---

## Autor

Ricardo  
LinkedIn: https://www.linkedin.com/in/ricardo-scotta/

## Licença

MIT — veja `LICENSE`.
