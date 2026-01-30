# bayes-stan-r

Scripts em R e modelos em Stan para análises bayesianas, simulação, modelagem e diagnósticos (PPC, checagens e visualizações).

## Uso principal

- Portfólio público  
- Suporte para artigos no LinkedIn (com dados públicos quando aplicável)  
- Base de referência para meu futuro livro sobre R e Stan  

## Status do repositório (importante)

Este repositório está em crescimento. **Hoje, os exemplos publicados estão concentrados em `scripts/therapeutic_touch/`.**
As pastas “por temas” (regression, glm, hierarchical etc.) aparecem no roadmap abaixo e serão preenchidas aos poucos.

---

## Quickstart (rodar em 3–5 minutos)

Recomendação: rode sempre a partir do diretório raiz (root) do repositório.

### Clonar o repositório

    git clone https://github.com/riscotta/bayes-stan-r.git
    cd bayes-stan-r

### Instalar dependências R

    Rscript scripts/_setup/install_deps.R

### Se quiser instalar um conjunto mais amplo (diagnósticos/relatórios):

    Rscript scripts/_setup/install_deps.R --all

### (Opcional) Instalar CmdStan (se for rodar Stan via cmdstanr)

    Rscript scripts/_setup/install_cmdstan.R

### Exemplo com versão e núcleos:

    Rscript scripts/_setup/install_cmdstan.R --version=2.35.0 --cores=4

### Rodar um script que exista no repo

Como este repo ainda está crescendo, a forma mais segura de listar scripts é:

    Rscript -e "cat(list.files('scripts', recursive=TRUE, pattern='\\\\.R$', full.names=TRUE), sep='\n')"

Depois rode um deles, por exemplo (ajuste o caminho para um que exista na sua listagem):

    Rscript scripts/therapeutic_touch/<um_script>.R

### Rodar testes (se existir runner)

Se você tiver o runner tests/run_tests.R:

    Rscript tests/run_tests.R

## Estrutura atual (o que existe hoje)

- scripts/therapeutic_touch/ — exemplos publicados atualmente
- R/ — helpers reutilizáveis
- data/ — dados pequenos e/ou públicos
- outputs/ — saídas geradas (em geral, regeneráveis)
- reports/ — relatórios (Quarto/Rmd)
- tests/testthat/ — testes (quando aplicável)
- config/ — configurações auxiliares

### Roadmap (organização por temas — a preencher)

A organização alvo (conforme o repositório crescer):

- scripts/bayes-basics/ — priors, posterior, checagens iniciais
- scripts/regression/ — regressão linear/logística (Bayes)
- scripts/glm/ — Poisson, NegBin, zero-inflated/hurdle (quando aplicável)
- scripts/hierarchical/ — multiníveis/hierárquicos (ex.: Eight Schools)
- scripts/stan/ — arquivos .stan e wrappers em R
- scripts/diagnostics/ — R-hat, ESS, divergences, treedepth, rank plots
- scripts/ppc/ — posterior predictive checks e calibração
- scripts/simulation/ — Monte Carlo, cenários e sensibilidade
- scripts/viz/ — utilitários de visualização

### Convenções
- Evito setwd(); os paths são pensados para rodar a partir do root.
- Scripts devem, sempre que possível:
  - fixar seed quando houver aleatoriedade
  - escrever outputs em outputs/ com nomes descritivos
  - declarar dependências e suposições no topo do arquivo (comentário curto)

## Autor

Ricardo  
LinkedIn: https://www.linkedin.com/in/ricardo-scotta/

## Licença

MIT — veja LICENSE.
