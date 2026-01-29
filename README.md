# bayes-stan-r

Scripts em R e modelos em Stan para análises bayesianas, simulação, modelagem e diagnósticos (PPC, checagens e visualizações).

## Uso principal

- Portfólio público  
- Suporte para artigos no LinkedIn (com dados públicos quando aplicável)  
- Base de referência para meu futuro livro sobre R e Stan  

## Quickstart (rodar em 3–5 minutos)

Recomendação: rode sempre a partir do diretório raiz (root) do repositório.

### Clonar o repositório

    git clone https://github.com/riscotta/bayes-stan-r.git
    cd bayes-stan-r

### Instalar dependências R

    Rscript scripts/_setup/install_deps.R

### (Opcional) Instalar CmdStan (se for rodar Stan via cmdstanr)

    Rscript scripts/_setup/install_cmdstan.R

### Rodar um exemplo (ajuste para um script que exista no repo)

    Rscript scripts/regression/linear_bayes.R

### Rodar testes (sem pacote)

    Rscript tests/run_tests.R

**Observações:**

- Se você não pretende rodar modelos Stan agora, pode pular a instalação do CmdStan.
- Evito setwd(); os paths são pensados para rodar a partir do root do repositório.

## Organização (por temas)

Os scripts são organizados por tema (não há um pipeline único).

- scripts/bayes-basics/ — priors, posterior, checagens iniciais  
- scripts/regression/ — regressão linear/logística (Bayes)  
- scripts/glm/ — Poisson, NegBin, zero-inflated/hurdle (quando aplicável)  
- scripts/hierarchical/ — modelos multiníveis/hierárquicos (ex.: Eight Schools)  
- scripts/stan/ — arquivos .stan e wrappers em R  
- scripts/diagnostics/ — R-hat, ESS, divergences, treedepth, rank plots  
- scripts/ppc/ — posterior predictive checks e calibração  
- scripts/simulation/ — Monte Carlo, cenários e sensibilidade  
- scripts/viz/ — utilitários de visualização  

## Funções reutilizáveis

- R/ — helpers de IO, wrangling, visualizações e suporte ao Stan

## Dados e relatórios

- data/ — apenas dados pequenos e/ou públicos  
- reports/ — relatórios (Quarto .qmd / R Markdown)  
- outputs/ — saídas geradas (figuras/tabelas/modelos). Em geral, são regeneráveis.  

### Sugestão de política para outputs/ (para manter o repo limpo e consistente)

- commit apenas saídas “curadas” (figuras finais para post/relatório)
- evitar artefatos pesados e intermediários quando não forem necessários

## Testes (testthat) sem virar pacote

Este repositório usa testthat para validar helpers e comportamentos críticos, mas não é um pacote R.

- runner: tests/run_tests.R  
- testes: tests/testthat/  
- loader de helpers (via source dos arquivos em R/): tests/testthat/helper-load.R  

### Para executar

    Rscript tests/run_tests.R

## Como rodar no navegador (sem instalar nada no computador)

### GitHub Codespaces

1. Abra o repositório no GitHub
2. Vá em Code → Codespaces → Create codespace on main
3. Abra o terminal do Codespaces e execute o(s) script(s) desejado(s) a partir do root do repositório, por exemplo:

    Rscript scripts/regression/linear_bayes.R

## Dados (públicos e pequenos)

Este repositório usa apenas dados públicos e/ou exemplos pequenos.

Quando houver uma licença específica do dataset (CC BY, ODbL etc.), a fonte e os termos serão citados no diretório do exemplo.

## Convenções

- Scripts: scripts/<tema>/<nome_descritivo>.R
- Modelos Stan: em scripts/stan/ (ou no tema correspondente)
- Funções reutilizáveis: R/
- Evito setwd(); os paths são pensados para rodar a partir do root do repositório.

Sempre que possível, scripts devem:

- fixar seed quando houver aleatoriedade
- escrever outputs em outputs/ com nomes descritivos
- declarar dependências e suposições no topo do arquivo (comentário curto)

## Artigos (LinkedIn) e livro

Links dos artigos serão adicionados conforme publicação.

Este repo é a base de código + dados para os posts e para o livro.

## Autor

Ricardo  
LinkedIn: https://www.linkedin.com/in/ricardo-scotta/

## Licença

MIT — veja LICENSE.
