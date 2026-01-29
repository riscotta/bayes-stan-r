bayes-stan-r

Scripts em R e modelos em Stan para análises bayesianas, simulação, modelagem e diagnósticos (PPC, checagens e visualizações).

Uso principal

Portfólio público

Suporte para artigos no LinkedIn (com dados públicos quando aplicável)

Base de referência para meu futuro livro sobre R e Stan

Organização (por temas)

Os scripts são organizados por tema (não há um pipeline único).

scripts/bayes-basics/ — priors, posterior, checagens iniciais

scripts/regression/ — regressão linear/logística (Bayes)

scripts/glm/ — Poisson, NegBin, zero-inflated/hurdle (quando aplicável)

scripts/hierarchical/ — modelos multiníveis/hierárquicos (ex.: Eight Schools)

scripts/stan/ — arquivos .stan e wrappers em R

scripts/diagnostics/ — R-hat, ESS, divergences, treedepth, rank plots

scripts/ppc/ — posterior predictive checks e calibração

scripts/simulation/ — Monte Carlo, cenários e sensibilidade

scripts/viz/ — utilitários de visualização

Funções reutilizáveis

R/ — helpers de IO, wrangling, visualizações e suporte ao Stan

Dados e relatórios

data/ — apenas dados pequenos e/ou públicos

reports/ — relatórios (Quarto .qmd / R Markdown)

outputs/ — saídas geradas (figuras/tabelas/modelos). Em geral, são regeneráveis.

Como rodar no navegador (sem instalar nada no computador)
GitHub Codespaces

Abra o repositório no GitHub

Vá em Code → Codespaces → Create codespace on main

Rode um script específico, por exemplo:

Rscript scripts/regression/linear_bayes.R

Dados (públicos e pequenos)

Este repositório usa apenas dados públicos e/ou exemplos pequenos.

Quando houver uma licença específica do dataset (CC BY, ODbL etc.), a fonte e os termos serão citados no diretório do exemplo.

Convenções

Scripts: scripts/<tema>/<nome_descritivo>.R

Modelos Stan: em scripts/stan/ (ou no tema correspondente)

Funções reutilizáveis: R/

Evito setwd(); os paths são pensados para rodar a partir do root do repositório.

Artigos (LinkedIn) e livro

Links dos artigos serão adicionados conforme publicação.

Este repo é a base de código + dados para os posts e para o livro.

Autor

Ricardo
LinkedIn: https://www.linkedin.com/in/ricardo-scotta/

Licença

MIT — veja LICENSE.
