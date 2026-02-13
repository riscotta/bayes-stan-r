# Config

Esta pasta é opcional.

Se você quiser centralizar *defaults* do repositório (por exemplo, caminhos e parâmetros padrão de MCMC), coloque aqui.

Sugestões de padrão (quando você decidir usar):

- `config.yml`: defaults “globais” (chains, iter_warmup, iter_sampling, etc.)
- `paths.R`: helpers para montar paths do repo sem `setwd()` (ex.: `data/raw`, `outputs/figures`)

Hoje a pasta fica como *placeholder* para manter a estrutura organizada.
