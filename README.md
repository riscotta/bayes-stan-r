# bayes-stan-r

Repositório de estudos, exemplos e experimentos em **Estatística Bayesiana com R e Stan**, com foco em scripts reproduzíveis, organização por tema e separação clara entre:

- dados de entrada
- scripts executáveis
- artefatos gerados
- relatórios opcionais
- testes opcionais

Este repositório **não é um pacote R**. A unidade principal aqui é o **script executável por exemplo/tema**.

## Objetivo do repositório

Organizar exemplos práticos e estudos aplicados em Bayes/Stan de forma que cada tema tenha:

- um script principal
- uma pasta própria
- documentação curta local
- entradas de dados bem definidas
- saídas previsíveis em `outputs/` quando aplicável

## Estrutura do projeto

```text
.
├── config/         # configurações opcionais do repositório
├── data/
│   ├── raw/        # dados brutos
│   ├── interim/    # dados intermediários (não versionar artefatos)
│   └── processed/  # dados processados (não versionar artefatos)
├── outputs/
│   ├── figures/    # gráficos gerados
│   ├── models/     # objetos/modelos salvos, quando aplicável
│   └── tables/     # tabelas e relatórios textuais
├── reports/        # relatórios opcionais (Quarto / R Markdown)
├── scripts/        # scripts executáveis do repositório
└── tests/          # testes opcionais
```

## Como usar

A regra geral é rodar tudo a partir da **raiz do repositório**, sem `setwd()`.

```bash
Rscript scripts/<pasta>/<script>.R
```

Exemplo:

```bash
Rscript scripts/therapeutic_touch/therapeutic_touch.R
```

## Setup inicial

### 1) Instalar dependências R

Mínimo:

```bash
Rscript scripts/_setup/install_deps.R
```

Mais completo (útil para testes, relatórios e dependências opcionais):

```bash
Rscript scripts/_setup/install_deps.R --all
```

### 2) Instalar CmdStan

Necessário para os exemplos que usam `cmdstanr`.

```bash
Rscript scripts/_setup/install_cmdstan.R
```

Exemplo com opções:

```bash
Rscript scripts/_setup/install_cmdstan.R --version=2.35.0 --cores=4
```

## Onde encontrar cada coisa

- `scripts/README.md`  
  Índice dos scripts executáveis e dos exemplos disponíveis.

- `config/README.md`  
  Convenções para centralizar parâmetros e caminhos do projeto, caso você queira evoluir essa camada.

- `reports/README.md`  
  Uso opcional de relatórios reproduzíveis.

- `tests/README.md`  
  Informações sobre os testes disponíveis e como executá-los.

## Exemplos atualmente organizados em `scripts/`

Entre os temas já estruturados no repositório:

- Therapeutic Touch
- Modelo hierárquico de baseball (3 níveis)
- Mortalidade com Poisson hierárquico e offset
- ISUS / SIA com Monte Carlo robusto
- SAheart com regressão logística Bayesiana
- Deck: número de partidas até alcançar vitórias
- Hidrômetro: degradação / erro de medição
- Média vs ruído
- Coortes de aposentadoria
- Kidney survival
- INHALER ordinal crossover
- RS Seguro

Para detalhes de entrada, saída e execução de cada exemplo, consulte:

`scripts/README.md`

## Convenções adotadas

- **1 pasta = 1 tema ou exemplo**
- cada tema deve ter um **script principal** claramente identificável
- quando necessário, cada pasta pode ter um `README.md` próprio
- artefatos regeneráveis devem ir para `outputs/`
- dados derivados devem ficar em `data/interim/` ou `data/processed/`
- scripts devem funcionar a partir da raiz do repositório

## Saídas e versionamento

Por padrão, este repositório evita versionar artefatos regeneráveis, como:

- figuras
- tabelas
- modelos salvos
- dados intermediários
- dados processados

A estrutura de pastas é preservada com `.gitkeep` quando necessário.

## Testes

Quando houver testes disponíveis, rode:

```bash
Rscript tests/run_tests.R
```

Os testes deste repositório são auxiliares e não definem a estrutura principal do projeto, que continua sendo orientada a scripts.

## Perfil do repositório

Este projeto foi organizado para priorizar:

- clareza operacional
- reprodutibilidade
- separação entre dados, scripts e saídas
- facilidade de expansão por novos exemplos

Em outras palavras: a porta de entrada do repositório é este `README`, e o catálogo operacional detalhado dos exemplos fica em `scripts/README.md`.

## Licença

Consulte o arquivo `LICENSE`.
