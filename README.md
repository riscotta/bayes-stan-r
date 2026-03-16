# bayes-stan-r

Repositório dedicado a **Estatística Bayesiana com R e Stan**, reunindo estudos, experimentos, modelos aplicados e exemplos reproduzíveis organizados por tema.

A proposta deste projeto é transformar conceitos bayesianos em **scripts executáveis, estruturas reutilizáveis e análises transparentes**, com foco em modelagem, simulação, inferência e documentação prática.

## Visão geral

Este repositório foi construído para servir como base de trabalho e estudo em:

- modelagem bayesiana com **Stan**
- análise de dados com **R**
- simulação e inferência probabilística
- organização reproduzível de experimentos
- documentação por exemplo e por tema

Em vez de concentrar tudo em notebooks isolados ou scripts dispersos, o projeto adota uma estrutura em que cada estudo pode evoluir de forma clara, auditável e expansível.

## Objetivo

O objetivo central é manter um ambiente em que modelos e análises possam ser:

- escritos com clareza
- executados a partir da raiz do projeto
- documentados por contexto
- expandidos para novos casos de uso
- reutilizados como base para estudos, artigos, relatórios e aplicações futuras

## Estrutura do repositório

```text
.
├── config/         # configurações opcionais do projeto
├── data/
│   ├── raw/        # dados brutos
│   ├── interim/    # dados intermediários
│   └── processed/  # dados processados
├── outputs/
│   ├── figures/    # gráficos gerados
│   ├── models/     # modelos e objetos salvos
│   └── tables/     # tabelas e saídas textuais
├── reports/        # relatórios opcionais (Quarto / R Markdown)
├── scripts/        # núcleo operacional: scripts por tema
└── tests/          # testes e validações opcionais
```

## Filosofia do projeto

Este repositório não foi estruturado como pacote R tradicional.  
A unidade principal aqui é o **script reproduzível por tema ou problema**.

A lógica é simples:

- cada tema fica organizado em sua própria pasta
- cada exemplo possui um script principal claramente executável
- dados, saídas e documentação ficam separados
- a raiz do repositório funciona como ponto de entrada
- os detalhes operacionais ficam documentados localmente nas pastas apropriadas

## Como executar

Os scripts devem ser executados a partir da **raiz do repositório**, sem uso de `setwd()`.

```bash
Rscript scripts/<pasta>/<script>.R
```

Exemplo:

```bash
Rscript scripts/therapeutic_touch/therapeutic_touch.R
```

## Setup inicial

### Instalação de dependências R

Instalação mínima:

```bash
Rscript scripts/_setup/install_deps.R
```

Instalação ampliada, incluindo dependências opcionais:

```bash
Rscript scripts/_setup/install_deps.R --all
```

### Instalação do CmdStan

Necessário para os exemplos que utilizam `cmdstanr`.

```bash
Rscript scripts/_setup/install_cmdstan.R
```

Exemplo com parâmetros:

```bash
Rscript scripts/_setup/install_cmdstan.R --version=2.35.0 --cores=4
```

## Organização da documentação

A documentação do projeto está distribuída por função:

- `README.md` da raiz  
  Apresenta a visão geral do repositório.

- `scripts/README.md`  
  Funciona como catálogo operacional dos scripts e exemplos disponíveis.

- `config/README.md`  
  Descreve convenções e possibilidades de configuração centralizada.

- `reports/README.md`  
  Explica o uso opcional de relatórios reproduzíveis.

- `tests/README.md`  
  Reúne orientações sobre testes e validações.

## Temas e exemplos

O repositório já reúne estudos e exemplos em áreas como:

- modelos hierárquicos
- regressão logística bayesiana
- modelos de contagem com Poisson
- simulações Monte Carlo
- sobrevivência
- modelos ordinais
- estudos aplicados em saúde, risco e inferência

Entre os exemplos organizados em `scripts/`, estão temas como:

- Therapeutic Touch
- Baseball hierárquico em 3 níveis
- Mortalidade com Poisson hierárquico e offset
- SAheart com regressão logística Bayesiana
- Kidney survival
- INHALER ordinal crossover
- RS Seguro
- DETER mensal por bioma-UF
- estudos de simulação e comparação entre sinal e ruído

Para o catálogo detalhado de execução, entradas e saídas, consulte:

`scripts/README.md`

## Convenções adotadas

Este projeto segue algumas convenções simples para facilitar manutenção e expansão:

- **1 pasta = 1 tema, estudo ou experimento**
- cada pasta deve ter um **script principal**
- saídas regeneráveis devem ser gravadas em `outputs/`
- dados derivados devem ficar em `data/interim/` ou `data/processed/`
- documentação local pode ser adicionada quando um tema exigir contexto extra
- scripts devem funcionar a partir da raiz do repositório

## Reprodutibilidade e versionamento

O repositório prioriza reprodutibilidade e organização.  
Por isso, artefatos regeneráveis normalmente não devem ser versionados, como:

- gráficos
- tabelas exportadas
- modelos salvos
- dados intermediários
- dados processados derivados

A estrutura de diretórios é preservada quando necessário com arquivos auxiliares como `.gitkeep`.

## Testes

Quando houver testes implementados, eles podem ser executados com:

```bash
Rscript tests/run_tests.R
```

Os testes têm papel de apoio à validação, mas a arquitetura principal do repositório continua centrada em **scripts reproduzíveis por tema**.

## Para quem este repositório foi pensado

Este projeto foi organizado para ser útil a quem deseja:

- estudar Bayes de forma aplicada
- transformar teoria em implementação
- manter exemplos executáveis e auditáveis
- construir uma base técnica sólida em **R + Stan**
- evoluir estudos em direção a relatórios, artigos, produtos ou aplicações práticas

## Licença

Consulte o arquivo `LICENSE`.
