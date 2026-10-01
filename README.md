# bayes-stan-r

[![R CI](https://github.com/riscotta/bayes-stan-r/actions/workflows/ci.yml/badge.svg)](https://github.com/riscotta/bayes-stan-r/actions/workflows/ci.yml)

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
│   └── <estudo>/   # saídas regeneráveis agrupadas por estudo
│       ├── figures/
│       ├── tables/
│       ├── models/
│       ├── logs/
│       ├── cmdstan_csv/
│       └── data_stan/
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

### Ambiente reproduzível com `renv`

O ambiente R do projeto é congelado com `renv`, usando **R 4.5.1** como versão de referência. A toolchain canônica inclui `cmdstanr 0.9.0`, `rstan 2.32.7` e **CmdStan 2.40.0**; os detalhes ficam em `config/environment.yml`.

Em um clone que já contenha o `renv.lock` oficial:

```bash
Rscript -e "if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv'); renv::restore(prompt = FALSE)"
```

Para inicializar ou reconstruir deliberadamente o lockfile do projeto:

```bash
Rscript scripts/_setup/init_renv.R
```

O conjunto de dependências é centralizado em `scripts/_setup/dependencies.R`.

### Instalação direta de dependências R

Fora do ambiente `renv`, continuam disponíveis os instaladores tradicionais:

```bash
Rscript scripts/_setup/install_deps.R
Rscript scripts/_setup/install_deps.R --all
```

### Instalação do CmdStan

O `renv` congela os pacotes R, inclusive `cmdstanr`, mas não o binário do CmdStan. Para instalá-lo:

```bash
Rscript scripts/_setup/install_cmdstan.R
```

O instalador usa **CmdStan 2.40.0** por padrão. Exemplo explícito equivalente:

```bash
Rscript scripts/_setup/install_cmdstan.R --version=2.40.0 --cores=4
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
- IPCA / SIDRA 1419 com decomposição por grupos e ajuste em Stan
- Censo Escolar 2021-2025 com modelagem de tempo integral na rede pública
- PNAD Contínua — desocupação com série mensal total e série trimestral por sexo via rstan
- PMS / Serviços em janeiro de 2026 com nível ancorado e tendência AR(1) via rstan
- FruitFlies com modelo AFT log-normal via Stan
- SAT com seleção Bayesiana de variáveis e comparação entre modelos
- estudos de simulação e comparação entre sinal e ruído
- retratações científicas globais: tempo até retratação e tendência temporal com modelos Bayesianos em Stan
- retratações científicas globais: composição dos motivos com modelo Dirichlet-multinomial hierárquico em Stan
- Consumer Shopping Trends: gasto online/loja com modelo Beta Bayesiano em rstan
- Mega-Sena: análise Bayesiana da distribuição das dezenas por fatores nominais e temporais com modelo multinomial log-linear em Stan
- Exercises Dataset: análise de cobertura do catálogo de exercícios e modelagem ordinal Bayesiana da dificuldade com Stan
- IOM Missing Migrants Project: análise Bayesiana de mortos e desaparecidos registrados por rota migratória e mês com modelo NB2 hierárquico em Stan
- sobrevivência cadastral de estabelecimentos de alimentação fora do lar em Porto Alegre, com modelo Bayesiano em tempo discreto e efeitos que variam no tempo
- UNODC Prisons and Prisoners: análise Bayesiana de `held_rate`, prisão sem sentença e contraste Female−Male com três modelos hierárquicos em Stan

Para o catálogo detalhado de execução, entradas e saídas, consulte:

`scripts/README.md`

## Convenções adotadas

Este projeto segue algumas convenções simples para facilitar manutenção e expansão:

- **1 pasta = 1 tema, estudo ou experimento**
- cada pasta deve ter um **script principal**
- saídas regeneráveis devem ser gravadas em `outputs/<estudo>/<tipo>/`, mantendo o estudo como primeiro nível
- dados derivados devem ficar em `data/interim/` ou `data/processed/`
- documentação local pode ser adicionada quando um tema exigir contexto extra
- scripts devem funcionar a partir da raiz do repositório

## Convenção de saídas

O padrão canônico do repositório é:

```text
outputs/<estudo>/<tipo>/
```

O nome do estudo vem sempre imediatamente após `outputs/`. Os tipos mais comuns são `figures`, `tables`, `models`, `logs`, `cmdstan_csv` e `data_stan`; cada estudo usa apenas os diretórios de que precisa. O padrão antigo `outputs/<tipo>/<estudo>/` não deve ser usado.

Consulte `outputs/README.md` para os detalhes.

## Integração contínua

O workflow `.github/workflows/ci.yml` roda em pushes e pull requests para `main` e pode ser disparado manualmente.

O CI mínimo:

- usa R 4.5.1;
- valida a sintaxe dos arquivos `.R` em `scripts/` e `tests/`;
- executa a suíte estrutural em `tests/testthat/`.

A pipeline não executa ajustes Stan completos em cada commit. Compilação, amostragem e validações estatísticas integrais continuam sendo etapas explícitas dos estudos quando necessárias.

## Reprodutibilidade e versionamento

O repositório prioriza reprodutibilidade e organização. O ambiente R é controlado por `renv`: o `renv.lock` registra versões de pacotes e repositórios, enquanto `.R-version` declara a versão de referência do R. O CmdStan é tratado separadamente, pois não é gerenciado pelo `renv`; sua versão canônica é registrada em `config/environment.yml` e aplicada por `scripts/_setup/install_cmdstan.R`.  

Por isso, artefatos regeneráveis normalmente não devem ser versionados, como:

- gráficos
- tabelas exportadas
- modelos salvos
- dados intermediários
- dados processados derivados

A estrutura de diretórios é preservada quando necessário com arquivos auxiliares como `.gitkeep`.

Ao mesmo tempo, alguns **dados brutos selecionados** permanecem versionados quando isso melhora a reprodução imediata do estudo, a utilidade didática ou a estabilidade de exemplos compactos. Exemplos atuais:

- `data/raw/pnadc_desocupacao/pnadc_mensal_taxa_desocupacao_6381.csv`
- `data/raw/pnadc_desocupacao/pnadc_trimestral_taxa_desocupacao_por_sexo_4093.csv`
- `data/raw/rs_seguro/rs_month_macrocrime.csv`
- `data/raw/rs_seguro/rs_month_macrocrime_profile_v1_1vict.csv`
- `data/raw/BattingAverage.csv`
- `data/raw/TherapeuticTouchData.csv`
- `data/raw/pms_servicos/pms_base_analitica_stan.csv`
- `data/raw/mega_sena_dezenas/base_analitica.csv`
- `data/raw/missing_migrants/Missing_Migrants_Global_Figures_allData.csv`
- `data/raw/sobrevivencia_cadastral_poa/model_inputs/stan_data_M0_agregado.json.gz`
- `data/raw/sobrevivencia_cadastral_poa/model_inputs/stan_data_M1_agregado.json.gz`
- `data/raw/prisons_prisoners_unodc/model_inputs/base_analitica_nucleo_nacional.csv`
- `data/raw/prisons_prisoners_unodc/model_inputs/stan_data_M1.json`

Bases externas grandes, arquivos com registros individualizados ou fontes sob download manual podem ficar fora do versionamento e ser referenciados na documentação local em `data/raw/` e `scripts/`. No estudo de sobrevivência cadastral, a base individual com CNPJ e endereço não é publicada; apenas insumos agregados e resultados auditáveis são versionados.

Em contrapartida, caches grandes, artefatos auxiliares e derivados continuam fora do versionamento. Para o inventário e as observações de origem/licença dos dados, consulte também `data/raw/README.md`.

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

## Estudo incluído: Importações anuais por país — World Bank + Stan

Este repositório agora inclui o estudo reproduzível de importações anuais por país, organizado no padrão do projeto:

- script final: `scripts/importacoes_world_bank/Script.R`
- modelo Stan: `scripts/importacoes_world_bank/modelo_principal_hierarquico_student_t.stan`
- dado bruto versionado: `data/raw/importacoes_world_bank/world_bank_Import_Usd_enriched.csv`
- saídas regeneráveis: `outputs/importacoes_world_bank/tables/`, `outputs/importacoes_world_bank/figures/` e `outputs/importacoes_world_bank/models/`

Execução a partir da raiz do repositório:

```bash
Rscript scripts/importacoes_world_bank/Script.R
```

O estudo usa `cmdstanr` e não salva objetos `.rds` por padrão; os diagnósticos de amostragem são exportados em CSV, logs e arquivos de configuração.

## Estudo incluído: Exercises Dataset — dificuldade e cobertura do catálogo

Este repositório agora inclui o estudo reproduzível do Exercises Dataset, organizado no padrão do projeto:

- script final: `scripts/exercises_dataset/exercises_dataset_cmdstanr.R`
- modelos Stan: `scripts/exercises_dataset/modelo_ordinal_dificuldade_sem_equipamento.stan`, `scripts/exercises_dataset/modelo_ordinal_dificuldade_com_equipamento_diagnostico.stan` e `scripts/exercises_dataset/modelo_softmax_dificuldade_sensibilidade.stan`
- dado bruto versionado: `data/raw/exercises_dataset/final_exercise_dataset.csv`
- saídas regeneráveis: `outputs/exercises_dataset/`

Execução a partir da raiz do repositório:

```bash
Rscript scripts/exercises_dataset/exercises_dataset_cmdstanr.R
```

Para reexecutar a amostragem Stan/cmdstanr:

```bash
Rscript scripts/exercises_dataset/exercises_dataset_cmdstanr.R --run_stan=1
```

A execução padrão não salva objetos `.rds`; os dados preparados, tabelas, JSONs para Stan, logs e diagnósticos são exportados em arquivos auditáveis.

## Estudo incluído: Sherlock Holmes — frequências de termos/personagens

Este repositório agora inclui o estudo reproduzível sobre frequências de termos/personagens no corpus Sherlock Holmes, organizado no padrão do projeto:

- script final: `scripts/sherlock_holmes_terms/sherlock_holmes_terms_cmdstanr.R`
- modelo Stan: `scripts/sherlock_holmes_terms/modelo_operacional_poisson_lognormal_e07.stan`
- dado bruto versionado: `data/raw/sherlock_holmes_terms/12_Sherlock_Holmes_Texts.csv`
- dados de modelagem e tabelas auditadas: `data/raw/sherlock_holmes_terms/`
- saídas regeneráveis: `outputs/sherlock_holmes_terms/`

Execução a partir da raiz do repositório:

```bash
Rscript scripts/sherlock_holmes_terms/sherlock_holmes_terms_cmdstanr.R
```

Para reexecutar a amostragem Stan/cmdstanr:

```bash
Rscript scripts/sherlock_holmes_terms/sherlock_holmes_terms_cmdstanr.R --run_stan=1
```

A execução padrão não reamostra o modelo; ela recompõe auditorias, tabelas finais e figuras a partir dos artefatos auditáveis versionados. O estudo usa um modelo Poisson-lognormal hierárquico com offset de exposição textual para comparar taxas por 10 mil tokens.

## Estudo incluído: IOM Missing Migrants Project — rota-mês + Stan

Este repositório inclui o estudo reproduzível sobre mortos e desaparecidos registrados pelo IOM Missing Migrants Project, organizado no padrão do projeto:

- script final: `scripts/missing_migrants/missing_migrants_cmdstanr.R`
- modelo Stan: `scripts/missing_migrants/modelo_nb_hierarquico_rota_mes_estavel.stan`
- dado bruto versionado: `data/raw/missing_migrants/Missing_Migrants_Global_Figures_allData.csv`
- artefatos auditáveis: `data/raw/missing_migrants/model_inputs/` e `data/raw/missing_migrants/resultados_auditados/`
- saídas regeneráveis: `outputs/missing_migrants/tables/`, `outputs/missing_migrants/figures/`, `outputs/missing_migrants/logs/`, `outputs/missing_migrants/data_stan/` e `outputs/missing_migrants/cmdstan_csv/`

Execução padrão a partir da raiz do repositório:

```bash
Rscript scripts/missing_migrants/missing_migrants_cmdstanr.R
```

A execução padrão recompõe bases mínimas, dados Stan, tabelas e figuras finais a partir dos artefatos auditáveis versionados. Para reamostrar o modelo com `cmdstanr`, use:

```bash
Rscript scripts/missing_migrants/missing_migrants_cmdstanr.R --run_stan=1
```

O estudo usa `cmdstanr` e não salva objetos `.rds` por padrão; os diagnósticos de amostragem são preservados em CSV, logs e arquivos de configuração.

## Estudo incluído: sobrevivência cadastral de estabelecimentos em Porto Alegre

Este repositório inclui o estudo reproduzível sobre a permanência cadastral de estabelecimentos de alimentação fora do lar em Porto Alegre:

- script final: `scripts/sobrevivencia_cadastral_poa/sobrevivencia_cadastral_poa_cmdstanr.R`
- modelos Stan: `scripts/sobrevivencia_cadastral_poa/modelo_M0_rw2_centrado_qr.stan` e `scripts/sobrevivencia_cadastral_poa/modelo_M1_rw2_centrado_qr.stan`
- insumos agregados para Stan: `data/raw/sobrevivencia_cadastral_poa/model_inputs/`
- resultados auditáveis: `data/raw/sobrevivencia_cadastral_poa/resultados_auditados/`
- saídas regeneráveis: `outputs/sobrevivencia_cadastral_poa/`

Execução padrão a partir da raiz do repositório:

```bash
Rscript scripts/sobrevivencia_cadastral_poa/sobrevivencia_cadastral_poa_cmdstanr.R
```

Para reexecutar M0 e M1 com `cmdstanr`:

```bash
Rscript scripts/sobrevivencia_cadastral_poa/sobrevivencia_cadastral_poa_cmdstanr.R --run_stan=1
```

A execução padrão recompõe tabelas e figuras a partir de artefatos auditáveis. A base individual com CNPJ e endereço permanece fora do versionamento; o repositório publica somente matrizes agregadas suficientes para reamostrar os modelos e reproduzir as conclusões centrais.

## Estudo incluído: UNODC Prisons and Prisoners — encarceramento e prisão sem sentença

Este repositório inclui o estudo reproduzível sobre encarceramento e prisão sem sentença com dados do UNODC *Prisons and Prisoners*:

- script final: `scripts/prisons_prisoners_unodc/prisons_prisoners_unodc_cmdstanr.R`
- modelos Stan finais: `scripts/prisons_prisoners_unodc/modelo_M1_held_rate_hurdle_REV5.stan`, `modelo_M2_unsentenced_total.stan` e `modelo_M3_unsentenced_sex.stan`
- base analítica nacional e entradas congeladas dos modelos: `data/raw/prisons_prisoners_unodc/model_inputs/`
- resultados compactos auditados: `data/raw/prisons_prisoners_unodc/resultados_auditados/`
- saídas regeneráveis: `outputs/prisons_prisoners_unodc/`

Execução padrão a partir da raiz:

```bash
Rscript scripts/prisons_prisoners_unodc/prisons_prisoners_unodc_cmdstanr.R
```

Para reexecutar os três ajustes finais com `cmdstanr`:

```bash
Rscript scripts/prisons_prisoners_unodc/prisons_prisoners_unodc_cmdstanr.R --run_stan=1
```

A execução padrão recompõe e valida a síntese final a partir de artefatos auditados. O modo `--run_stan=1` usa os dados Stan congelados e as especificações finais M1 REV5 / M2-M3 REV4.1. O estudo é descritivo/associativo: não sustenta causalidade nem generalização validada para países novos.
