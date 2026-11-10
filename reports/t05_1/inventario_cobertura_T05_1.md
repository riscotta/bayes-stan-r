# T05.1 — inventário de cobertura real de testes e dependências

**Ciclo executor, 10/11/2026. Estado: implementação documental para QA independente; NÃO é homologação científica.**

> Tarefa literal: “Inventariar cobertura real por linguagem, script, estudo, tipo de teste e dependência externa.”

## 1. Proveniência, universo e metodologia

- Repositório: [riscotta/bayes-stan-r](https://github.com/riscotta/bayes-stan-r); referência exata [main `d8b593e36b8faf5367a09c55e6247cfbf971a8af`](https://github.com/riscotta/bayes-stan-r/commit/d8b593e36b8faf5367a09c55e6247cfbf971a8af); [PR #43](https://github.com/riscotta/bayes-stan-r/pull/43) integrado, CI de push [#42827479741](https://github.com/riscotta/bayes-stan-r/actions/runs/42827479741) (`completed/success`, job #129868463548).
- Universo: árvore Git **não truncada** no SHA-base; `scripts/` contém **79 fontes executáveis/de modelo: 47 R, 10 Python, 20 Stan e 2 PowerShell** (inclui `scripts/_setup/`); 33 pastas de estudos mais infraestrutura `_setup`. `tests/testthat/` contém 46 arquivos `test-*.R` na base, acrescidos do teste de controle T05.1 nesta branch. `tests/` contém ainda `run_tests.R` e `helpers/markdown_links.R`. `renv/activate.R` está fora do parsing da CI.
- [Inventário integral por arquivo de código](inventario_scripts.csv) (79 linhas, uma por fonte); [inventário por arquivo de teste](inventario_testes.csv) (47 linhas, incluindo o novo teste T05.1). Cada linha traz localizador de código no SHA de referência, linguagem, unidade, tipo, condição de execução e dependência **documentada ou ainda a confirmar**.
- **Critério de classificação:** teste relacionado ao estudo ≠ execução funcional do script do estudo; `parse()` ≠ compilação Stan; confronto de CSV congelado ≠ recomputação desde a fonte; texto/contrato ≠ PPC nem evidência de convergência; "não identificado" nesta árvore não é prova universal de inexistência externa.
- Nenhum `data/raw/`, modelo, script científico, saída histórica, prior, likelihood, amostra ou inferência foi alterado. Não foi lido conteúdo dos datasets institucionais para formular este inventário.

## 2. Matriz de cobertura efetiva por linguagem e mecanismo

| Superfície | O que realmente ocorre | Limite da alegação |
| --- | --- | --- |
| R — sintaxe | `.github/workflows/ci.yml` executa `parse()` sobre `scripts/` e `tests/`: **95 arquivos no run-base** (47 fontes R em scripts, 48 R em tests: runner, helper e 46 testes). | Somente sintaxe; não roda os 45 scripts R dos estudos/infra fora dos dois passos específicos executados. O novo teste aumenta o universo de parsing para 96, **sujeito a confirmação na CI do PR**. |
| R — testthat | `Rscript tests/run_tests.R` carrega por `sys.source()` os **46** arquivos `test-*.R` no run-base. Os arquivos cobrem estrutura, contratos de dados, textos, links, aritmética sintética e métricas publicadas. | **Não** implica invocar cada entrypoint do estudo nem compilar Stan. Após a inclusão de T05.1, são **47** arquivos, somente se o PR passar. |
| R — auditoria E10 SP | `scripts/mortalidade_sp_0_74/reproduzir_E10.R --modo=auditar` foi executado no job-base. | Verificação de indicadores públicos preservados; não reconstitui fontes primárias nem reestima modelos. |
| R — diagnóstico publicado | `scripts/_setup/report_scientific_gates.R` foi executado no job-base (IOM, UNODC, F5). | Reporta alertas já existentes; não reexecuta MCMC, SBC, LOO ou PPC em cadeias. |
| Python — PNAD N2 | Teste `test-pnadc-e10-matriz-t04-6.R` chama `auditar_e10.py` **somente se encontrar Python 3**. Log do run-base: **`Reason: Python 3 ausente: somente N1 foi executado`**. | N2 **SKIP** no SHA-base; check geral verde não demonstra auditoria N2. R1/T04.6: o script usa `assert`; `python3 -O` pode suprimir os gates. A correção pertence a tarefa própria, não à T05.1. |
| Python — E04, E05, E08, E09 | Os sete fontes históricos `estudo_I` não são executados pela CI principal; também não há `python -m py_compile` configurado. | CSV/manifestos e versões locais faltantes; nenhuma execução histórica E04–E09 ou teste de saída reproduzido. |
| Python — infraestrutura | `audit_data_privacy_surface.py` somente no workflow separado [data-surface-audit](../../.github/workflows/data-surface-audit.yml) acionado por alterações de caminhos ou despacho; `check_external_links.py` em [external-links](../../.github/workflows/external-links.yml) semanal/manual. | Inspeção de cabeçalhos/arquivos ZIP não homologa privacidade; auditoria de URL não garante acessibilidade. Nenhum destes jobs integra o check obrigatório do PR para uma alteração exclusivamente documental. |
| Stan — 20 fontes | Alguns testes leem nomes/texto de arquivos Stan e executam oráculos matemáticos em R. `ci.yml` não instala CmdStan nem usa `stanc`, `cmdstan_model()`, `sampling()` ou `rstan::stan()`. | **0/20 modelos comprovadamente compilados ou amostrados por essa CI**. Não há recuperação de parâmetros, PPC, SBC, ESS, R-hat ou divergências geradas novamente. |
| PowerShell — 2 fontes | Arquivos `.ps1` aparecem na árvore, mas não há `pwsh` no job Ubuntu/R. | Nenhum `.ps1` executado na CI; runner local Windows requer pré-requisitos próprios. |
| Ambiente | Runner `rocker/r-ver:4.5.1`; instala **`testthat` pela CRAN ao vivo**, e dependências de sistema. Desabilita autoloader renv. | Não roda `renv::restore()`, não fixa dependências transitivas do `testthat`, não instala Python 3, não materializa CmdStan 2.40.0. |

**Evidência do log:** [run-base #42827479741](https://github.com/riscotta/bayes-stan-r/actions/runs/42827479741), job [#129868463548](https://github.com/riscotta/bayes-stan-r/actions/runs/42827479741/job/129868463548): `Sintaxe válida em 95 arquivos R.`; `Rodando 46 arquivos de teste...`; `Reason: Python 3 ausente: somente N1 foi executado`; teste runner finaliza `Todos os arquivos de teste foram executados com sucesso.` Os quatro passos principais (parse, testthat, E10 SP e relatório de gates) terminaram com sucesso. O log separa **ausência de execução** de **falha do teste**.

## 3. Matriz por estudo e dependência externa

**Interpretação:** os testes associados abaixo verificam contrato, texto ou agregados onde aplicável, **não necessariamente o arquivo de script da mesma linha**. Dependências de dados identificadas exigem inspeção futura do README e do contrato específico, não autorização por inferência. "Sem teste específico" refere-se a `tests/testthat/` na base; existe parsing R transversal.

| Estudo | Fontes na árvore | Testes relacionados no testthat | Dependência externa/ponto de atenção |
| --- | --- | --- | --- |
| `caes_raca_f5` | 2 R / 0 Py / 4 Stan / 1 PS1 | `test-caes-f5-numerical.R`, `test-caes-raca-f5-structure.R` | agregados auditados F5 versionados; scripts históricos R/Stan e fontes cong.; sem reestimação |
| `censo_escolar_tempo_integral` | 1 R / 0 Py / 0 Stan / 0 PS1 | `test-censo-escolar-structure.R` | base do Censo Escolar e pacotes R/Stan; proveniência/vintage a confirmar |
| `consumer_shopping_trends_beta` | 1 R / 0 Py / 0 Stan / 0 PS1 | `test-consumer-shopping-trends-beta-structure.R` | CSV versionado e rstan; licença/vintage a confirmar |
| `coortes_aposentadoria` | 1 R / 0 Py / 0 Stan / 0 PS1 | **nenhum teste específico identificado** | Yahoo Finance e BCB/SGS via rede; versões de séries e respostas externas |
| `deck_15wins` | 1 R / 0 Py / 0 Stan / 0 PS1 | **nenhum teste específico identificado** | dados simulados no script; R/cmdstanr/CmdStan |
| `deter_mensal_bioma_uf` | 1 R / 0 Py / 0 Stan / 0 PS1 | `test-deter-mensal-structure.R` | série DETER por bioma-UF; origem/vintage e pacotes R/Stan |
| `exercises_dataset` | 1 R / 0 Py / 3 Stan / 0 PS1 | `test-exercises-dataset-structure.R` | base Exercises Dataset de origem/licença ainda pendente; modelos Stan |
| `fruitflies_aft` | 1 R / 0 Py / 0 Stan / 0 PS1 | `test-fruitflies-structure.R` | base FruitFlies e pacotes R/cmdstanr/CmdStan |
| `hidrometro_degradacao` | 1 R / 0 Py / 0 Stan / 0 PS1 | **nenhum teste específico identificado** | dados simulados no script; R/cmdstanr/CmdStan |
| `hierarchical` | 1 R / 0 Py / 0 Stan / 0 PS1 | **nenhum teste específico identificado** | BattingAverage.csv versionado; direitos de compilação pendentes; CmdStan |
| `importacoes_world_bank` | 1 R / 0 Py / 1 Stan / 0 PS1 | **nenhum teste específico identificado** | dados World Bank e enriquecimentos/vintage a confirmar; Stan |
| `inhaler_ordinal` | 1 R / 0 Py / 0 Stan / 0 PS1 | **nenhum teste específico identificado** | brms::inhaler ou CSV externo opcional; pacote brms; CmdStan |
| `ipca_sidra_1419` | 1 R / 0 Py / 0 Stan / 0 PS1 | **nenhum teste específico identificado** | SIDRA/IBGE e dependência de consulta ou série externa; vintage a confirmar |
| `isus_sia` | 1 R / 0 Py / 0 Stan / 0 PS1 | **nenhum teste específico identificado** | ZIP institucional ISUS/SIA; autorização e privacidade P0 pendentes |
| `kidney_survival` | 1 R / 0 Py / 0 Stan / 0 PS1 | **nenhum teste específico identificado** | dataset survival::kidney via pacote survival; CmdStan |
| `media_ruido_mu` | 1 R / 0 Py / 0 Stan / 0 PS1 | **nenhum teste específico identificado** | dados simulados no script; CmdStan |
| `mega_sena_dezenas` | 1 R / 0 Py / 2 Stan / 0 PS1 | `test-mega-sena-dezenas-structure.R` | séries da Mega-Sena; proveniência/vintage a confirmar; Stan |
| `missing_migrants` | 1 R / 0 Py / 1 Stan / 0 PS1 | `test-missing-migrants-structure.R`, `test-scientific-published-diagnostics.R` | IOM Missing Migrants; revisão de versão/licença/risco de dados P0; agregados auditados |
| `mortalidade_sp_0_74` | 5 R / 0 Py / 0 Stan / 0 PS1 | `test-mortalidade-sp-0-74.R`, `test-mortalidade-sp-links.R`, `test-study-pilot-t02-7.R` | insumos e indicadores versionados de mortalidade SP; auditoria de agregados, não base primária |
| `mortality` | 1 R / 0 Py / 0 Stan / 0 PS1 | **nenhum teste específico identificado** | Dados_Mortalidade.xlsx; produtor e redistribuição pendentes; CmdStan |
| `pms_servicos` | 1 R / 0 Py / 0 Stan / 0 PS1 | `test-pms-servicos-structure.R` | série PMS de serviços e pacotes R/rstan; vintage a confirmar |
| `pnadc_desocupacao` | 1 R / 0 Py / 0 Stan / 0 PS1 | `test-pnadc-desocupacao-structure.R` | PNAD Contínua e rstan; versão de dados/definições a confirmar |
| `pnadc_estudo_i` | 4 R / 8 Py / 3 Stan / 1 PS1 | `test-pnadc-estudo-i.R`, `test-pnadc-contratos-e01-e05-t04-2.R`, `test-pnadc-e00-consolidacao-t04-1.R`, `test-pnadc-e06-especificacao-t04-3.R`, `test-pnadc-e07-inventory-t03-1.R`, `test-pnadc-e10-matriz-t04-6.R`, `test-pnadc-exemplos-t04-7.R`, `test-pnadc-gaussian-derivation-t03-2.R`, `test-pnadc-inferential-alternatives-t03-5.R`, `test-pnadc-invariantes-sinteticos-t03-7.R`, `test-pnadc-labels-t03-6.R`, `test-pnadc-ponte-e06-e07-t04-4.R`, `test-pnadc-ponte-e08-e09-t04-5.R`, `test-pnadc-probability-trace-t03-3.R`, `test-pnadc-support-cv-rounding-t03-4.R` | snapshot SIDRA E04/manifestos locais, 52 cadeias E07 e saídas E08/E09 não publicados; Python histórico, R/CmdStan |
| `prisons_prisoners_unodc` | 1 R / 0 Py / 3 Stan / 0 PS1 | `test-scientific-published-diagnostics.R` | UNODC agregado e saídas MCMC históricas; licença/vintage P0; Stan |
| `retractions_reason_composition` | 1 R / 0 Py / 0 Stan / 0 PS1 | **nenhum teste específico identificado** | base de retratações e rstan; origem/vintage a confirmar |
| `retractions_time_to_retraction` | 1 R / 0 Py / 0 Stan / 0 PS1 | `test-retractions-time-to-retraction-structure.R` | base de retratações e cmdstanr; origem/vintage a confirmar |
| `retractions_trend` | 1 R / 0 Py / 0 Stan / 0 PS1 | `test-retractions-trend-structure.R` | base de retratações e rstan; origem/vintage a confirmar |
| `rs_seguro` | 2 R / 0 Py / 0 Stan / 0 PS1 | `test-rs-seguro-data-contract.R` | extratos institucionais RS Seguro e camadas opcionais; licença/privacidade P0 |
| `saheart` | 1 R / 0 Py / 0 Stan / 0 PS1 | **nenhum teste específico identificado** | SAheart.data local ou download automático pela rede; CmdStan |
| `sat_model_selection` | 1 R / 0 Py / 0 Stan / 0 PS1 | `test-sat-model-selection-structure.R` | dataset SAT e rstan; origem/vintage a confirmar |
| `sherlock_holmes_terms` | 1 R / 0 Py / 1 Stan / 0 PS1 | `test-sherlock-holmes-terms-structure.R` | corpus Sherlock Holmes; edição/compilação/direitos pendentes; Stan |
| `sobrevivencia_cadastral_poa` | 1 R / 0 Py / 2 Stan / 0 PS1 | **nenhum teste específico identificado** | dados de sobrevivência cadastral POA e CmdStan; fonte/perm. a validar |
| `therapeutic_touch` | 1 R / 0 Py / 0 Stan / 0 PS1 | **nenhum teste específico identificado** | TherapeuticTouchData.csv; licença/compilação a confirmar; R |

Dos **33** estudos, **18** têm ao menos um teste de estudo/resultado relacionado pela classificação nominal e conteúdo observado; **15** não têm teste específico identificável (além da sintaxe R se houver fonte R). Isso é **quantidade de pastas**, não porcentagem de linhas, branches ou modelos cobertos.

## 4. Tipos de teste — definições de aceitação

1. **Sintaxe:** `parse()` de arquivos R, sem invocação do corpo e sem interpretação estatística.
2. **Estrutural/documental:** existência de caminhos; colunas CSV; metadados/manifestos; links Markdown; tokens, matrizes e rótulos em relatórios. Detecta regressões de contrato, não erros numéricos gerais do script científico.
3. **Numérico sobre histórico congelado:** identidades e métricas em CSVs auditados (mortalidade SP, F5, IOM/UNODC, PNAD E09). Confirma consistência dos agregados versionados, não coleta original, ajuste nem validade externa.
4. **Funcional sintético limitado:** álgebra e propriedades sintéticas executadas em R (`test-pnadc-invariantes-sinteticos-t03-7.R`, pontes e alternativas). Não chamam Stan/Python históricos, não estimam parâmetros novos e não atestam inferências reais.
5. **Auditoria descritiva N2:** comando Python PNAD disponível para execução independente, mas opcional e **saltado** no job-base; seu escopo é 26 deltas, períodos e SHA-256, não E04–E09 integral.
6. **Stan/MCMC e reprodução científica:** nenhuma compilação/amostragem verificada no job-base; N3/N4 e teste preditivo externo permanecem **não demonstrados**, mesmo se CI estiver verde.

**Separação crítica:** `PASS de infraestrutura` = passos configurados da CI passaram; `PASS de agregados` = identidades de arquivos públicos conferidas; `PASS de reprodução científica` exige insumos, versões, geração de dados/modelo, amostragem, comparação, diagnósticos e parecer específico. Não converter a primeira afirmação na terceira.

## 5. Setup, versões, interfaces e riscos

- `config/environment.yml` registra R **4.5.1**, `cmdstanr` **0.9.0** de Stan r-universe, CmdStan **2.40.0** (instalação separada), `rstan` **2.32.7** via `renv.lock`; `scripts/_setup/dependencies.R` enumera bibliotecas R; `scripts/_setup/install_cmdstan.R` instala/verifica CmdStan apenas quando invocado. **Nada disso é instalado na CI rápida, exceto o pacote testthat e suas dependências do ambiente.**
- `scripts/pnadc_estudo_i/requirements-historicos.txt` registra pandas 2.2.3, numpy 2.3.5, scipy 1.17.0, matplotlib 3.10.8 e arviz 1.1.0; o job-base não usa `pip` nem verifica esse ambiente.
- Arquivos locais e externos relevantes: coleta/vintage SIDRA; **snapshot E04 de 22.130 linhas**, **19 CSV locais** e manifesto; **52 cadeias Stan E07**; diagnósticos E08.1, **104 cenários E08.2**, saídas E08.3/E08.4 e E09 probabilística integral **não disponibilizadas no clone**. O conjunto público de 26 linhas E09 é apenas descritivo. E08.3 tem dependência operacional indireta **E08.3 → E08.4 → E09**, sem certificação N3/N4.
- P0: T01.3–T01.7, [Issue #14](https://github.com/riscotta/bayes-stan-r/issues/14), decisões **D00-01–D00-06** continuam abertas; dados institucionais ISUS/SIA, RS Seguro e demais origens não são homologados por esse inventário. [Issue #15](https://github.com/riscotta/bayes-stan-r/issues/15) mantém pendentes smoke tests Stan, sampling e recuperação de parâmetros. [Issue #16](https://github.com/riscotta/bayes-stan-r/issues/16) mantém pendências de homepage e releases.
- Ressalvas herdadas: T04.4 **R1–R4**, T04.5 **R1–R3**, T04.6 **R1–R3** (especialmente `assert` sob otimização Python, dependência indireta E08.3 e skip de N2). T04.7 conserva ressalvas editoriais quanto à fonte primária IBGE e limites dos testes. Nada foi remediado ou reinterpretado aqui.

## 6. Lacunas por próximo ciclo e gates

| Tarefa posterior | Lacuna comprovada na T05.1 | Ação ainda **não** executada |
| --- | --- | --- |
| T05.2 | Auditoria Python N2 opcional e saltada no job-base; `assert` sob `-O`. | CI Python obrigatória, fixação de ambiente e teste adversarial de falha. |
| T05.3 | Nenhum dos 20 Stan compilado. | `stanc`/compilação de exemplares com versão fixada e logs. |
| T05.4 | Nenhum smoke de amostragem/recuperação. | Dados sintéticos pequenos e checagem de dimensão e parâmetros. |
| T05.5 | Diagnósticos históricos publicados, mas não gerados novamente. | PPC, priors, ESS, R-hat, divergências, E-BFMI etc. com tolerâncias justificadas. |
| T05.6 | Job rápido único não separa computação científica. | Jobs rápidos/lentos e dependências controladas. |
| T05.7 | PASS técnico e inferencial permanecem distintos. | Relato consolidado após execução e revisão por estudo. |

## 7. Evidência de mudança controlada e aceite T05.1

- **Escopo do PR:** somente este relatório, duas matrizes CSV e `tests/testthat/test-cobertura-inventario-t05-1.R` (teste declarativo de completude com `list.files()` e asserções de categorias). Nenhuma mudança na `ci.yml`, execução de MCMC, fontes do estudo, `data/`, `outputs/`, `renv.lock`, modelos Stan ou política de proteção.
- **Teste novo:** compara o universo real de fontes `scripts/` e o universo de testes `tests/testthat/` com as duas matrizes; exige categorias e registra que N2 foi `OPCIONAL_SKIP` na **CI-base**. Se novos scripts forem introduzidos futuramente, o inventário deve ser atualizado (é uma fotografia exata do SHA-base, não telemetria futura).
- **QA próprio obrigatório:** confirmar no HEAD final todos os caminhos, relações de teste, ausência de alegações de execução indevida, checks do PR e diff restrito. Emissão exclusiva de parecer **APROVADO / APROVADO COM RESSALVAS / REPROVADO / INCONCLUSIVO** em chat revisor distinto. **Exec. ☒ condicionado a artefatos e check; QA ☐; não mesclar.** Se o HEAD mudar, refazer verificação no novo SHA.
