# T02.1 — Classificação editorial das pastas de `scripts/`

**Base de inventário:** `main` em `051258640321dc75d8df66b30c4663a80b895e92`, 03/11/2026.  
**Escopo:** taxonomia editorial, sem movimentação de diretórios nem mudança de IDs, código, dados, modelos, resultados ou permissões.  
**Estado:** classificação proposta e implementada para **QA independente da T02.1**; não equivale a homologação científica, de privacidade, licenças ou reprodução.

## Critério de decisão

- **Laboratório de modelos:** objetivo principal é ilustrar, replicar ou comparar uma técnica, parametrização, diagnóstico ou simulação, ainda que use um dataset real de exemplo. A etiqueta não implica que o ajuste foi validado.
- **Estudo aplicado:** objeto ou pergunta substantiva explicitamente delimitados (p. ex. mortalidade territorial, mercado de trabalho, políticas públicas, texto, economia, segurança), com análise orientada ao fenômeno; a classificação não comprova suficiência dos resultados nem a existência de documentação E00–E10.
- A classificação é **editorial, provisória até QA**, baseada no catálogo e nas descrições das pastas. Dúvidas metodológicas e condições de publicação serão enfrentadas em T02.2–T02.7 e nas etapas científicas correspondentes.
- `scripts/_setup/` é infraestrutura e **não constitui unidade de estudo**. O N/A é justificado, sem forçá-lo em uma das duas classes.

## Inventário integral das 33 unidades de estudo

| Pasta (identificador preservado) | Classe | Fundamento observável no catálogo/README |
| --- | --- | --- |
| [`caes_raca_f5/`](caes_raca_f5/README.md) | Estudo aplicado | Raça declarada como preditor do escore F5 e avaliação preditiva |
| [`censo_escolar_tempo_integral/`](censo_escolar_tempo_integral/README.md) | Estudo aplicado | Tempo integral na rede pública e recortes do Censo Escolar |
| [`consumer_shopping_trends_beta/`](consumer_shopping_trends_beta/README.md) | Laboratório de modelos | Demonstração de ajuste Beta Bayesiano a variável de gasto |
| [`coortes_aposentadoria/`](coortes_aposentadoria/README.md) | Estudo aplicado | Sequência de retornos e patrimônio de coortes brasileiras |
| [`deck_15wins/`](deck_15wins/README.md) | Laboratório de modelos | Simulação sintética e comparação de Stan com posterior conjugada |
| [`deter_mensal_bioma_uf/`](deter_mensal_bioma_uf/README.md) | Estudo aplicado | Série mensal DETER por bioma e UF |
| [`exercises_dataset/`](exercises_dataset/README.md) | Estudo aplicado | Dificuldade e cobertura de exercícios em base identificada |
| [`fruitflies_aft/`](fruitflies_aft/README.md) | Laboratório de modelos | Replicação de exemplo AFT log-normal com dados de pacote |
| [`hidrometro_degradacao/`](hidrometro_degradacao/README.md) | Laboratório de modelos | Pipeline hierárquico validado em dados simulados |
| [`hierarchical/`](hierarchical/README.md) | Laboratório de modelos | Demonstração de partial pooling em baseball |
| [`importacoes_world_bank/`](importacoes_world_bank/README.md) | Estudo aplicado | Importações anuais por país e série do World Bank |
| [`inhaler_ordinal/`](inhaler_ordinal/README.md) | Laboratório de modelos | Demonstração de cutpoints em modelo ordinal crossover |
| [`ipca_sidra_1419/`](ipca_sidra_1419/README.md) | Estudo aplicado | IPCA obtido do SIDRA/IBGE, com análise temporal |
| [`isus_sia/`](isus_sia/README.md) | Estudo aplicado | Simulação de custos para precificação no contexto ISUS/SIA |
| [`kidney_survival/`](kidney_survival/README.md) | Laboratório de modelos | Exemplo de sobrevivência log-normal censurada |
| [`media_ruido_mu/`](media_ruido_mu/README.md) | Laboratório de modelos | Experimento sintético de média, ruído e posterior de μ |
| [`mega_sena_dezenas/`](mega_sena_dezenas/README.md) | Estudo aplicado | Estabilidade da distribuição das dezenas da Mega-Sena |
| [`missing_migrants/`](missing_migrants/README.md) | Estudo aplicado | Contagens registradas de mortes/desaparecimentos por rota-mês |
| [`mortalidade_sp_0_74/`](mortalidade_sp_0_74/README.md) | Estudo aplicado | Mortalidade de 0–74 anos em São Paulo, 2006–2023 |
| [`mortality/`](mortality/README.md) | Laboratório de modelos | Exemplo de Poisson hierárquico com offset e shrinkage |
| [`pms_servicos/`](pms_servicos/README.md) | Estudo aplicado | Indicadores da PMS e serviços em período definido |
| [`pnadc_desocupacao/`](pnadc_desocupacao/README.md) | Estudo aplicado | Desocupação na PNAD Contínua com cortes por sexo/período |
| [`pnadc_estudo_i/`](pnadc_estudo_i/README.md) | Estudo aplicado | Estudo multidimensional de indicadores do trabalho |
| [`prisons_prisoners_unodc/`](prisons_prisoners_unodc/README.md) | Estudo aplicado | Taxas e perfil de custódia prisional em registros UNODC |
| [`retractions_reason_composition/`](retractions_reason_composition/README.md) | Estudo aplicado | Composição de motivos de retratações científicas |
| [`retractions_time_to_retraction/`](retractions_time_to_retraction/README.md) | Estudo aplicado | Tempo transcorrido até retratação científica |
| [`retractions_trend/`](retractions_trend/README.md) | Estudo aplicado | Tendência temporal de retratações científicas |
| [`rs_seguro/`](rs_seguro/README.md) | Estudo aplicado | Séries de segurança pública e perfil da vítima |
| [`saheart/`](saheart/README.md) | Laboratório de modelos | Exemplo de regressão logística Bayesiana em SAheart |
| [`sat_model_selection/`](sat_model_selection/README.md) | Laboratório de modelos | Ilustração de seleção de modelos, Bayes factors e LOO |
| [`sherlock_holmes_terms/`](sherlock_holmes_terms/README.md) | Estudo aplicado | Frequências de termos/personagens em corpus delimitado |
| [`sobrevivencia_cadastral_poa/`](sobrevivencia_cadastral_poa/README.md) | Estudo aplicado | Sobrevivência cadastral de estabelecimentos em Porto Alegre |
| [`therapeutic_touch/`](therapeutic_touch/README.md) | Laboratório de modelos | Exemplo comparativo pooled/unpooled/hierárquico e diagnóstico |

**Controle de cobertura:** 33/33 pastas de estudos do catálogo `scripts/README.md` classificadas: **12 Laboratórios de modelos** e **21 Estudos aplicados**; 1 pasta de infraestrutura (`_setup/`) **N/A**. Identificadores, caminhos e nomes originais foram mantidos.

## Limites e interfaces com tarefas seguintes

1. **T02.2–T02.3:** metadados de publicação e modelo E00 ainda não existem para todos os itens; a classe não atribui estado aprovado, completo ou reproduzido.
2. **T02.4–T02.6:** pontes matemática–código e evidência–conclusão e estado formal por etapa ainda precisam ser estabelecidos; não inferir E00–E10 completos.
3. **T02.7:** formalização no CONTRIBUTING e validação em piloto continuam pendentes. Esta taxonomia não modifica a política de contribuição.
4. **T01.3 / Issue #14:** classificação editorial não modifica `review_required`, `not_identified`, não autoriza acesso/publicação de dados e não libera os gates G1–G5.
5. **QA independente:** comparar a lista com a árvore `scripts/` e o catálogo, testar links/contagens e julgar especialmente a fronteira entre exemplos com dados reais e estudos aplicados. Ajustar com justificativa antes de mesclar. CI verde comprova apenas os checks nela programados.

**Fontes de decisão:** [catálogo de scripts](README.md), [README principal](../README.md), READMEs de cada pasta (links acima) e [Plano de Correção v1.1](https://github.com/riscotta/bayes-stan-r) como controle externo. Não foram lidos conteúdos dos datasets prioritários de privacidade.
