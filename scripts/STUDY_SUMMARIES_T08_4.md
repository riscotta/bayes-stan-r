# T08.4 — Sumários científicos e roteiros E00–E10

Base da leitura: `9e91ab6b5f756fda25258d3cf181f51cd487a8e2`, 26/11/2026. Consulte o [catálogo T08.2](STUDY_CATALOG_T08_2.md), a [taxonomia](STUDY_TAXONOMY_T02_1.md) e as [decisões de proveniência da issue #14](../data/DATA_PROVENANCE_DECISIONS.md).

## Cobertura

Das 33 unidades, 12 são laboratórios e 21 são estudos aplicados. Oito aplicados têm fechamento histórico ou síntese substantiva rastreável a resultados finais versionados e entram neste roteiro. Esse critério editorial admite lacunas no histórico e suspensões operacionais. A navegação indica material de leitura e preserva os [estados formais por etapa](STUDY_STAGE_STATUS_T02_6.md). Os [cinco eixos T06.2](../reports/t06_2/README.md) e os [três modos T06.3](../reports/t06_3/README.md) conservam seus próprios escopos.

| Fora do recorte | Unidades | Evidência ou lacuna |
| --- | --- | --- |
| 12 laboratórios | `consumer_shopping_trends_beta`, `deck_15wins`, `fruitflies_aft`, `hidrometro_degradacao`, `hierarchical`, `inhaler_ordinal`, `kidney_survival`, `media_ruido_mu`, `mortality`, `saheart`, `sat_model_selection`, `therapeutic_touch` | Objetivo didático ou metodológico na taxonomia. |
| 2 aplicados com base e implementação | [Exercises Dataset](exercises_dataset/README.md), [Importações World Bank](importacoes_world_bank/README.md) | Entradas e métodos documentados. Síntese final rastreável a resultados históricos não localizada nos READMEs e artefatos de saída versionados. |
| 8 aplicados com rotas de execução | [Censo Escolar](censo_escolar_tempo_integral/README.md), [Coortes de aposentadoria](coortes_aposentadoria/README.md), [DETER](deter_mensal_bioma_uf/README.md), [IPCA](ipca_sidra_1419/README.md), [ISUS/SIA](isus_sia/README.md), [PMS](pms_servicos/README.md), [PNAD desocupação](pnadc_desocupacao/README.md), [RS Seguro](rs_seguro/README.md) | Os READMEs descrevem métodos e saídas. A síntese final com evidência versionada não foi localizada na triagem. Em RS Seguro, M2 trata de contagens mensais de ocorrências com uma vítima por sexo, faixa e crime. |
| 3 aplicados sobre retratações | [Motivos](retractions_reason_composition/README.md), [Tempo até retratação](retractions_time_to_retraction/README.md), [Tendência](retractions_trend/README.md) | Base externa e rotas de execução documentadas, com síntese final versionada não localizada na triagem. |

## Cães e escore F5

A raça pura declarada pelo tutor melhora a previsão interna do F5 além de idade, sexo, porte e esterilização? Em 2.084 cães, cinco folds compararam modelos com e sem interceptos hierárquicos por raça. O ganho relativo de RMSE foi pequeno, de 0,85% a 1,41% conforme a família, com piora no primeiro fold. Os folds foram definidos após exploração da coorte. A calibração condicional e a generalização externa seguem sem validação. O F5 mede um escore de questionário e o desenho não identifica causalidade genética. [Relatório E10](../reports/caes_raca_f5/relatorio_cientifico.md)

- **E00–E05** — Leia a pergunta, o recorte e a fonte no [README](caes_raca_f5/README.md). A base analítica histórica E04 está fora do repositório. A documentação inicial por etapa está incompleta na árvore.
- **E06–E10** — O README localiza a especificação e o código histórico E07/E08. No relatório, leia as métricas por família e fold, as checagens de calibração e o fechamento. O [piloto T06.6](../reports/t06_6/README.md) cobre quatro agregados N2. A [rota do adaptador segue suspensa](../reports/t07_7/README.md).

## Mortalidade de 0–74 anos em São Paulo

Como evoluiu a mortalidade registrada em 96 distritos entre 2006 e 2023? A análise por idade e sexo combina padronização direta, decomposição descritiva e avaliação temporal de Poisson/NB2 em R. De 2019 a 2023, a taxa bruta subiu 2,8% e a padronizada caiu 5,0%. A correspondência territorial é provisória e o desenho não identifica efeitos causais. [Síntese E10](../reports/mortalidade_sp_0_74/relatorio_cientifico.md)

- **E00–E05** — O [piloto T02.7](STUDY_PILOT_T02_7_MORTALIDADE_SP.md) localiza a pergunta e distingue reconstrução editorial de evidência histórica. O [caderno](../reports/mortalidade_sp_0_74/caderno_E01_E10.md) registra os insumos, o cruzamento territorial e a base provisória. Fontes primárias e correspondência nominal não acompanham o repositório.
- **E06–E10** — Siga do caderno para a [matriz de evidências](../reports/mortalidade_sp_0_74/matriz_evidencias.md) e a [E10](../reports/mortalidade_sp_0_74/E10_comunicacao_e_preservacao.md). A síntese pública N2 usa agregados E09. A [rota do adaptador segue suspensa](../reports/t07_7/README.md).

## PNAD Contínua — Estudo I

A queda da desocupação veio acompanhada de melhora nas demais dimensões do mercado de trabalho? O estudo compara 13 agregados para o Brasil em duas janelas terminadas em setembro de 2026. Na janela mais recente, o desalento e a subocupação por insuficiência de horas aumentaram nos agregados publicados. Os modelos Stan históricos estimam níveis marginais. As probabilidades de direção dos contrastes são cenários gaussianos condicionais a uma correlação temporal postulada. Falta a covariância de desenho para identificar uma posterior temporal conjunta. [Nota científica](../reports/pnadc_estudo_i/nota_cientifica.md)

- **E00–E05** — Comece pela [consolidação editorial E00](../reports/pnadc_estudo_i/e00_consolidado_T04_1.md) e pelos [contratos E01–E05](../reports/pnadc_estudo_i/contratos_e01_e05_T04_2.md). Os insumos de execução da cadeia histórica estão fora do GitHub.
- **E06–E10** — Leia a [especificação E06](../reports/pnadc_estudo_i/e06_especificacao_modelos_T04_3.md), a [ponte E06–E07](../reports/pnadc_estudo_i/ponte_e06_e07_T04_4.md), a [ponte E08–E09](../reports/pnadc_estudo_i/ponte_e08_e09_T04_5.md) e a [matriz E10](../reports/pnadc_estudo_i/e10_matriz_evidencias_reproducao_T04_6.md). O N2 cobre a projeção de 26 contrastes. A T07.6 cobre equivalência operacional contemporânea e contrato do adaptador. O cotejo com os logs originais ficou pendente.

## Mega-Sena

A distribuição das dezenas mudou entre períodos, décadas ou dias da semana? Modelos log-lineares multinomiais comparam dezenas por fator. A síntese E09 indica estabilidade prática na escala agregada avaliada. A posição publicada das dezenas é uma estatística de ordem. A comparação por ano é exploratória e teve uma divergência MCMC. O resultado não sustenta rankings preditivos. [README e conclusão](mega_sena_dezenas/README.md)

- **E00–E05** — Use o README e a [descrição da base analítica](../data/raw/mega_sena_dezenas/README.md). A pergunta é recuperável da descrição. O roteiro não localizou um planejamento E00 original nem uma cadeia primária integral.
- **E06–E10** — O README localiza os modelos e o ajuste. Consulte as checagens em `diagnosticos_auditados/` e as mensagens E09 em `tabelas_auditadas/E09_mensagens_chave.csv`. A conclusão e a rotina de recomposição delimitam a leitura final. Um relatório E10 separado não foi localizado.

## IOM Missing Migrants

Como a carga registrada de mortos e desaparecidos varia por rota e mês? O modelo histórico usa NB2 hierárquica, incidentes simples e exposição em dias. A posterior indica concentração por rota e tendência média anual positiva da carga registrada. Há parâmetros com R-hat acima do alvo estrito e falhas preditivas em zeros e dispersão. A taxa modelada não mede risco por migrante, e a cobertura dos registros limita a interpretação. [README e conclusão](missing_migrants/README.md)

- **E00–E05** — O README identifica fonte, recorte e preparação mínima. Planejamento E00 e a linhagem completa da extração histórica não foram localizados.
- **E06–E10** — Siga os insumos rota-mês e a especificação Stan indicados no README. Leia os diagnósticos E08 antes das sínteses de rota, sazonalidade e tendência E09. A E10 preserva as sínteses `E10_resultados_chave.csv` e `E10_reprodutibilidade.csv`, em `resultados_auditados/`. A recomposição padrão usa evidência congelada.

## Sherlock Holmes

Como se distribuem as menções dos termos selecionados entre as obras do corpus? O modelo hierárquico Poisson-lognormal usa células obra–termo e exposição em tokens. Na síntese histórica, Sherlock Holmes e Dr. Watson tiveram as maiores taxas globais entre os termos analisados. Para termos raros, há forte esparsidade. A sensibilidade a priors alternativos e à NB2 permanece pendente. A leitura fica restrita ao corpus e ao vocabulário selecionados. [README](sherlock_holmes_terms/README.md)

- **E00–E05** — O README identifica o corpus e os mapas de obra e termo. Planejamento original e edição ou montagem exata do corpus não foram localizados.
- **E06–E10** — O README localiza o modelo operacional E07, a entrada Stan e as tabelas finais auditáveis. Leia diagnósticos e checagens preditivas antes das taxas e contrastes. A síntese final em `tabelas_finais_auditadas/tabela_final_mensagens_chave.csv` usa taxas posteriores por 10 mil tokens. Um relatório E10 numerado não foi localizado.

## Sobrevivência cadastral em Porto Alegre

Qual a permanência cadastral de estabelecimentos de alimentação fora do lar nos primeiros cinco anos? A análise de sobrevivência em tempo discreto usa dados agregados e compara efeitos constantes e variantes no tempo. Na amostra analítica, a sobrevivência observada aos 60 meses foi de 57,9%, e o modelo com efeitos variantes no tempo teve melhor desempenho na comparação interna. Baixa cadastral é um desfecho administrativo. O desenho deixa em aberto a previsão de coortes futuras e a dependência entre unidades do mesmo CNPJ básico. [README e resultados](sobrevivencia_cadastral_poa/README.md)

- **E00–E05** — Comece pela pergunta, pelos horizontes e pela [descrição dos insumos](../data/raw/sobrevivencia_cadastral_poa/README.md). A base individual não é versionada. A cadeia individual não pode ser seguida na árvore.
- **E06–E10** — O README indica M0/M1 e a configuração E07B. Siga LOO, diagnósticos e PPC antes das tabelas de sobrevivência e da matriz de validade. O resumo final está preservado em `resultados_auditados/resumo_executivo.csv`. Um relatório E10 numerado não foi localizado.

## UNODC Prisons and Prisoners

Quanta heterogeneidade há na taxa de custódia e na proporção de presos sem sentença? Na base nacional da UNODC, o estudo usa um modelo hurdle para a taxa de custódia e modelos separados para prisão sem sentença, total e por sexo. O histórico indica heterogeneidade entre países e um contraste feminino–masculino positivo no painel pareado, menor com composição temporal fixa. Há limitações de adequação preditiva, e o PSIS-LOO agrupado não sustentou transporte para novos países. A evidência é observacional. [README e resultados](prisons_prisoners_unodc/README.md)

- **E00–E05** — Comece pelo recorte nacional e pela [proveniência da base analítica](../data/raw/prisons_prisoners_unodc/README.md). Versão exportada e transformações de origem seguem parcialmente rastreáveis.
- **E06–E10** — O README distingue M1 REV5, M2 e M3 REV4.1 e identifica os insumos históricos. Em `resultados_auditados/`, leia a matriz de validade E08 antes de `E09_tabela_rastreabilidade.csv` e da síntese no README. Um relatório E10 numerado não foi localizado.
