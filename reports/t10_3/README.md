# T10.3 — Consolidação científica específica das 33 unidades

**Data:** 26/11/2026. **Base fixa:** [06ef29c7e233396809d13804989efa19f7d49538](https://github.com/riscotta/bayes-stan-r/commit/06ef29c7e233396809d13804989efa19f7d49538); T10.2 integrada no [PR #78](https://github.com/riscotta/bayes-stan-r/pull/78), [CI pós-merge #45858209999](https://github.com/riscotta/bayes-stan-r/actions/runs/45858209999) success.

## Escopo e método

Adendos científicos específicos, sem reescrever READMEs protegidos pela T06.1, resultados, modelos ou snapshots. As 33 fichas cruzam [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md), [T08.4](../../scripts/STUDY_SUMMARIES_T08_4.md), [T08.6](../../scripts/STUDY_EVIDENCE_T08_6.md), READMEs no SHA fixo e trechos específicos de implementação R/Stan (ou documentação matemática PNAD), com fonte, localizador e blob. Pergunta, dados, licença, estimandos, matemática, implementação, diagnóstico/resultado, limites, reprodutibilidade e decisões abertas aparecem em cada adendo. A descrição matemática é condensada e identifica o objeto estimado; não equivale a prova de correspondência integral por variante, revisão transversal T10.6 ou execução. A ausência de estimativa localizada não foi preenchida com número fictício. Para métodos não bayesianos (Coortes, ISUS/SIA e Mortalidade SP), não se inventa likelihood/prior bayesiana. Os registros de direitos permanecem históricos e a [decisão da issue #14](../../data/DATA_PROVENANCE_DECISIONS.md) trata de permanência, não release.

## Índice

| Unidade | Natureza documental | N2 delimitado |
| --- | --- | --- |
| [caes_raca_f5](caes_raca_f5.md) | Histórico com síntese rastreável (T08.4) | Sim |
| [censo_escolar_tempo_integral](censo_escolar_tempo_integral.md) | Aplicado com método/rota descritos; síntese final não localizada na triagem | Não reatestado |
| [consumer_shopping_trends_beta](consumer_shopping_trends_beta.md) | Laboratório metodológico | Não reatestado |
| [coortes_aposentadoria](coortes_aposentadoria.md) | Aplicado com método/rota descritos; síntese final não localizada na triagem | Não reatestado |
| [deck_15wins](deck_15wins.md) | Laboratório metodológico | Não reatestado |
| [deter_mensal_bioma_uf](deter_mensal_bioma_uf.md) | Aplicado com método/rota descritos; síntese final não localizada na triagem | Não reatestado |
| [exercises_dataset](exercises_dataset.md) | Aplicado com método/rota descritos; síntese final não localizada na triagem | Não reatestado |
| [fruitflies_aft](fruitflies_aft.md) | Laboratório metodológico | Não reatestado |
| [hidrometro_degradacao](hidrometro_degradacao.md) | Laboratório metodológico | Não reatestado |
| [hierarchical](hierarchical.md) | Laboratório metodológico | Não reatestado |
| [importacoes_world_bank](importacoes_world_bank.md) | Aplicado com método/rota descritos; síntese final não localizada na triagem | Não reatestado |
| [inhaler_ordinal](inhaler_ordinal.md) | Laboratório metodológico | Não reatestado |
| [ipca_sidra_1419](ipca_sidra_1419.md) | Aplicado com método/rota descritos; síntese final não localizada na triagem | Não reatestado |
| [isus_sia](isus_sia.md) | Aplicado com método/rota descritos; síntese final não localizada na triagem | Não reatestado |
| [kidney_survival](kidney_survival.md) | Laboratório metodológico | Não reatestado |
| [media_ruido_mu](media_ruido_mu.md) | Laboratório metodológico | Não reatestado |
| [mega_sena_dezenas](mega_sena_dezenas.md) | Histórico com síntese rastreável (T08.4) | Não reatestado |
| [missing_migrants](missing_migrants.md) | Histórico com síntese rastreável (T08.4) | Não reatestado |
| [mortalidade_sp_0_74](mortalidade_sp_0_74.md) | Histórico com síntese rastreável (T08.4) | Sim |
| [mortality](mortality.md) | Laboratório metodológico | Não reatestado |
| [pms_servicos](pms_servicos.md) | Aplicado com método/rota descritos; síntese final não localizada na triagem | Não reatestado |
| [pnadc_desocupacao](pnadc_desocupacao.md) | Aplicado com método/rota descritos; síntese final não localizada na triagem | Não reatestado |
| [pnadc_estudo_i](pnadc_estudo_i.md) | Histórico com síntese rastreável (T08.4) | Sim |
| [prisons_prisoners_unodc](prisons_prisoners_unodc.md) | Histórico com síntese rastreável (T08.4) | Não reatestado |
| [retractions_reason_composition](retractions_reason_composition.md) | Aplicado com método/rota descritos; síntese final não localizada na triagem | Não reatestado |
| [retractions_time_to_retraction](retractions_time_to_retraction.md) | Aplicado com método/rota descritos; síntese final não localizada na triagem | Não reatestado |
| [retractions_trend](retractions_trend.md) | Aplicado com método/rota descritos; síntese final não localizada na triagem | Não reatestado |
| [rs_seguro](rs_seguro.md) | Aplicado com método/rota descritos; síntese final não localizada na triagem | Não reatestado |
| [saheart](saheart.md) | Laboratório metodológico | Não reatestado |
| [sat_model_selection](sat_model_selection.md) | Laboratório metodológico | Não reatestado |
| [sherlock_holmes_terms](sherlock_holmes_terms.md) | Histórico com síntese rastreável (T08.4) | Não reatestado |
| [sobrevivencia_cadastral_poa](sobrevivencia_cadastral_poa.md) | Histórico com síntese rastreável (T08.4) | Não reatestado |
| [therapeutic_touch](therapeutic_touch.md) | Laboratório metodológico | Não reatestado |

## Controles e fronteiras

- 33/33 adendos, 12 laboratórios + 21 aplicados; oito aplicados com síntese histórica, 13 sem síntese final localizada na triagem anterior.
- 53 localizadores específicos de implementação/documentação (alguns apontam ao mesmo README; não são execuções). Os caminhos e blobs foram conferidos contra a árvore Git não truncada do SHA base.
- Os três N2 são apenas Cães/F5, Mortalidade SP e PNAD Estudo I; não há nova amostragem, análise de dados, coleta ou teste inferencial na T10.3. CI da T10.2 não valida cientificamente cada unidade. Nenhum estado E00–E10 promovido; [T10.2](../t10_2/README.md) permanece referência.
- Os sumários históricos e parâmetros são identificados como relatos e especificações existentes; não são resultados novos. Diligências e questões em aberto são recomendações editoriais, não achados comprovados de falha.
- T10.4–T10.7 não iniciadas; revisão de navegação T10.5 e revisão técnico-científica transversal T10.6 têm escopos próprios. Release integral sem decisão. QA independente do diff e CI do HEAD exato antes de integrar.

