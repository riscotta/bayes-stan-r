# T10.1 — Inventário documental das 33 unidades

26/11/2026. Base: [main 6cb8177b36b8f2fdaa48667fcc2c5c2c01b4078e](https://github.com/riscotta/bayes-stan-r/commit/6cb8177b36b8f2fdaa48667fcc2c5c2c01b4078e), árvore `a491056a2bc5c8e630fe57c7b84f416d197e270a`. Escopo: inventário, não QA independente. Abertura solicitada pelo responsável. [PR #75](https://github.com/riscotta/bayes-stan-r/pull/75) e [#76](https://github.com/riscotta/bayes-stan-r/pull/76) integrados; [CI pós-merge](https://github.com/riscotta/bayes-stan-r/actions/runs/45817064272) verde. [Deck publicado](https://github.com/riscotta/bayes-stan-r/commit/24f6a6f4c827de6437799b2eb841afb4ae102261) em downloads/deck; release integral sem decisão. [Ruleset 26061035](https://github.com/riscotta/bayes-stan-r/rules/26061035) ativo, exige PR e check R; endpoint detalhado de branch protection retornou 403, ruleset consultado com sucesso.

## Cobertura e critérios

Árvore não truncada: 633 entradas. Os 33 READMEs foram obtidos no SHA fixo; 33/33 blobs e tamanhos coincidem com [T06.1](../t06_1/manifesto_estudos_T06_1.json). [Taxonomia T02.1](../../scripts/STUDY_TAXONOMY_T02_1.md): 12 laboratórios e 21 aplicados; _setup é infraestrutura. Classes locais de triagem (não são estados E00–E10): L = laboratório, 12; H = aplicado com síntese histórica rastreável, 8; R = aplicado com rota/implementação descrita, sem síntese final localizada na [T08.4](../../scripts/STUDY_SUMMARIES_T08_4.md), 13. R não significa estudo inexistente/inconcluso; L não implica E00–E10 inaplicáveis. READMEs são documentação específica, não templates vazios nem prova de reexecução.

D = arquivos Markdown/CSV/Quarto em reports/<id>/ (exclui figuras); B = blobs em data/raw/<id>/ exceto Markdown e .gitkeep (insumos, planos ou resultados; não são todos resultados); O = .gitkeep/outros blobs em outputs/<id>/. Contagens de localização, não aprovação, não incluem caminhos compartilhados. Zero em O não implica ausência de resultado histórico.

| Unidade / README | Blob README | D | B | O | Natureza / diligência |
| --- | --- | ---: | ---: | --- | --- |
| [caes_raca_f5](../../scripts/caes_raca_f5/README.md) | `7656e617336ff5928af83c556edf48bcd5c69462` | 1 | 5 | 0/0 | H — relatório E10, E08_MP e piloto N2; E04/cadeias externas, adaptador suspenso |
| [censo_escolar_tempo_integral](../../scripts/censo_escolar_tempo_integral/README.md) | `459f4a074ac87283110f8bebd3f4ab72b129168d` | 0 | 0 | 1/0 | R — rota/método descritos; síntese final versionada não localizada em T08.4 |
| [consumer_shopping_trends_beta](../../scripts/consumer_shopping_trends_beta/README.md) | `6627d311bbf5d9af4a11ddd30ad67330487c6bd0` | 0 | 0 | 3/0 | L — exemplo metodológico; avaliar aplicabilidade por etapa |
| [coortes_aposentadoria](../../scripts/coortes_aposentadoria/README.md) | `97c14c49ceb7473b836576b65a8848270c7e83d6` | 0 | 0 | 0/0 | R — rota/método descritos; síntese final versionada não localizada em T08.4 |
| [deck_15wins](../../scripts/deck_15wins/README.md) | `39631212e9123f3d10a9c24bf7f003cc26ee3f60` | 0 | 0 | 0/0 | L — simulação sintética; publicação Deck não é validação empírica nem release integral |
| [deter_mensal_bioma_uf](../../scripts/deter_mensal_bioma_uf/README.md) | `9bbbf7c2e8d9c14089039bf1116af5c133869b79` | 0 | 0 | 0/0 | R — rota/método descritos; síntese final versionada não localizada em T08.4 |
| [exercises_dataset](../../scripts/exercises_dataset/README.md) | `5e9ce43dd72f357e814f5116fb1b297adc37e319` | 0 | 1 | 4/0 | R — base e implementação, síntese final não localizada |
| [fruitflies_aft](../../scripts/fruitflies_aft/README.md) | `3dac347171dfc11a582f88810f03c4796900d130` | 0 | 0 | 0/0 | L — exemplo metodológico; avaliar aplicabilidade por etapa |
| [hidrometro_degradacao](../../scripts/hidrometro_degradacao/README.md) | `c773cc4a753d3e16b81eb2420e65ec51ef767374` | 0 | 0 | 0/0 | L — exemplo metodológico; avaliar aplicabilidade por etapa |
| [hierarchical](../../scripts/hierarchical/README.md) | `48e03852fdb4c4a7b1a3539d0ee52a23dcfb1d4b` | 0 | 0 | 0/0 | L — pooling parcial; licença específica do insumo não identificada em T09.3 |
| [importacoes_world_bank](../../scripts/importacoes_world_bank/README.md) | `7d24ad2763ee505da3f5e08831f9b1b0c1bda088` | 0 | 1 | 3/0 | R — base e implementação, síntese final não localizada |
| [inhaler_ordinal](../../scripts/inhaler_ordinal/README.md) | `5b9cc172d38d9767bb31511e7be17a8a09e90372` | 0 | 0 | 0/0 | L — exemplo metodológico; avaliar aplicabilidade por etapa |
| [ipca_sidra_1419](../../scripts/ipca_sidra_1419/README.md) | `2ed6412c4f81ed957244a782a46d09407c9e1c63` | 0 | 0 | 1/0 | R — rota/método descritos; síntese final versionada não localizada em T08.4 |
| [isus_sia](../../scripts/isus_sia/README.md) | `6b78e33f36c1474cbe41446aa887dd7c6ed96724` | 0 | 1 | 0/0 | R — Monte Carlo, síntese final não localizada, proveniência parcial |
| [kidney_survival](../../scripts/kidney_survival/README.md) | `76c147e6d7acd95623e22e4250d47f4d1849bba4` | 0 | 0 | 0/0 | L — exemplo metodológico; avaliar aplicabilidade por etapa |
| [media_ruido_mu](../../scripts/media_ruido_mu/README.md) | `5b0d1c59a80d5a9417eb6bf395479bbb50d9fc52` | 0 | 0 | 0/0 | L — exemplo metodológico; avaliar aplicabilidade por etapa |
| [mega_sena_dezenas](../../scripts/mega_sena_dezenas/README.md) | `d875f490f04f6d482f24c36264e116add878893f` | 0 | 20 | 0/0 | H — E09 e diagnósticos; alternativa E07 substituta, configuração planejada; E10 separado não localizado |
| [missing_migrants](../../scripts/missing_migrants/README.md) | `17876280be00c2497604041f9f051553896ea2c1` | 0 | 16 | 5/0 | H — sínteses E10, PPC e convergência com ressalvas; recomposição não é novo ajuste |
| [mortalidade_sp_0_74](../../scripts/mortalidade_sp_0_74/README.md) | `7e2d093b17edd068940ee6069439b96b567eb31d` | 6 | 13 | 2/0 | H — caderno E01–E10, E10, matriz, piloto T02.7 editorial; base territorial provisória, adaptador suspenso |
| [mortality](../../scripts/mortality/README.md) | `b9a32d700a6296275279c6e2bd910144852df5fd` | 0 | 1 | 0/0 | L — Poisson com offset; estrutura XLSX conferida, sem classificação semântica |
| [pms_servicos](../../scripts/pms_servicos/README.md) | `90fb414151788c508c1b80a5091eb6715c0d84e6` | 0 | 1 | 0/0 | R — rota/método descritos; síntese final versionada não localizada em T08.4 |
| [pnadc_desocupacao](../../scripts/pnadc_desocupacao/README.md) | `869d564737a51d9c97eb32fa5eb5999e6f23e1c4` | 0 | 3 | 0/0 | R — rota/método descritos; síntese final versionada não localizada em T08.4 |
| [pnadc_estudo_i](../../scripts/pnadc_estudo_i/README.md) | `f1fd01d06f2ec03092ab11cedac3a4e7e946dc8f` | 15 | 1 | 0/0 | H — nota, T03/T04, E00 editorial e N2; logs originais indisponíveis, covariância de desenho não identificada |
| [prisons_prisoners_unodc](../../scripts/prisons_prisoners_unodc/README.md) | `645842fb3ee0756dee711b0936ca3327285704b6` | 0 | 19 | 0/0 | H — E08/E09, limites de adequação e transporte; E10 numerado não localizado |
| [retractions_reason_composition](../../scripts/retractions_reason_composition/README.md) | `712695bc4404aa03b5617dcebad0ecaf94bddf50` | 0 | 0 | 0/0 | R — rota/método descritos; síntese final versionada não localizada em T08.4 |
| [retractions_time_to_retraction](../../scripts/retractions_time_to_retraction/README.md) | `c878e2cb31c73f83caf179c1727b2455d0372555` | 0 | 0 | 3/0 | R — rota/método descritos; síntese final versionada não localizada em T08.4 |
| [retractions_trend](../../scripts/retractions_trend/README.md) | `820574b8c9b34f1d5c682d171f9eab452b436647` | 0 | 0 | 0/0 | R — rota/método descritos; síntese final versionada não localizada em T08.4 |
| [rs_seguro](../../scripts/rs_seguro/README.md) | `2599c242b6d2e7243a5996608e3a9e34beafd0f6` | 0 | 2 | 0/0 | R — M1/M2, síntese final não localizada; M2 conta ocorrências mensais com uma vítima por sexo, faixa e crime; alerta de triagem T09.1 |
| [saheart](../../scripts/saheart/README.md) | `ed08118b20675530d7074db9a68ae42ac51b451c` | 0 | 0 | 0/0 | L — exemplo metodológico; avaliar aplicabilidade por etapa |
| [sat_model_selection](../../scripts/sat_model_selection/README.md) | `05c8db1d877b896ab070ce8538dfd8299e37d19e` | 0 | 0 | 0/0 | L — exemplo metodológico; avaliar aplicabilidade por etapa |
| [sherlock_holmes_terms](../../scripts/sherlock_holmes_terms/README.md) | `ef7276644ed479076d6f1e780c9a9e6f87dc4fc4` | 0 | 16 | 1/0 | H — tabelas finais, sensibilidade a priors/NB2 pendente; E10 numerado não localizado |
| [sobrevivencia_cadastral_poa](../../scripts/sobrevivencia_cadastral_poa/README.md) | `b9346f159ac123dadf46ffa954d4ff4ecd9e61f0` | 0 | 15 | 0/0 | H — resumo executivo, base individual externa; previsão de coortes futuras e dependência entre unidades não validadas; E10 numerado não localizado |
| [therapeutic_touch](../../scripts/therapeutic_touch/README.md) | `d41ed21eb282918f9b4d12eb65f3d7ea703de647` | 0 | 0 | 0/0 | L — exemplo metodológico; avaliar aplicabilidade por etapa |

Totais D=22, B=115, O=23 placeholders/0 outros blobs. Três conjuntos de relatórios específicos: Cães/F5, Mortalidade SP e PNAD Estudo I; cinco outros H têm síntese no README e artefatos congelados. [T08.6](../../scripts/STUDY_EVIDENCE_T08_6.md) distingue versões, execução e síntese offline.

## Páginas, modelos e registros de estado

| Documento | Natureza / tratamento |
| --- | --- |
| [README](../../README.md), [scripts](../../scripts/README.md), [relatórios](../README.md), [saídas](../../outputs/README.md), [CONTRIBUTING](../../CONTRIBUTING.md) | Entrada, navegação e norma; revisar inconsistências T10.5. |
| [T02.2](../../scripts/STUDY_METADATA_CONTRACT_T02_2.md) | Contrato/modelo de oito metadados; marcador estrutural não é lacuna factual. |
| [T02.3](../../scripts/STUDY_E00_TEMPLATE_T02_3.md) | E00 vazio; não é protocolo executado. |
| [T02.4](../../scripts/STUDY_E06_E07_MATRIX_T02_4.md) | Matriz matemática–código vazia, não prova equivalência. |
| [T02.5](../../scripts/STUDY_E08_E09_MATRIX_T02_5.md) | Matriz evidência–conclusão vazia, não resultado. |
| [T02.6](../../scripts/STUDY_STAGE_STATUS_T02_6.md) | Contrato de estados e formulário vazio; PENDENTE estrutural não equivale a pendência factual. |
| [T02.7](../../scripts/STUDY_PILOT_T02_7_MORTALIDADE_SP.md) | Piloto preenchido, reconstrução editorial, não E00 original; preservar corte. |
| [Quarto](../template/template.qmd) | Modelo de relatório, não documento científico específico. |
| [T08.2](../../scripts/STUDY_CATALOG_T08_2.md), [T08.4](../../scripts/STUDY_SUMMARIES_T08_4.md), [T08.5](../../scripts/STUDY_EXAMPLES_T08_5.md), [T08.6](../../scripts/STUDY_EVIDENCE_T08_6.md), [T08.7](../t08_7/README.md) | Catálogo, reconstruções e revisão editorial com cortes próprios. |
| [T06.1](../t06_1/README.md), [T06.2](../t06_2/README.md), [T06.3](../t06_3/README.md), [T06.4](../t06_4/README.md), [T06.5](../t06_5/README.md), [T06.6](../t06_6/README.md), [T06.7](../t06_7/README.md) | Integridade, cinco eixos, três modos, custódia, dependências, piloto, revisão. Preservar snapshots e evidência posterior separada. |
| [T07.6](../t07_6/README.md), [T07.7](../t07_7/README.md), [T09](../t09/README.md) | Registros técnicos com cortes próprios; fechamento T07.6 no escopo revisado, não equivalência histórica integral. |
| [T05.7](../t05_7/relatorio_evidencias_T05_7.md), [gate científico](../SCIENTIFIC_VALIDITY_GATE_2026-10-28.md), [T08.3](../t08_3/README.md) | Evidência técnica/testes delimitados, não validação científica. |
| [Proveniência](../../data/DATA_PROVENANCE.md), [decisões](../../data/DATA_PROVENANCE_DECISIONS.md), [manifesto](../../data/TRACKED_DATA_MANIFEST.csv) | Custódia e permanência; issue #14 fechada documentalmente, não licença irrestrita. |

## Pendências e encaminhamento

- T10.2: 33 × 11 = 363 registros E00–E10 a reconciliar; não atribuir estados aqui. Cinco eixos T06.2 independentes: snapshot original 33 N1, 2 auditorias limitadas, zero pipeline/inferência/validação reatestados; piloto Cães N2 posterior separado. Não promover N2 a N3/N4.
- T10.3: localizar documentação/sínteses adicionais para 13 aplicados, se existentes/autorizadas; preservar limitações dos oito H, avaliar escopo dos laboratórios. Ausência na triagem não prova inexistência.
- T10.4: apresentar cinco modelos T02.2–T02.6 e Quarto como modelos, nunca resultados; não preencher artificialmente.
- T10.5: o índice reports/README.md ainda diz que T07.6/Etapa 07 não foram concluídas e #14 está aberta. Corrigir índice vigente, preservar snapshots históricos.
- Pendências efetivas: logs originais PNAD não comparados; adaptadores Cães/F5 e Mortalidade SP suspensos; 14 conjuntos/83 arquivos com rastreabilidade parcial e alertas de triagem T09.1; 11 ocorrências externas inconclusivas T08.3; issues #15/#16 e PR #64 conforme T09.5. T09.2 selecionou 2/20 programas Stan para execução nativa sintética, não valida os demais. Release integral exige composição, licença, autorização e deliberação específica.
- Sem alteração de READMEs protegidos, código, dados, estimandos, outputs, manifestos ou snapshots. Nenhum ajuste, inspeção de linhas de dados ou resultado novo. T10.1 não substitui T10.2–T10.7.

## Verificações do executor

33/33 READMEs e correspondência de blob/tamanho T06.1, taxonomia 12/21 e triagem 12/8/13; links internos deste documento resolvidos contra a árvore base. Classificação ancorada nos READMEs e T08.4/T08.6; não constitui leitura integral de todos os relatórios/dados. QA independente e CI no HEAD exato requeridos antes de integração.
