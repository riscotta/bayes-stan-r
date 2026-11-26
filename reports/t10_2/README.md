# T10.2 — Reconciliação dos estados documentais e evidências

Data: 26/11/2026. Base fixa [093c2d6d1ade591685e0a28c99814048bae15c58](https://github.com/riscotta/bayes-stan-r/commit/093c2d6d1ade591685e0a28c99814048bae15c58). T10.1 integrada [PR #77](https://github.com/riscotta/bayes-stan-r/pull/77), revisão Codex sem achados no HEAD 074c0734ec6b9f8726700839c6e9702757129001 e [CI pós-merge #45854424296](https://github.com/riscotta/bayes-stan-r/actions/runs/45854424296) success (três jobs). O plano mestre exige reconciliação por unidade/etapa e cinco dimensões, sem promoção automática.

## Entregáveis

- [estados_e00_e10.csv](estados_e00_e10.csv): 363 registros, 33 unidades × 11 etapas; fonte, blob, localizador, natureza da evidência, observação, limite e diligência. A variante avaliada é o conjunto documental disponível por unidade no SHA fixado, não certificação de todas as variantes históricas. A coluna estado_anterior distingue os 11 estados efetivamente registrados no piloto T02.7 dos demais sem estado formal individual localizado no recorte.
- [dimensoes_reprodutibilidade.csv](dimensoes_reprodutibilidade.csv): 165 registros, 33 × 5 eixos. Estado literal e evidência do snapshot T06.2 preservados; estado reconciliado separado, com fonte e CI/PR quando pertinentes. Códigos de reconciliação são locais à matriz dos eixos, não estados T02.6.

## Regras e resultados

O [contrato T02.6](../../scripts/STUDY_STAGE_STATUS_T02_6.md) requer parecer independente por etapa e evidência específica para APROVADO/RESSALVA; NAO_APLICAVEL exige decisão revisada por escopo; INDISPONIVEL_NO_HISTORICO exige busca documentada/restrição comprovada e revisão. Não localizamos no recorte evidência suficiente para promover uma etapa integral. Todos os 363 registros permanecem PENDENTE, incluindo os 11 do [piloto T02.7](../../scripts/STUDY_PILOT_T02_7_MORTALIDADE_SP.md). Isso é estado de suficiência/QA, não contagem de documentos ausentes nem reprovação científica. Para as 352 demais etapas, estado inicial conservador, não transição a partir de estado anterior inventado. As 12 unidades de laboratório não recebem NAO_APLICAVEL por categoria. Ausências pontuais não autorizam declarar toda a etapa indisponível. Os pareceres anteriores têm seus escopos (editorial, técnico, N2); não foram confundidos com homologação integral por etapa.

A evidência por etapa é localizada no [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md), no [inventário T10.1](../t10_1/README.md), na árvore Git e nos roteiros [T08.4](../../scripts/STUDY_SUMMARIES_T08_4.md)/[T08.6](../../scripts/STUDY_EVIDENCE_T08_6.md). Para os oito aplicados com síntese rastreável, apontam-se evidências específicas históricas, reconstruções, limitações e, quando pertinente, N2. Os demais têm descrições de pergunta, dados, método e saídas; referências indiretas não certificam execução E03–E05. Localizadores CSV históricos foram conferidos por existência/blob, não por leitura de linhas de dados. O código listado em E07 é inventário por extensão, não auditoria matemática ou prova de execução. Não foram procurados arquivos privados nem lidas linhas de dados, não houve amostragem, ajuste, coleta ou nova avaliação inferencial. O responsável científico por etapa não foi presumido. A próxima diligência poderá envolver fonte autorizada e parecer competente.

| Eixo | T06.2 (12/11) | Reconciliação no recorte 26/11 |
| --- | ---: | ---: |
| Integridade documental N1 | 33 | 33, manifesto preservado e 33 READMEs cotejados na T10.1 |
| Síntese descritiva limitada N2 | 2 | 3 (PNAD Estudo I, Mortalidade SP, Cães/F5) |
| Pipeline integral reatestado | 0 | 0 |
| Inferência histórica reproduzida | 0 | 0 |
| Validação científica homologada | 0 | 0 |

A fonte dos cinco eixos é o [JSON T06.2](../t06_2/estados_reprodutibilidade_T06_2.json), mantido intacto. O piloto [T06.6](../t06_6/README.md) posterior registrou 30/30 cotejos de quatro agregados, com QA delimitado (PR #56, parecer COMMENTED, não aprovação humana formal). A CI pós-merge da T10.1 confirma os três passos N2: Verify published Sao Paulo mortality indicators; Audit published PNAD E09 projection (N2 only); Execute T06.6 F5 public N2 pilot and independent numeric comparison. N2 não é N3/N4. Para PNAD a comparação direta/adaptada é contemporânea, sem logs históricos autenticados. Cães/F5 e Mortalidade SP permanecem suspensos no adaptador. O parecer T09.6 e os testes nativos de dois modelos sintéticos não homologam inferência histórica das 33 unidades.

## Controles

Conferência do executor: 33 IDs idênticos entre catálogo e JSON T06.2; 363 chaves únicas estudo/etapa e 165 chaves estudo/eixo; 11 etapas por unidade, cinco eixos por unidade; estados T06.2 preservados literalmente; 3 passos N2 com success na CI de base; fontes da matriz existem com blobs no SHA fixo; ausência de promoções formais. Nenhuma alteração em READMEs, código, dados, modelos, resultados ou snapshots. Issue #14 fechada no escopo documental, permanecem restrições específicas e release integral sem decisão. T10.3–T10.7 não iniciadas. QA independente do diff e CI do HEAD exato requeridos antes do merge.

Limite desta entrega: reconciliação documental baseada em fontes públicas consolidadas, não inspeção integral de cada documento original nem auditoria de cada execução; o estado PENDENTE registra precisamente a falta de suficiência para decisão formal.
