# T10.4 — Identificação de templates e matrizes documentais

**Data:** 26/11/2026. **Base fixa:** [9f13421e1799512d9178f6466ae0d0cd4dcaac77](https://github.com/riscotta/bayes-stan-r/commit/9f13421e1799512d9178f6466ae0d0cd4dcaac77). T10.3 integrada no [PR #79](https://github.com/riscotta/bayes-stan-r/pull/79), [CI pós-merge #45863317532](https://github.com/riscotta/bayes-stan-r/actions/runs/45863317532) success. Escopo definido na [T10.1](../t10_1/README.md): cinco contratos T02.2–T02.6 e Quarto.

## Mudanças

Cada documento recebeu no início do corpo o aviso visível **MODELO PARA PREENCHIMENTO — NÃO É RESULTADO DE ESTUDO**, esclarecimento de natureza e, nos blocos críticos, identificação local do exemplo estrutural. Em T02.2 e T02.6, os exemplos históricos/condicionais e as regras normativas continuam distintos do formulário copiável. No Quarto, o front matter e a célula executável são mantidos, com a identificação no corpo renderizável. Não foi realizada renderização neste ciclo.

| Documento | Blob original no SHA base | Linhas acrescentadas | Escopo |
| --- | --- | ---: | --- |
| [STUDY_METADATA_CONTRACT_T02_2.md](../../scripts/STUDY_METADATA_CONTRACT_T02_2.md) | `ff850f226810956e2ed7950a6cdd50e889566749` | 10 | Contrato e exemplos históricos |
| [STUDY_E00_TEMPLATE_T02_3.md](../../scripts/STUDY_E00_TEMPLATE_T02_3.md) | `a66b926ab9afd52f79ae620d32b17e775a00c14c` | 8 | E00 e registro ilustrativo de pendências |
| [STUDY_E06_E07_MATRIX_T02_4.md](../../scripts/STUDY_E06_E07_MATRIX_T02_4.md) | `58576d08c6890a422d7d97a3e0505323ab49dc87` | 8 | Matriz E06 → E07 |
| [STUDY_E08_E09_MATRIX_T02_5.md](../../scripts/STUDY_E08_E09_MATRIX_T02_5.md) | `e119d23797383da5c46addb5efcdec9dfda9c8af` | 8 | Matriz E08 → E09 |
| [STUDY_STAGE_STATUS_T02_6.md](../../scripts/STUDY_STAGE_STATUS_T02_6.md) | `268780885df7a9530a414614c64a4c35ce75f729` | 8 | Contrato de estados e formulário E00–E10 |
| [template.qmd](../../reports/template/template.qmd) | `a3354b55c39e2b788fa42aae3b0be7b92933ffae` | 8 | Modelo Quarto e exemplo de ambiente |

## Como interpretar os marcadores

| Contexto | Natureza | Tratamento |
| --- | --- | --- |
| Formulário não instanciado | Placeholder estrutural | Não contar como lacuna factual de estudo. Preservar códigos iniciais, IDs e campos obrigatórios. |
| Unidade identificada; informação faltante ou não examinada | Pendência factual ou de verificação | Registrar motivo, fontes consultadas, impacto, diligência e responsável comprovado, se conhecido. Não inferir inexistência ou falha. |
| Etapa identificada, avaliada no escopo/versão definidos | Estado formal do contrato T02.6 | Aplicar dicionário, evidência e QA; PENDENTE é estado inicial ou insuficiência de evidência, não reprovação nem inexistência comprovada. |
| Estado de decisão ou linha de matriz | Estado local | Aplicar T02.3/T02.4/T02.5 conforme o caso; não equiparar ABERTA ou CONFORME_CODIGO a estado de etapa. |
| Exemplo histórico ou cenário hipotético | Registro contextual ou ilustração | Respeitar o recorte; não tratar como execução ou homologação atual. |

Para estados reconciliados de unidades existentes, consultar [T10.2](../t10_2/README.md) e a [matriz E00–E10](../t10_2/estados_e00_e10.csv), não o formulário vazio. Os 363 registros PENDENTE da T10.2 são registros por unidade/etapa, com justificativas e limites, não contagem de placeholders do template nem de falhas provadas. Não há alteração das matrizes T10.2 ou dos [adendos T10.3](../t10_3/README.md).

## Controles do executor

- Seis arquivos no escopo, um relatório novo. Somente inserções textuais nos seis: removendo as inserções, o conteúdo original é recuperado exatamente, inclusive cabeçalhos, tabelas, IDs, marcadores, critérios e checklists. Nenhum campo existente reescrito ou preenchido.
- Cinco testes estruturais T02.2–T02.6 foram inspecionados; as linhas que verificam permanecem intactas. Os testes R não foram executados pelo executor nesta tarefa; a CI do HEAD exato é o gate de execução.
- Não se alteraram snapshots, READMEs de estudos, código científico, dados, resultados, permissões ou estados. A revisão é de apresentação, não nova taxonomia nem homologação.
- O template institucional T01.3, o template de PR e os documentos históricos preenchidos não integram os seis artefatos delimitados na T10.1. Não os alterar nem reclassificar como modelos de estudo.
- QA independente e CI do HEAD exato necessários antes do merge. T10.5–T10.7 não iniciadas. Release integral sem decisão.

