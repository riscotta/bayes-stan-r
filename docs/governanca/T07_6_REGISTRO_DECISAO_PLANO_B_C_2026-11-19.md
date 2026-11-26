# Bayes-Stan-R — T07.6 | Registro de decisão do responsável pelo plano — B + C

**ID do registro:** `T07.6-DEC-PLANO-BC-2026-11-19`  
**Data da manifestação:** 19/11/2026  
**Fonte da manifestação:** instrução expressa do usuário nesta conversa, na qualidade **declarada** de responsável pelo plano do projeto. O registro não contém assinatura digital verificável, documento de delegação ou validação externa da identidade/competência.  
**Natureza:** decisão sobre encaminhamento e revisão de escopo **do plano**; termo para validação e rastreamento, não ato de custódia institucional, homologação científica, aprovação independente ou autorização de acesso a dados.  
**Status documental:** `DECISAO_PLANO_DECLARADA_REGISTRADA_PENDENTE_VALIDACAO_QA_E_INTEGRACAO`.

## 1. Manifestação registrada

> Aprovo a proposta B + C de revisão do escopo da T07.6 como responsável pelo plano do projeto, mantendo explícitas as pendências de custódia e os bloqueios científicos. Prepare o registro formal da decisão para validação e os próximos passos, sem presumir autorização institucional de terceiros ou QA independente.

**Alcance verificável:** esta manifestação aprova, **no nível de gestão do plano**, o encaminhamento B + C apresentado no PR #64. Não aprova automaticamente a dispensa histórica de nenhum objeto, não comprova competência institucional para autorizar dados de terceiros, não certifica execução/QA da T07.6 e não autoriza merge dos PRs #63/#64.

## 2. Decisão sobre o critério C — escopo revisado

**Deliberação no âmbito do plano:** adotar como direção aprovada o texto substitutivo abaixo, sujeito a validação técnico-científica independente, formalização/integridade no plano mestre e integração autorizada antes de produzir efeitos como critério operativo:

> Para cada saída de adaptador efetivamente executável e autorizada, identificar função, proveniência, homologia e referência histórica. Quando houver referência preexistente, autenticada, legalmente utilizável e comparável, realizar confronto conforme a T06.4, com protocolo e critérios fixados antes da análise. Quando um artefato novo não tiver homólogo histórico tecnicamente possível, somente admitir não aplicabilidade histórica específica mediante decisão competente documentada, diligência e QA, preservando testes de contrato, esquema e integridade. Rotas bloqueadas por execução, direitos, custódia ou exigências científicas permanecem suspensas, não recebem PASS nem dispensa implícita e devem manter gates de reabertura. Qualquer aceite parcial deve explicitar os objetos cobertos, as exclusões, os riscos e o trabalho remanescente; não demonstra migração global nem N3/N4.

**Efeito imediato:** autorização para registrar a decisão de gestão do plano e submetê-la à revisão; **não** significa que a redação original foi substituída na `main`, que o PR #63 pode ser integrado ou que a T07.6 foi concluída.

## 3. Diretriz B por objeto — aprovação de encaminhamento, não homologação

| ID | Diretriz aprovada no plano | Estado técnico e de governança que permanece |
|---|---|---|
| `PNAD_STDOUT` | Diligenciar a saída `stdout.txt`; conservar comparação operacional N2 estritamente limitada. | `PENDENTE_CUSTODIA`; o log textual histórico não comprova bytes de arquivo original nem equivalência histórica independente. |
| `PNAD_STDERR` | Investigar captura e custódia de `stderr.txt` antes de qualquer dispensa. | `PENDENTE_CUSTODIA`; silêncio do log não prova saída vazia. |
| `PNAD_REPORT` | Submeter `report.json` à decisão individual de não aplicabilidade histórica, pois foi introduzido pelo wrapper; exigir validação de contrato. | `NAO_APLICAVEL_HISTORICO_PROPOSTO`, **não homologado**; decisão competente e QA ainda exigidos. |
| `MORT_SP` | Manter outputs do adaptador R fora do escopo executável atual, com backlog e gates explícitos. | `SUSPENSO_POR_CONTRATO`; sem reexecução, comparação ou PASS. |
| `CAES_F5` | Manter outputs do adaptador Stan/MCMC suspensos até autorizações e protocolo apropriado. | `SUSPENSO_POR_CONTRATO`; sem sampling, comparação ou PASS. |

Esta decisão **não transforma** `PENDENTE_CUSTODIA` em `NAO_APLICAVEL_HISTORICO`, nem `SUSPENSO_POR_CONTRATO` em tarefa concluída. Uma avaliação documental limitada à PNAD pública não encerra a obrigação futura de examinar os outros dois estudos.

## 4. Evidências, rastreabilidade e limites

- Repositório: `riscotta/bayes-stan-r`.
- Base de preparação: `main` `77930248d5fdad81b999bc5741bbb930f4f97858` (confirmar novamente antes de integrar).
- PR documental B + C: [#64](https://github.com/riscotta/bayes-stan-r/pull/64), criado como **draft**; o SHA final e a CI desta revisão devem ser verificados após o novo commit.
- PR técnico T07.6: [#63](https://github.com/riscotta/bayes-stan-r/pull/63), não aprovado para integração por esta decisão; seu parecer anterior continua inconclusivo para o objetivo histórico literal.
- Evidência histórica PNAD: [workflow #43440975355](https://github.com/riscotta/bayes-stan-r/actions/runs/43440975355), log textual de 13/11/2026, **não** par de bytes de referência autenticado.
- Controle PNAD N2: CI do PR #63 [#44356334938](https://github.com/riscotta/bayes-stan-r/actions/runs/44356334938), com igualdade operacional sob mesmo código/entrada/ambiente, sem homologação histórica.
- Política preservada: `reports/t06_4/README.md`. Issues de risco [#14](https://github.com/riscotta/bayes-stan-r/issues/14) (P0), [#15](https://github.com/riscotta/bayes-stan-r/issues/15) e [#16](https://github.com/riscotta/bayes-stan-r/issues/16) não são encerradas por este registro.

**Não verificado por este registro:** assinatura ou ata de custodiantes, responsabilidade científica de terceiros, direitos/licenças de cópias institucionais, independência de referências, cadeia de custódia externa, autenticação do usuário por terceiro, QA e validade inferencial dos modelos.

## 5. Condições de validação e eficácia

| Gate | Responsável | Evidência/decisão necessária | Estado nesta emissão |
|---|---|---|---|
| G1 — escopo do plano | Responsável pelo plano, identidade/competência confirmadas conforme governança aplicável | Ratificação verificável do alcance do aceite parcial; referência à revisão C | `MANIFESTACAO_DECLARADA; VALIDACAO_PENDENTE` |
| G2 — cada artefato | Custodiante e/ou responsável técnico-científico competente por objeto | Decisão datada e fundamentada, direitos/custódia/diligência, assinatura ou referência institucional verificável, publicada apenas se autorizada | `PENDENTE` |
| G3 — revisão independente | Chat/revisor independente do executor, e responsáveis formais quando exigidos | Parecer sobre diff e SHA exato do PR #64, mérito da revisão C, ausência de dispensas indevidas e CI conclusiva | `PENDENTE` |
| G4 — implementação controlada | Executor subsequente | Aplicar somente mudanças de documentação/gates necessárias, preservar T06.4 e os bloqueios científicos, revalidar PR #63 se houver mudanças | `PENDENTE` |
| G5 — integração | Revisor/integrador autorizado | Checks no SHA final, parecer favorável e regras de proteção da `main`; verificação de squash e CI posterior | `PENDENTE` |

Uma ratificação do responsável pelo plano não supre o G2. Uma CI verde não supre G1, G2 ou G3. A condição de não aplicabilidade para `PNAD_REPORT` não deve ser consolidada como aprovada até ser atendida a competência decisória e a revisão específica.

## 6. Próximas ações ordenadas

1. **Validar o registro de decisão do plano:** conferir escopo, autoria/competência, versão e eventual necessidade de assinatura/ata sob os procedimentos aplicáveis. A conversa é evidência da manifestação, não assinatura certificada.
2. **Encaminhar a matriz de cinco objetos aos responsáveis competentes:** obter resposta separada sobre `PNAD_REPORT` e diligência de `PNAD_STDOUT`/`PNAD_STDERR`; manter `MORT_SP`/`CAES_F5` suspensos. Não anexar dados institucionais protegidos ao PR.
3. **Submeter o PR #64 a QA independente:** inspecionar SHA, somente documentação, CI, precisão da redação, direitos, T06.4 e o risco de confundir aprovação de plano com liberação científica; registrar parecer.
4. **Se os gates permitirem, ajustar o plano mestre e o PR #63 em ciclo executor próprio:** delimitar formalmente um escopo PNAD N2 auditável e backlog dos estudos R/Stan, sem reescrever resultados históricos e sem execução proibida; novo SHA, testes e QA do que for alterado.
5. **Integrar apenas após autorização e QA aplicáveis:** confirmar a `main` e CI pós-merge. Só então avaliar o fechamento de subtarefas efetivamente aceitas; a Etapa 07 permanece aberta enquanto houver obrigações não satisfeitas.

## 7. Declaração de estados preservados

- `T07.6 Exec. ☐ / QA ☐`; `T07.7 Exec. ☐ / QA ☐`; **Etapa 07 aberta**.
- Sem certificação N3/N4, sem reexecução R/Stan/MCMC e sem novos resultados científicos.
- PR #63 e PR #64 **não aprovados para merge** em razão desta manifestação.
- O plano revisado foi **aprovado como direção pelo responsável declarado**, mas sua eficácia técnica/operacional depende dos gates acima.

**Campos para validação formal fora deste registro:** responsável identificado, função e limites de competência, data/hora e referência verificável da ratificação/ata/assinatura, responsáveis pelos cinco objetos, parecer de QA independente, vínculo ao SHA aprovado e decisão de integração. Não preencher esses campos sem prova.