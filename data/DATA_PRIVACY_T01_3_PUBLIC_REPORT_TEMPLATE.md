# T01.3 — Modelo de relatório público sanitizado de inspeção de privacidade

> **MODELO NÃO PREENCHIDO — NÃO É LAUDO, AUTORIZAÇÃO OU QA.** Este arquivo é exclusivamente estrutural, criado para orientar um relatório posterior à inspeção no ambiente institucional protegido. Não inserir registros, linhas/células, nomes de pessoas, números de documentos, cabeçalhos livres, combinações raras, capturas de tela, trechos de autorização ou logs institucionais. Não copiar o formulário interno de evidências para GitHub.

## 1. Identificação da execução

| Campo | Preenchimento público permitido |
| --- | --- |
| Tarefa | T01.3 do Plano de Correção v1.1 |
| Data do exame | PENDENTE |
| Código do parecer institucional | PENDENTE — somente referência opaca cuja divulgação seja permitida |
| Função da autoridade que homologou a inspeção | PENDENTE — citar cargo institucional, se autorizado, sem dados pessoais desnecessários |
| Identificador da cópia/snapshot | PENDENTE — identificador não sensível, sem caminho protegido ou hash não autorizado |
| Operador autorizado | PENDENTE — registrar somente papel funcional, se permitido |
| Parecer de divulgação deste próprio relatório | PENDENTE — verificação institucional obrigatória |
| PR e HEAD da execução real | PENDENTE — não usar o PR documental #22 como evidência de exame de conteúdo |

**Proibições:** não colocar aqui o documento de autorização, contato, nome de titular, identificador direto, conteúdo de dados, captura de tela, log, caminho interno ou conjunto de variáveis que facilite reidentificação.

## 2. Gates de controle — registrar só o estado comprovado

| Gate | Estado permitido | Critério de evidência interna, nunca anexada |
| --- | --- | --- |
| G1. Autoridade | PENDENTE / VALIDADO INTERNAMENTE / INSUFICIENTE | Instituição, competência do emissor e custodiante confirmados |
| G2. Escopo e direito de exame/publicação | PENDENTE / VALIDADO INTERNAMENTE / INSUFICIENTE | Cópia, finalidade, operadores, prazo, exame e publicação cobertos explicitamente |
| G3. Ambiente protegido | PENDENTE / VALIDADO INTERNAMENTE / INSUFICIENTE | Acesso, armazenamento, logs protegidos e restrições de exportação conferidos |
| G4. Custódia e inspeção local | PENDENTE / VALIDADO INTERNAMENTE / INSUFICIENTE | Integridade/versionamento e análise local documentada |
| G5. Parecer sanitizado | PENDENTE / VALIDADO INTERNAMENTE / INSUFICIENTE | Achados revisados por autoridade competente e divulgação aprovada |

**Importante:** a declaração do responsável pelo projeto de que existem autorização e ambiente apropriados **não substitui** o registro interno da validação desses cinco controles. Somente após as verificações o estado poderá mudar para `VALIDADO INTERNAMENTE`.

## 3. Matriz de execução por dataset

| `dataset_id` | Autorização de inspeção/publicação | Exame local efetivo | Conclusão pública de privacidade | Limitação | QA independente |
| --- | --- | --- | --- | --- | --- |
| `isus_sia` | DECLARADA; NÃO VALIDADA | NÃO EXECUTADO | PENDENTE | Avaliar ZIP em ambiente autorizado | PENDENTE |
| `rs_seguro_macrocrime` | DECLARADA; NÃO VALIDADA | NÃO EXECUTADO | PENDENTE | Validar escopo do extrato exato | PENDENTE |
| `rs_seguro_macrocrime_profile` | DECLARADA PARA RS SEGURO; ESCOPO DESTA CÓPIA NÃO VALIDADO | NÃO EXECUTADO | PENDENTE | Revisão particular de granularidade/risco de combinação | PENDENTE |
| `mortality` | NÃO DECLARADA | NÃO EXECUTADO | PENDENTE | Classificação e autorização da planilha não comprovadas | PENDENTE |
| `missing_migrants` | NÃO DECLARADA PARA A CÓPIA LOCAL | NÃO EXECUTADO | PENDENTE | Termos primários IOM e análise protegida de incidentes ainda pendentes | PENDENTE |

**Status permitidos na versão final:** `INSPECIONADO` / `NÃO INSPECIONADO`; `RISCO RESIDUAL DOCUMENTADO` / `MEDIDAS EXIGIDAS` / `INCONCLUSIVO`; `QA APROVADO` / `QA REPROVADO` / `QA PENDENTE`. Não usar `SEM RISCO` ou `ANÔNIMO` sem fundamento e escopo verificáveis. Para `mortality` e `missing_migrants`, manter pendência até haver procedimento autorizado próprio.

## 4. Método mínimo para análise protegida — não relatar observações individuais

1. Identificar *localmente* campos que permitam identificação direta e indireta e justificar o tratamento conforme o contexto.
2. Examinar granularidade temporal, espacial e demográfica, singularidade/raridade e potencial de cruzamento com informações acessíveis por terceiros.
3. Examinar presença de texto livre, registros não estruturados, nomes próprios e dados sobre situações de vulnerabilidade, **sem reproduzir exemplos reais**.
4. Avaliar risco na forma **efetivamente publicada**, incluindo recortes, agregações, transformações e metadados de arquivos.
5. Separar permissões de uso, de processamento, de publicação de dados e de publicação de **relatório sanitizado**.
6. Documentar medidas, limitações, risco residual, responsável pela decisão e nova revisão necessária, guardando detalhes em registro restrito.

O uso de uma heurística ou ferramenta automática não equivale a certificação de anonimização. Dados já públicos no GitHub ainda exigem análise do histórico e de outras cópias caso se identifique exposição indevida.

## 5. Resultado publicável — preencher somente após a revisão institucional

| Campo | Estado inicial; atualizar exclusivamente com resultado autorizado |
| --- | --- |
| Escopo de inspeção | **NÃO REALIZADO** |
| Procedimentos efetivamente empregados | **NÃO DOCUMENTADOS — inspeção pendente** |
| Resultado sanitizado | **PENDENTE**, sem inferir ausência de dados pessoais |
| Medidas necessárias | **PENDENTES** |
| Risco residual | **NÃO AVALIADO** |
| Restrições de divulgação | **A DETERMINAR** |
| Autoridade que aprovou este resumo público | **PENDENTE** |
| QA independente sobre execução | **PENDENTE** |
| Decisão de publicar/manter/substituir/restringir | **PENDENTE T01.4–T01.6** |

### Critério de conclusão e integração

1. Validar G1–G5 internamente, executar de fato a triagem no ambiente aprovado e obter autorização institucional para divulgar **este resumo**, não evidências restritas.
2. Abrir PR específico com o relatório sanitizado **preenchido com resultados reais**, comando ou procedimento geral, limites e referência de controle; nunca anexar arquivos sensíveis.
3. Revisor independente verifica que os achados são sustentados por evidências permitidas, que não há vazamento no diff/CI e que o escopo está completo ou foi formalmente reclassificado.
4. Marcar T01.3 `Exec. ☒ / QA ☒` somente após parecer independente favorável para a execução e integração conforme plano. A Issue #14 e a Etapa 01 permanecem abertas enquanto T01.4–T01.7 ou outras pendências subsistirem.

**Estado desta entrega:** preparação pública; **T01.3 não executada**; nenhum arquivo de dados aberto, examinado ou alterado pelo executor do GitHub.
