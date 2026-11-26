# T01.3 — Reconciliação documental pós-Etapa 10 (26/11/2026)

**Escopo:** reconciliação de evidências de governança, não execução da inspeção de privacidade. T01.3 permanece Exec. ☐ / QA ☐; Etapa 01 aberta. O QA e a CI deste adendo são próprios da entrega documental e não homologam uma inspeção.

Base revalidada: [main 9fcdab26d09ad2fa2a83388dce1fdf65dad9846b](https://github.com/riscotta/bayes-stan-r/commit/9fcdab26d09ad2fa2a83388dce1fdf65dad9846b), árvore `671aa4237d8db08fdc738433f4c7035261d2fdd7`; [CI push 45875844119](https://github.com/riscotta/bayes-stan-r/actions/runs/45875844119) completed/success nesse SHA. Main protegida, [ruleset 26061035](https://github.com/riscotta/bayes-stan-r/rules/26061035) ativo, PR e check R 4.5.1 — syntax and tests; QA independente adicional exigido pelo plano. PR #64 draft, issues #15–16 abertas, Deck em `24f6a6f4c827de6437799b2eb841afb4ae102261`, latest release v1.0.0. Nenhuma nova publicação de release autorizada.

## 1. Precedência e fontes

Tarefa literal do Word mestre: “Fazer inspeção de privacidade em ambiente autorizado, sem expor registros em chats, issues ou logs públicos.” O pacote de continuidade pós-Etapa 10 e seu adendo determinam começar pela reconciliação, sem repetir a preparação já integrada ou promover o estado da inspeção.

| Código | Evidência consultada | Data e alcance |
| --- | --- | --- |
| S1 | [T01.2](DATA_RISK_PRIORITIZATION_T01_2_2026-11-03.md), [protocolo T01.3](DATA_PRIVACY_GATE_T01_3_2026-11-03.md), [template vazio](DATA_PRIVACY_T01_3_PUBLIC_REPORT_TEMPLATE.md), [PR #22](https://github.com/riscotta/bayes-stan-r/pull/22) | 03/11/2026, priorização, declaração de autorização/ambiente, cinco gates; não houve exame de registros nem verificação interna pelo executor. |
| S2 | [Issue #14](https://github.com/riscotta/bayes-stan-r/issues/14), [PR #71](https://github.com/riscotta/bayes-stan-r/pull/71), [decisões](DATA_PROVENANCE_DECISIONS.md) | 26/11/2026, origem pública, preparação própria e ausência de restrições segundo declaração do responsável; permanência de 14 conjuntos/83 arquivos com rastreabilidade parcial. |
| S3 | [Proveniência](DATA_PROVENANCE.csv) e [manifesto Git](TRACKED_DATA_MANIFEST.csv) | Base fixa acima; metadados documentais, não conteúdo ou cadeia de custódia institucional. Manifesto: 117 linhas, 18 IDs com arquivos; os cinco priorizados mantêm review_required, not_identified, prioridade high. |
| S4 | [T09.1](../reports/t09/README.md) e [T10.7](../reports/t10_7/README.md) | 26/11/2026, triagem superficial de 103 cabeçalhos CSV e controles superficiais ZIP/XLSX; pendências P01/P02, alertas semânticos, sem inspeção protegida homologada. |

O fechamento da #14 é documental e de decisão do projeto; as cláusulas de manutenção da issue aberta em S1 são históricas. Não reabrir automaticamente a issue. S2 amplia a informação declarada, mas não autentica autorização institucional, não comprova termos específicos de cada cópia e não revoga G1–G5. O protocolo de S1 continua referência para pré-acesso. O template permanece vazio.

## 2. Matriz por ID e gate

Legenda: D = declaração existente, não validada internamente; P = evidência específica pendente, sem comprovação; M = metadado documental existente, não suficiente para o gate. Nenhum desses estados significa VALIDADO INTERNAMENTE. G1 = autoridade; G2 = legitimidade e escopo; G3 = ambiente; G4 = custódia; G5 = protocolo/parecer sanitizado. As fontes têm datas e escopos na seção 1. Todos os gates são cumulativos e devem ser verificados internamente antes de abrir o conteúdo; a aprovação do relatório efetivo ocorre depois do exame.

| ID (arquivos) | G1 | G2 | G3 | G4 | G5 | Exame protegido/QA |
| --- | --- | --- | --- | --- | --- | --- |
| isus_sia (1) | D S1/S2 | D S1/S2 | D S1 | M S3/S4; P cadeia | P S1 | Não comprovado / pendente |
| rs_seguro_macrocrime (1) | D S1/S2 | D S1/S2 | D S1 | M S3/S4; P cadeia | P S1 | Não comprovado / pendente |
| rs_seguro_macrocrime_profile (1) | D S1/S2; P escopo funcional | D S1/S2; P versão e abrangência específica | D S1; P alcance desta cópia | M S3/S4; P cadeia | P S1 | Não comprovado / pendente |
| mortality (1) | P S1; D preparação própria S2 | P autorização específica; D origem pública S2 | P S1 | M S3/S4; P cadeia | P S1 | Não comprovado / pendente |
| missing_migrants (16) | P S1; D preparação própria S2 | P termos/cópia; D origem pública S2 | P S1 | M S3; P cadeia | P S1 | Não comprovado / pendente |

Os cinco IDs totalizam 20 arquivos associados, não 20 bases. G1 não é provado pela identidade do preparador; G2 não é provado pela mera existência de uma fonte pública; G3 não é provado pelo acesso ao GitHub; G4 não é provado por hash de blob isolado; G5 não é provado pelo template.

## 3. Diligência por ID, sem acesso a registros

| ID | Evidência existente e lacuna específica (S1–S4) | Diligência do operador autorizado |
| --- | --- | --- |
| isus_sia | Referência DATASUS SIA/SUS, série 2025 descrita; falta consulta, recorte, montagem e classificação integral do ZIP. | G1–G2: confirmar emissor/custodiante, cópia e finalidade; G3: validar controles; G4: vincular snapshot e transformações; G5: aprovar protocolo; só então examinar conteúdo no ambiente protegido. |
| rs_seguro_macrocrime | Referência pública SSP/RS; faltam publicação exata, período, filtros, agrupamento. | G1–G2: autoridade e autorização da cópia; G3: ambiente; G4: fonte e transformação; G5: protocolo aprovado; examinar granularidade e cruzamentos localmente. |
| rs_seguro_macrocrime_profile | Referência SSP/RS e unidade de análise documentada; faltam publicação, período e transformação; alerta de combinação na frente RS Seguro não é achado de identificação. | G1–G2: confirmar alcance específico do extrato; G3–G4: controles e versão; G5: protocolo; examinar raridade/cruzamento sem exportar observações. |
| mortality | Uso municipal de contagens/população documentado; origem das variáveis, janela e junção faltam; XLSX apenas superficialmente verificado. | G1–G2: identificar produtor, competência, termos e versão; G3–G4: ambiente, snapshot e linhagem; G5: protocolo; classificar conteúdo apenas com acesso habilitado. |
| missing_migrants | Referência IOM, uso rota-mês; faltam versão, termos primários e linhagem dos 16 artefatos. | G1–G2: confirmar autoridade, termos e escopo; G3–G4: ambiente, versão e linhagem; G5: protocolo; avaliar identificabilidade no contexto autorizado. |

**Alerta posterior distinto:** `mortalidade_sp_0_74` possui 13 arquivos de resultados agregados no manifesto, estado aggregates_only e prioridade high. S4 registra alerta por nome de campo; não é o ID mortality nem se presume incidente, microdados ou anonimização. Encaminhar avaliação protegida delimitada (escopo/autoridade G1–G5 e permissão) como pendência P02; não ampliar automaticamente o escopo histórico dos cinco IDs.

## 4. Regra de parada e interface com próximas tarefas

Não foi apresentada evidência sanitizada da validação interna cumulativa dos cinco gates por ID. Neste ciclo, nenhum registro/arquivo de dados foi aberto, nenhuma inspeção local foi executada, nenhuma autorização externa foi autenticada. Não é conclusão de inexistência dos documentos nem de irregularidade.

O operador habilitado deve validar internamente G1–G5, inspecionar somente no ambiente aprovado e, depois de autorização de divulgação, fornecer relatório sanitizado por ID conforme o template. Registrar somente estados, data, papel funcional e referência opaca permitida, escopo efetivamente examinado, resultado geral, medidas e risco residual. Não transferir documentos, linhas, células, cabeçalhos identificáveis ou logs ao GitHub/chat. Havendo indício verificável de exposição indevida, encaminhar à autoridade competente, preservar evidência e não executar mudanças destrutivas unilateralmente.

A decisão de permanência de S2 permanece vigente. É insumo para futura reconciliação de T01.4–T01.7, não homologação dessas tarefas: T01.4 deve cotejar direitos/alternativas por cópia; T01.5 deve aproveitar as 14 decisões e delimitar lacunas de privacidade/histórico; T01.6 depende de ações aprovadas; T01.7 deve distinguir registros atualizados de pendências materiais. Não executar essas tarefas neste PR. Se o responsável desejar alterar formalmente o objetivo da inspeção, isso requer decisão específica de escopo, QA e registro, não inferida desta reconciliação.

## 5. QA e integração deste adendo

Escopo do diff: somente este documento; preservar protocolos históricos, manifesto, inventário, dados, scripts, resultados, issues e branches existentes. Revisão independente automatizada no HEAD exato e CI favorável antes de squash com SHA esperado; confirmar main e CI pós-merge. Não marcar Exec./QA da inspeção com base em QA documental. Nenhum novo teste inferencial é pertinente ao adendo. Etapa 10 continua formalmente encerrada em seu escopo; Etapa 01 e autorização de release integral continuam pendentes.
