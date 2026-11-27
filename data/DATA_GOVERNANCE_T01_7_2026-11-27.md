# T01.7 — Consolidação dos controles e pendências reais (27/11/2026)

**Tarefa literal:** “Atualizar documentação, testes/manifestos e a Issue #14 com pendências reais, sem encerramento prematuro.”

**Escopo revisado autorizado:** concluir administrativamente a Etapa 01 no perímetro documental, com ressalvas e transferência controlada das diligências. Não homologar inspeção, autorização institucional, licença ou redistribuição. Não declarar todas as tarefas originais executadas. P01/P02 permanecem P0 para os objetivos afetados.

## 1. Base e evidência de integração

Base [main 928d81281b439b12d1cfe231aaf5d48aa9420718](https://github.com/riscotta/bayes-stan-r/commit/928d81281b439b12d1cfe231aaf5d48aa9420718), árvore `ae480a52df03b502af54676f8b6cb684dec9840c`, [CI push #45992966772](https://github.com/riscotta/bayes-stan-r/actions/runs/45992966772) completed/success no SHA exato. Diff desde a base pós-#85 `de3658d7601a13bfcc793f631059728edd90974c`: somente dois documentos adicionados, +116/-0. Ruleset 26061035 ativo, main protegida, #14 fechada, #64 draft único PR aberto antes deste ciclo; Deck `24f6a6f4c827de6437799b2eb841afb4ae102261`, release v1.0.0.

| Registro | Entrega e estado | Evidência |
| --- | --- | --- |
| T01.1–T01.2 | Encerradas nos escopos históricos, conforme pacote de continuidade | Inventário, manifesto, controles de risco; sem reexecução |
| T01.3 | Reconciliação documental integrada; inspeção original não executada/homologada | [#84](https://github.com/riscotta/bayes-stan-r/pull/84), [reconciliação](DATA_PRIVACY_T01_3_RECONCILIATION_2026-11-26.md) |
| T01.4 | Reconciliação documental integrada; evidências de autorização ainda insuficientes, alternativas condicionais | [#85](https://github.com/riscotta/bayes-stan-r/pull/85), [reconciliação](DATA_AUTHORIZATION_T01_4_RECONCILIATION_2026-11-26.md) |
| T01.5 | Decisões por ID e implicações no histórico; execução/QA no escopo revisado concluídos | [#86](https://github.com/riscotta/bayes-stan-r/pull/86), HEAD `f745c41bce71ddf89ffa62afc319f0b5da6668ae`, merge `16e1693cdef125ce2a4f00b3c65da83fb20de2e4`; CI [45990540366](https://github.com/riscotta/bayes-stan-r/actions/runs/45990540366) e [45991843534](https://github.com/riscotta/bayes-stan-r/actions/runs/45991843534) success; [decisões](DATA_DISPOSITION_T01_5_2026-11-27.md) |
| T01.6 | Preservação/controle e plano condicional R1–R7; execução/QA no escopo revisado concluídos | [#87](https://github.com/riscotta/bayes-stan-r/pull/87), HEAD `c340ff11b10fd64f8d7c75dfa6ff037461b543ad`, merge na base; CI [45992543668](https://github.com/riscotta/bayes-stan-r/actions/runs/45992543668) e [45992966772](https://github.com/riscotta/bayes-stan-r/actions/runs/45992966772) success; [ações](DATA_ACTIONS_RESPONSE_T01_6_2026-11-27.md) |
| T01.7 | Este registro; candidato à integração, não autoaprovado | QA e CI do HEAD, squash e CI push a confirmar, atualização sanitizada da #14 a verificar |

Codex independente completed sem achados ou apontamentos não resolvidos nos HEADs exatos das T01.5 e T01.6, conforme verificações anteriores; não equivale à validação por autoridade de dados. O Word mestre e os adendos históricos são preservados. A atualização final em versão sucessora pertence ao encerramento global.

## 2. Controles de dados, testes e manifesto

| Controle | Evidência na base | Resultado |
| --- | --- | --- |
| Inventário estruturado | [DATA_PROVENANCE.csv](DATA_PROVENANCE.csv), blob `97cfe122f903b8c1b52c57ed0a6c287695f33ba3` | 25 IDs distintos; estados preservados |
| Manifesto | [TRACKED_DATA_MANIFEST.csv](TRACKED_DATA_MANIFEST.csv), blob `ed99382fbe6764a00567d7ec534268398a580145` | 117 caminhos distintos; 117/117 coincidências de caminho, blob e tamanho |
| Cobertura da árvore | Árvore recursiva não truncada na base fixa, sem leitura de registros | 0 blobs adicionais em data/raw fora do manifesto excluídos Markdown e .gitkeep |
| Classes | Cruzamento inventário/manifesto por dataset_id | review_required 14 IDs/83 arquivos; aggregates_only 4/34; not_versioned 7/0 |
| Testes de regressão existentes | [manifesto](../tests/testthat/test-data-file-manifest.R), blob `69cbc312cf784ea70ee80e3dedb0acc460c4bde0`; [proveniência](../tests/testthat/test-data-provenance.R), blob `f6f5b71ddf6d5025aa1b07d399b8421b2a8f7030` | Preservados; contratos conferidos estaticamente; CI da base success, CI deste candidato requerida |
| Revisão de caminhos | [CODEOWNERS](../.github/CODEOWNERS), blob `df04c364b2f60410c425de153cb8039801b3da69`; ruleset 26061035 | Revisores indicados; PR/check R obrigatório, não presumir code-owner review obrigatório |

Método de verificação executado: consultar metadados da árvore via GitHub no SHA fixo, interpretar os dois CSVs de governança com parser que respeita campos entre aspas e cruzar IDs, caminhos, SHA e tamanho. Não clonar, abrir registros brutos, executar nova inspeção semântica ou alegar execução local de R. Testes preexistentes mantidos e CI do candidato exigida. Como nenhum dado, código ou resultado mudou, não regenerar manifesto, alterar estados de licença ou fabricar testes sem necessidade. O [documento canônico de proveniência](DATA_PROVENANCE.md) continua referência de regras gerais; este adendo atualiza o controle de governança sem reescrever seu histórico.

## 3. Reconciliação da issue #14

A [issue #14](https://github.com/riscotta/bayes-stan-r/issues/14) está fechada desde a decisão de 26/11/2026. Seu título histórico e checklist inicial de 28/10 mencionam homologação e 24 IDs/116 arquivos; são registros da rodada original, não prova de homologação nem contagem atual. O [PR #71](https://github.com/riscotta/bayes-stan-r/pull/71) e a [decisão](DATA_PROVENANCE_DECISIONS.md) definem a permanência das 14 cópias históricas com rastreabilidade parcial. A declaração do responsável de origem pública, preparação própria e ausência de restrições é declaração, não autenticação da licença exata, da autorização institucional ou de privacidade. A frase histórica “a pendência passa a ser documental” não extingue as diligências materiais abaixo. O escopo de fechamento da issue é a decisão de permanência; não reabrir automaticamente e não apagar comentários/checklists antigos.

Após QA, merge e CI push desta entrega, registrar comentário sanitizado na #14 com link fixo para este documento, SHAs/CI reais, contagem atual, limites e P01/P02; reconsultar a issue para confirmar comentário e estado closed. Não alterar o corpo histórico nem o título, nem publicar dados ou documentos protegidos. A execução dessa atualização externa é requisito para fechar T01.7, não fato antecipado pelo candidato.

## 4. Diligências materiais que permanecem vigentes

| ID | Estado e limite | Responsável funcional (a validar) | Evidência de reabertura |
| --- | --- | --- | --- |
| P01 | P0: lacunas de origem, versão, termos, transformações e autorizações da cópia exata. T01.3/T01.4 originais Exec. ☐ / QA ☐; revisão administrativa não preenche essas caixas | Responsável do plano e autoridade/licenciante/custodiante competente | Evidências específicas por ID, escopo de uso/redistribuição e critérios de T01.4/T01.5; decisão material e QA próprios |
| P02 | P0: avaliação protegida não homologada; cinco IDs prioritários (20 arquivos) e alerta distinto de mortalidade_sp_0_74; nomes de campos/combinações RS Seguro e exame superficial ZIP/XLSX não constituem achado confirmado | Autoridade de dados/privacidade, operador habilitado em ambiente autorizado | G1–G5 cumulativos, inspeção delimitada e parecer sanitizado autorizado, QA competente; plano R1–R7 se acionado |

Não há registro novo de incidente confirmado. Não divulgar registros, ampliar publicação, executar substituições, reescrever histórico ou remover dados sem autorização específica. P01/P02 bloqueiam nova divulgação afetada e homologação material, não desaparecem nem são reclassificadas como backlog científico comum. Para os demais estados e riscos, ver [T10.7](../reports/t10_7/README.md): P03/P04 seguem custódia/contratos; P05–P07 manutenção científica; P08 editorial; P09 decisão de release separada; P10 atualização mestre sucessora. Sem nova release, tag, mudança do draft #64, issues #15/#16, dados ou resultados neste PR.

## 5. Aceite do escopo revisado

Formalização proposta da estratégia já autorizada: encerrar administrativamente a Etapa 01 **somente após** concluir o ciclo T01.7 (QA independente no HEAD exato, CI success, squash com SHA esperado, CI push success e atualização/verificação da #14). As tarefas T01.3 e T01.4 originais ficam suspensas no escopo material por insuficiência de evidências, com reconciliações documentais integradas; não marcar inspeção ou autorização originais como executadas/QA. T01.5–T01.7 no escopo revisado têm entregas e QA próprios. Preservar o histórico do plano original e seus critérios para reabertura. Não inferir homologação científica/institucional, conclusão global ou autorização de publicação.

Após o aceite efetivo, T09.7 exige deliberação separada; encerramento global só em ciclo próprio, com controle mestre sucessor, critérios e pendências P01–P10. Este documento não antecipa nenhum desses aceites.
