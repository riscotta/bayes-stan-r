# T01.6 — Implementação das ações aprovadas e plano de resposta (27/11/2026)

**Tarefa literal:** “Implementar somente ações aprovadas, com plano de resposta se houver dados sensíveis ou licença incompatível.”

**Perímetro:** executar a decisão de manutenção histórica e controles documentais no escopo revisado autorizado pelo responsável. Nenhuma alteração material de dados, licença, histórico, visibilidade do repositório, modelo, resultado, publicação ou rota foi autorizada neste ciclo. Não presumir incidente, licença incompatível comprovada, privacidade homologada ou validação institucional. P01/P02 permanecem P0 para os objetivos afetados.

## 1. Base e precedência

Base [main 16e1693cdef125ce2a4f00b3c65da83fb20de2e4](https://github.com/riscotta/bayes-stan-r/commit/16e1693cdef125ce2a4f00b3c65da83fb20de2e4), árvore `5008c8c6f20ba5f8c622d30a6d5bd722f9f79a7b`. [PR #86](https://github.com/riscotta/bayes-stan-r/pull/86) integrado por squash; HEAD `f745c41bce71ddf89ffa62afc319f0b5da6668ae`, Codex completed sem achados ou apontamentos não resolvidos no HEAD exato, [CI #45990540366](https://github.com/riscotta/bayes-stan-r/actions/runs/45990540366) completed/success no HEAD, [CI push #45991843534](https://github.com/riscotta/bayes-stan-r/actions/runs/45991843534) completed/success no SHA integrado. Diff da base pós-#85 para a base atual: somente adição do documento T01.5, +66/-0; ruleset 26061035 ativo, main protegida, #14 fechada. Nenhuma nova autorização material decorre de CI ou QA documental.

Fontes: [decisão #14](DATA_PROVENANCE_DECISIONS.md), [T01.3](DATA_PRIVACY_T01_3_RECONCILIATION_2026-11-26.md), [T01.4](DATA_AUTHORIZATION_T01_4_RECONCILIATION_2026-11-26.md), [T01.5](DATA_DISPOSITION_T01_5_2026-11-27.md), blob `33fe5a08bb9eae6cb4c9ce66fd6fc5f7cc7126c6`, [protocolo G1–G5](DATA_PRIVACY_GATE_T01_3_2026-11-03.md), [template sanitizado](DATA_PRIVACY_T01_3_PUBLIC_REPORT_TEMPLATE.md), [T09.1](../reports/t09/README.md), [T10.7](../reports/t10_7/README.md) e adendo de continuidade de 27/11/2026. A decisão de finalizar o plano revisado com ressalvas não revoga exigências de direitos e privacidade.

## 2. Registro de implementação

| Ação | Alcance aprovado | Execução e controle | Limite |
| --- | --- | --- | --- |
| Preservar histórico (#14, classe H) | 14 IDs/83 arquivos | Nenhuma modificação, exclusão, substituição ou reescrita; conferência dos blobs na base | Permanência documental, não autorização de nova redistribuição |
| Manter agregados (classe A) | 4 IDs/34 arquivos | Perímetro atual preservado; nenhuma ampliação | Não comprova anonimização, direitos ou equivalência científica |
| Manter aquisição externa (classe E) | 7 IDs/0 arquivos catalogados | Nenhuma incorporação | Não inferir ausência em todo o histórico Git |
| Restringir nova divulgação afetada | P01/P02 e materiais dependentes | Regra de governança registrada; não criar release, exportação, novo pacote, divulgação ou compartilhamento dos materiais afetados sem decisão competente | Não é revogação técnica de acessos anteriores nem alteração de visibilidade |
| Preservar condições de reabertura | G1–G5, direitos, contratos T07.7, custódia e QA | Nenhuma inspeção, execução de rota suspensa ou substituição; controle prospectivo neste documento | Operação material exige evidências e autorização específica |
| Estabelecer plano de resposta | Indício verificável de exposição de dados sensíveis ou licença incompatível | Procedimento da seção 3, sujeito a execução por autoridade competente se acionado | Não é registro de ocorrência ou de contenção material realizada |

A única alteração candidata é este documento, reversível por PR controlado. Não se acionou resposta a incidente: o executor não recebeu achado verificável de ocorrência. Os alertas de T09.1 (campo em agregado Mortalidade SP e combinação no RS Seguro), a inspeção superficial de ZIP/XLSX e as lacunas de termos não são provas de incidente nem prova de ausência de risco. Mantêm-se as diligências por ID de T01.5, inclusive distinção entre `mortality` e `mortalidade_sp_0_74`.

## 3. Plano condicional de resposta

Aplica-se a novo indício verificável de exposição indevida, dado sensível ou identificável fora do perímetro autorizado, violação de escopo/termos ou incompatibilidade de licença; também a comunicação fundamentada de titular, custodiante ou licenciante. Não abrir dados para confirmar hipótese neste ambiente. Tratar suspeita como suspeita, não como conclusão.

| Passo | Operação e critério | Responsável funcional (a identificar/validar) | Evidência permitida |
| --- | --- | --- | --- |
| R1 — triagem e escalonamento | Interromper a nova atividade afetada sob controle do operador; registrar existência do indício, data, perímetro e encaminhar sem demora à competência institucional/contratual pertinente; não divulgar registros | Operador habilitado e responsável pelo plano | Referência opaca autorizada e estado da diligência |
| R2 — preservar evidência | Em ambiente autorizado, proteger snapshot, versão, hashes, cadeia de custódia, logs e registro de acesso; limitar acesso e evitar cópia pública; preservar evidência antes de qualquer mudança destrutiva | Custodiante e operador autorizado | Confirmação sanitizada de custódia, sem conteúdo bruto |
| R3 — avaliar | Validar G1–G5 antes de inspeção; caracterizar escopo, natureza, risco, fonte, licença da cópia exata, permissões, exposição potencial e terceiros; distinguir fato, incerteza e hipótese | Autoridade de dados/privacidade e titular/licenciante conforme caso | Parecer sanitizado autorizado, sem linhas, células, documentos institucionais ou logs |
| R4 — deliberar contenção | Autoridade competente escolhe ação proporcional e prazo conforme obrigações aplicáveis: cessar nova distribuição, restringir acesso, substituir ou retirar; registrar autorização específica e comunicação necessária pelos responsáveis; não presumir obrigação legal ou prazo sem avaliação competente | Autoridade competente e responsável pelo plano; mantenedor apenas no escopo aprovado | Decisão e critérios sanitizados |
| R5 — plano técnico e execução | Inventariar refs, blobs, branches, tags, releases, forks, clones, caches, CI, backups e dependências; analisar impacto, reversibilidade, preservação protegida, riscos e coordenação com terceiros. Só executar mudança aprovada com procedimento controlado; não contornar ruleset nem fazer force push unilateral | Mantenedor autorizado, custodiante e responsável científico quando houver pipeline afetado | Diff, versão, testes e registro de ação sanitizados |
| R6 — verificação e recuperação | Conferir alcance da medida, acesso residual, manifesto e dependências; se substituir insumo, validar contrato, estimando, regressão e QA independente. Não prometer remoção de cópias de terceiros ou equiparar sintético a resultado empírico | Mantenedor, autoridade competente, responsável científico e QA conforme escopo | Controles verificáveis sem ampliação de exposição |
| R7 — encerramento específico | Parecer autorizado sobre medidas, risco residual, comunicação, reabertura e pendências; QA pertinente; registrar só o permitido. Reavaliar nova divulgação/release separadamente | Autoridade competente; QA independente e responsável pelo plano | Registro de decisão e parecer sanitizado |

Se não houver autorização/ambiente/evidência suficiente, manter o bloqueio de atividade afetada e escalar; não executar inspeção ou retirada unilateral. Notificação, prazo e eventual medida emergencial dependem das obrigações e da autoridade aplicáveis ao caso, não de conclusão jurídica deste documento. Se for necessário preservar informação restrita, fazê-lo fora de GitHub/chat e sem cópia pública. Remover um caminho em commit novo não apaga histórico, forks, caches ou backups. Reescrita exige decisão e plano próprios, não autorizados aqui.

## 4. Verificação da implementação e pendências

Consulta de metadados Git via conector: [manifesto](TRACKED_DATA_MANIFEST.csv) blob `ed99382fbe6764a00567d7ec534268398a580145`, [inventário](DATA_PROVENANCE.csv) blob `97cfe122f903b8c1b52c57ed0a6c287695f33ba3`, iguais à base anterior. Árvore recursiva não truncada; 117 caminhos distintos, 117/117 coincidências de caminho, blob SHA e tamanho, zero blobs adicionais em data/raw fora do manifesto excluídos Markdown e .gitkeep. Nenhum registro de dados aberto; sem novos testes inferenciais. Inventário e manifesto não precisam ser regenerados por esta ação documental; não simular alteração de dados.

P01: comprovação de direitos, proveniência e escopo da cópia exata continua pendente quando aplicável. P02: avaliação protegida, G1–G5 e parecer sanitizado continuam pendentes. Não alterar campos de licença nem declarar T01.3/T01.4 originais executadas. #14 preservada fechada no escopo da decisão de permanência; atualização de governança, controles e pendências da issue cabe ao ciclo T01.7. Nenhuma ampliação de divulgação, publicação integral ou homologação científica.

## 5. Critério de aceite T01.6

Escopo revisado: ações de preservação e controle verificadas e plano condicional registrado. Nenhuma ação material adicional aprovada ou executada. Entrega documental somente; QA independente no HEAD exato, CI favorável, squash com SHA esperado e CI push no SHA integrado antes de iniciar T01.7. O documento não se autoaprova nem constitui parecer jurídico/institucional, comprovação de incidente ou de sua inexistência. Se a inspeção futura ou uma decisão competente exigir medida material, abrir ciclo próprio com autorização, evidência, teste e QA.
