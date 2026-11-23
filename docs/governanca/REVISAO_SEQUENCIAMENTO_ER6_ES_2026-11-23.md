# Bayes-Stan-R — proposta de revisão controlada do sequenciamento ER6/ES

**Estado deste documento:** PROPOSTA PARA QA INDEPENDENTE; SEM EFICÁCIA OPERATIVA, SEM ACEITE DE T07.6 E SEM LIBERAÇÃO AUTOMÁTICA DAS ETAPAS 08–10.  
**Data:** 23/11/2026 (America/Sao_Paulo). **Objeto de governança:** GSEQ-2026-11-23.  
**Base GitHub verificada:** `main` `097650ac9a4a3c99d4c1d0453b63175938ba15fe`; PR #64 OPEN/DRAFT; PR #65 MERGED; PR #66 CLOSED/NOT MERGED.  
**Fontes de precedência:** Word mestre (Apêndices ER6/ES), Instrução Permanente do projeto, política [T06.4](../../reports/t06_4/README.md), [T07.6](../../reports/t07_6/README.md) e [T07.7](../../reports/t07_7/README.md).

## 1. Fato novo e competência delimitada

Em 23/11/2026, o responsável **declarado** pelo plano autorizou expressamente **iniciar revisão formal do sequenciamento ER6/ES**, para avaliar continuidade de tarefas comprovadamente independentes, exigindo que T07.6 e Etapa 07 continuem abertas e que bloqueios científicos, de custódia, direitos e R/Stan permaneçam vigentes. Determinou, expressamente, não iniciar Etapas 08–10 antes de documentar e validar alteração específica, com QA independente quando aplicável.

A manifestação é uma decisão de encaminhamento **interno do plano**, não instrumento institucional de terceiros, assinatura certificada, dispensa de comparação histórica, autorização para editar dados ou aprovação de uma nova ordem já efetiva. A identidade/competência sobre dados externos e os gates G1/G2 da proposta B+C **não** são presumidos satisfeitos.

**Objeto da revisão:** substituir, *somente após QA independente, autorização e integração controlada*, a regra de precedência absoluta da T07.6 para permitir um conjunto delimitado de tarefas editoriais independentes. A T07.6 não será dispensada, concluída, rebaixada nem relabelada.

## 2. Texto normativo proposto — ainda NÃO vigente

> Enquanto a T07.6 e a Etapa 07 permanecerem abertas, admite-se a abertura sequencial de ciclos **exclusivamente editoriais/documentais** da Etapa 08 que não dependam materialmente de homologação da equivalência histórica das saídas adaptadas, custódia institucional, liberação R/Stan, reamostragem ou nova inferência, **desde que** (i) esta revisão de sequenciamento tenha QA independente favorável sobre SHA exato e esteja formalmente integrada e refletida no Word mestre; (ii) cada tarefa tenha escopo, arquivo, evidência, testes, PR e QA próprios; (iii) documentos exponham expressamente as pendências T07.6, a questão de direitos P0 #14 e as limitações dos estudos; e (iv) nenhuma tarefa altere fontes históricas, dados, códigos científicos, conclusões inferenciais, status N3/N4, decisões de custódia, publicação ou release. A abertura de uma tarefa não implica conclusão da Etapa 08 nem revogação dos gates ER6/ES para outras tarefas ou etapas. As Etapas 09 e 10 exigem deliberação ulterior específica, depois de reavaliadas suas dependências e os bloqueios aplicáveis.

Este texto é proposta de **exceção estreita de ordem**, e não revisão de critérios científicos de T07.6/T06.4.

## 3. Matriz de independência condicionada — Etapa 08

| Tarefa literal (resumo fiel) | Juízo de independência da equivalência histórica T07.6 | Perímetro permitido se a alteração de ordem for homologada | Gatilho adicional de QA/limite |
|---|---|---|---|
| **T08.1** Evidenciar categorias e 3–5 estudos na entrada | **Sim, editorial condicional** | Reorganizar navegação para conteúdo já publicado; 3–5 estudos reais, com limitações e estado verificável | Links/conteúdo conferidos; não reutilizar PR #66 sem nova implementação e QA próprios |
| **T08.2** Catálogo pergunta/método/dados/resultado/estado/modos | **Sim, somente metadata comprovada** | Compilar descrição a partir de README, T06.1–T06.3 e fontes existentes, sem expor datasets/novas saídas | Ausência de evidência = PENDENTE; direitos #14 não homologados |
| **T08.3** Links/comandos/Markdown | **Sim, para testes não invasivos** | Verificar links e renderização; validar sintaxe/rotas sem executar scripts científicos proibidos | Comandos que toquem insumos ou R/Stan ficam apenas em revisão estática |
| **T08.4** Sumário científico e leitura E00–E10 | **Condicional por estudo** | Resumos editoriais fiéis a resultados e estados já documentados, sem recomputação | QA técnico-científico de afirmações substantivas; registrar lacunas em vez de preencher |
| **T08.5** Exemplos interpretados do fenômeno | **Condicional por estudo/exemplo** | Explicar somente evidências aceitas e limitações explícitas | Não deduzir novos estimandos, causalidade, validade preditiva ou equivalência histórica |
| **T08.6** Distinguir versões/execuções/artefatos offline | **Sim, editorial condicional** | Explicitar estados e rastreabilidade, inclusive T07.6 ainda incompleta | Não promover N2 a N3/N4 nem confundir entrada histórica com output adaptado |
| **T08.7** Revisão editorial independente | **Não é execução inicial** | Ocorre após as entregas editoriais relevantes, por revisor distinto | Não se confunde com autoinspeção do executor, revisão Codex ou CI |

**Sequência de menor risco a examinar para liberação futura:** começar **somente por T08.1** após eficácia formal da exceção; T08.2–T08.6 requerem ciclos e checagens por escopo; T08.7 ao fim das alterações relevantes. Não executar nenhuma dessas tarefas neste PR de governança.

## 4. Etapas 09 e 10 — fora da liberação proposta

A Etapa 09 requer auditoria transversal e, em T09.2, jobs científicos pertinentes; T09.7 só admite deliberar release após resolver P0 e obter autorização explícita. A Etapa 10 integra 33 estudos, revisão inferencial transversal e encerramento sem bloqueadores aplicáveis. Alguns trabalhos de inventário podem ser logicamente separáveis, mas **nenhuma tarefa de T09.* ou T10.* é liberada por esta proposta**. Qualquer exceção posterior exigirá nova avaliação documentada, deliberação e QA.

## 5. Gates preservados sem alteração

- **T07.6:** Exec. ☐ / QA ☐; **Etapa 07 aberta**; T07.1–T07.5 e T07.7 concluídas nos termos históricos.
- `PNAD_STDOUT`/`PNAD_STDERR`: `PENDENTE_CUSTODIA`; `PNAD_REPORT`: `NAO_APLICAVEL_HISTORICO_PROPOSTO`, sem homologação; `MORT_SP`/`CAES_F5`: `SUSPENSO_POR_CONTRATO`.
- Sem confundir `IGUALDADE_OPERACIONAL_N2` de execução contemporânea com referência histórica independente. Não mudar limiares ou critérios de T06.4.
- [Issue #14](https://github.com/riscotta/bayes-stan-r/issues/14) P0, [#15](https://github.com/riscotta/bayes-stan-r/issues/15), [#16](https://github.com/riscotta/bayes-stan-r/issues/16) abertas; sem homologação de dados, execução R/Stan/MCMC, N3/N4, release ou publicação nova.
- [PR #64](https://github.com/riscotta/bayes-stan-r/pull/64) B+C continua independente em draft; [PR #66](https://github.com/riscotta/bayes-stan-r/pull/66) permanece fechado sem merge e não é QA de T08.1.

## 6. Gates de eficácia e atribuições

| Gate | Evidência necessária | Responsável/papel | Estado no preparo |
|---|---|---|---|
| **SEQ-1 — proposta** | Esta manifestação, texto específico e matriz de dependências confrontados com Word | Executor deste PR | PREPARADO, NÃO APROVADO |
| **SEQ-2 — QA independente** | Conferir diff integral, base/HEAD, cláusula estrita, matriz, risco #14, T06.4, ER6/ES e CI; parecer explícito sobre mérito da exceção | **Outro chat/revisor**, não executor | PENDENTE |
| **SEQ-3 — decisão formal do plano** | Aprovação do texto final da exceção por autoridade competente do plano; a autorização atual é para **iniciar revisão**, não a sanção de um texto ainda não examinado | Responsável pelo plano | PENDENTE |
| **SEQ-4 — integração** | PR pronto, SHA exato aprovado, CI e ruleset verificados, integração por responsável distinto do executor | Integrador autorizado | PENDENTE |
| **SEQ-5 — Word mestre** | Registrar o texto eficaz e os identificadores reais da decisão/QA/merge, sem apagar ER6/ES e sem alterar T07.6 ou Etapa 07 | Integrador/controle | PENDENTE |
| **SEQ-6 — primeiro trabalho editorial** | Apenas depois de SEQ-2 a SEQ-5 e nova conferência de `main`, iniciar ciclo executor separado T08.1 | Novo chat executor | BLOQUEADO |

**Regra de parada:** não declarar nenhuma tarefa da Etapa 08 iniciada ou concluída, não atualizar a `main` como aprovação de ordem, não mesclar este PR e não reabrir G1/G2 da T07.6 por inércia.

## 7. Evidências e verificação proposta

- Word mestre vigente: `04_WORD_MESTRE_VIGENTE_ES_PR66_FECHADO.docx`, Apêndices ER6 e ES1–ES4, tabelas de T07.6 e Etapa 08.
- `main` inicial: `097650ac9a4a3c99d4c1d0453b63175938ba15fe`. CI pós-merge anterior [#44594052395](https://github.com/riscotta/bayes-stan-r/actions/runs/44594052395) não é QA desta proposta.
- Verificar apenas documentos de governança no diff; scripts, datasets, modelos, artefatos históricos e workflows devem permanecer intocados.
- CI do SHA específico deste PR: registrar se acionada; checks de sintaxe não validam a autoridade do responsável nem a independência científica.
- **Próximo ato autorizado nesta sessão:** preparar a proposta e encaminhá-la ao QA independente. Nenhuma aprovação ou integração é inferida.
