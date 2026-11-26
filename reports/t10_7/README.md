# T10.7 — Auditoria final e condições de encerramento

**Data:** 26/11/2026. Base fixa: [fb1d225af68b2d53351b6936fabe1bcfd0d2b3bc](https://github.com/riscotta/bayes-stan-r/commit/fb1d225af68b2d53351b6936fabe1bcfd0d2b3bc), árvore `ab6aed91a133d880be1a1d9bda6b0bab8aa83a83`. Base de abertura da Etapa 10: [6cb8177b36b8f2fdaa48667fcc2c5c2c01b4078e](https://github.com/riscotta/bayes-stan-r/commit/6cb8177b36b8f2fdaa48667fcc2c5c2c01b4078e). A CI pós-merge da T10.6 [#45874960683](https://github.com/riscotta/bayes-stan-r/actions/runs/45874960683) concluiu com sucesso. Este relatório é execução do auditor/executor; seu próprio QA e CI/integração ainda são gates, não fatos consumados.

## 1. Objetivo e precedência do plano

Tarefa literal do plano mestre: “Executar revisão final do repositório e plano: rastrear issues, PRs, branches, CI e gates P0; consolidar pendências, decisões e recomendações de publicação/release; obter QA independente por entrega e aprovar fechamento somente sem bloqueadores aplicáveis.”

Fonte histórica: pacote de continuidade T08.4 de 26/11/2026, arquivo `01_PLANO/PLANO_BAYES_STAN_R_T07_6_ENCERRAMENTO_E_PENDENCIAS_2026-11-25.docx`, SHA-256 `693b0c9b39140f36b1f948d8e39666eb5a1798dc046dad2bff30b33535e7fbdf`. Cotejo do checklist literal T10.1–T10.7, Apêndice EG, registro posterior de 25/11/2026 de encerramento da T07.6 no escopo revisado, quadro “O que falta para concluir o projeto” e ESTADO_ATUAL.md do pacote. O Word é snapshot anterior às Etapas 09/10 e à decisão #14; não tratar suas caixas como estado atual nem reescrever retrospectivamente. GitHub é referência para o estado posterior. Esta auditoria não edita o Word mestre; fornece registro reconciliado para atualização controlada futura.

## 2. Entregas e gates de QA da Etapa 10

| Tarefa | Entrega | PR | CI HEAD | CI pós-merge | Estado |
| --- | --- | --- | --- | --- | --- |
| [T10.1](../t10_1/README.md) | Inventário das 33 unidades | [#77](https://github.com/riscotta/bayes-stan-r/pull/77) | [45854051508](https://github.com/riscotta/bayes-stan-r/actions/runs/45854051508) | [45854424296](https://github.com/riscotta/bayes-stan-r/actions/runs/45854424296) | Exec./QA encerrados no escopo |
| [T10.2](../t10_2/README.md) | 363 estados e 165 dimensões | [#78](https://github.com/riscotta/bayes-stan-r/pull/78) | [45855225351](https://github.com/riscotta/bayes-stan-r/actions/runs/45855225351) | [45858209999](https://github.com/riscotta/bayes-stan-r/actions/runs/45858209999) | Exec./QA encerrados no escopo |
| [T10.3](../t10_3/README.md) | 33 adendos | [#79](https://github.com/riscotta/bayes-stan-r/pull/79) | [45859319384](https://github.com/riscotta/bayes-stan-r/actions/runs/45859319384) | [45863317532](https://github.com/riscotta/bayes-stan-r/actions/runs/45863317532) | Exec./QA encerrados no escopo |
| [T10.4](../t10_4/README.md) | 6 modelos | [#80](https://github.com/riscotta/bayes-stan-r/pull/80) | [45864776910](https://github.com/riscotta/bayes-stan-r/actions/runs/45864776910) | [45866718675](https://github.com/riscotta/bayes-stan-r/actions/runs/45866718675) | Exec./QA encerrados no escopo |
| [T10.5](../t10_5/README.md) | Navegação; 963 links locais e 34 âncoras no recorte | [#81](https://github.com/riscotta/bayes-stan-r/pull/81) | [45872291663](https://github.com/riscotta/bayes-stan-r/actions/runs/45872291663) | [45873046729](https://github.com/riscotta/bayes-stan-r/actions/runs/45873046729) | Exec./QA encerrados no escopo |
| [T10.6](../t10_6/README.md) | 20 registros, 13 unidades | [#82](https://github.com/riscotta/bayes-stan-r/pull/82) | [45874421309](https://github.com/riscotta/bayes-stan-r/actions/runs/45874421309) | [45874960683](https://github.com/riscotta/bayes-stan-r/actions/runs/45874960683) | Exec./QA encerrados no escopo |
| T10.7 | Auditoria e matriz de pendências deste relatório | A registrar no PR | A confirmar | A confirmar | Execução documental submetida; QA/merge ainda não comprovados |

Em 26/11/2026, reconsultados os seis PRs e seus comentários de revisão, as doze execuções de CI e a main. Cada revisão Codex tem estado completed, sem apontamentos e sem apontamentos não resolvidos, no HEAD final exato. Todas as doze execuções estão completed/success nos respectivos SHAs. Isto é revisão independente automatizada do executor e não homologação científica ou aprovação formal humana por terceiro. A proteção GitHub não exige aprovação humana, mas o fluxo do plano exige QA independente por entrega.

| Tarefa | HEAD revisado | Squash na main |
| --- | --- | --- |
| T10.1 | `074c0734ec6b9f8726700839c6e9702757129001` | `093c2d6d1ade591685e0a28c99814048bae15c58` |
| T10.2 | `83e8a081931348617ce4f1a625c06526145587e2` | `06ef29c7e233396809d13804989efa19f7d49538` |
| T10.3 | `298f5c90d08751f966f2980906dd37bbe0743238` | `9f13421e1799512d9178f6466ae0d0cd4dcaac77` |
| T10.4 | `34287d6b7436237f1daf3ce0a5dfc9fdf734a643` | `263e96f24be31f94aecc9534a8f66f964bc2e125` |
| T10.5 | `64c8004518f0870818098696ebb5ff7b90ee4c86` | `147704cb7ac491b35dfe4e57eb0182c8f2a58b42` |
| T10.6 | `53634c7a0f29f45ef7edd1696a7c4dfba95f01ff` | `fb1d225af68b2d53351b6936fabe1bcfd0d2b3bc` |

## 3. Controles finais de integridade e cobertura

| Controle | Resultado observado no SHA base T10.7 | Limite |
| --- | --- | --- |
| Árvore Git | 681 entradas, 513 blobs, não truncada | Metadados Git, não conteúdo científico |
| Diff da Etapa 10 até T10.6 | 52 caminhos, 42 novos, 10 modificados; +2850/-18; seis commits squash | Exclusivamente documentos, matrizes editoriais e template; não código científico, dados, resultados ou READMEs de estudo |
| [Manifesto T06.1](../t06_1/manifesto_estudos_T06_1.json) | 154/154 entradas elegíveis distintas, caminho/blob/tamanho coincidentes, inclusive 33 READMEs | Não comprova direitos, custódia externa ou validade |
| [Manifesto de dados](../../data/TRACKED_DATA_MANIFEST.csv) | 117/117 caminhos únicos, blob/tamanho coincidentes; nenhum blob extra em data/raw excluídos Markdown e .gitkeep | Sem abertura de registros nem nova inspeção semântica |
| Documentação individual | 33/33 READMEs e 33/33 adendos T10.3 | Não atesta execução inferencial |
| [Estados E00–E10](../t10_2/estados_e00_e10.csv) | 363 registros únicos, todos PENDENTE; 11 do piloto, 352 sem estado formal anterior localizado | Estado de evidência/QA, não ausência de trabalho ou reprovação |
| [Cinco eixos](../t10_2/dimensoes_reprodutibilidade.csv) | 165 pares únicos: 33 N1 documentais, 3 N2 limitados, 129 não reatestados | Não promoção de N3/N4 ou validade |
| [Matriz T10.6](../t10_6/matriz_achados.csv) | 20 registros em 13 unidades, 62/62 referências de blob coincidentes com a árvore | Amostra intencional; 20 unidades fora da revisão direta; não nova execução ou validação de todos os intervalos |
| [Navegação T10.5](../t10_5/README.md) | 963 links locais, 34 âncoras e 48 referências a código conferidos na tarefa | Evidência do recorte anterior, não nova varredura integral; URLs externas e preview não universalmente verificados |

Recontagem feita com parser de CSV que preserva campos entre aspas, vírgulas, aspas duplicadas e quebras de linha; chaves compostas de unidade/etapa e unidade/eixo únicas. Cotejo dos manifestos por metadados Git, sem extrair dados brutos. Os 154 objetos do manifesto T06.1 são distintos; não somar aos 117 como se não houvesse sobreposição. N1 foi rechecado neste perímetro, não autenticação científica. CI padrão Python/Stan sintático/R cobre contratos e três auditorias N2; T09.2 e T05.5 registram execução nativa sintética delimitada de 2/20 programas Stan, não 20/20. Não há novo ajuste científico neste PR.

## 4. Governança e gates P0

| Objeto | Estado consultado | Consequência |
| --- | --- | --- |
| [Issue #14](https://github.com/riscotta/bayes-stan-r/issues/14) / [PR #71](https://github.com/riscotta/bayes-stan-r/pull/71) | Fechada em 26/11/2026; [decisão de permanência](../../data/DATA_PROVENANCE_DECISIONS.md) dos 14 conjuntos/83 arquivos com rastreabilidade parcial | Encerramento documental; não homologação de licença específica, LGPD, inspeção G1–G5, autorização institucional ou release |
| [Issue #15](https://github.com/riscotta/bayes-stan-r/issues/15) | Aberta P2 | Refatoração, testes funcionais e revisão de modelos/diagnósticos continuam |
| [Issue #16](https://github.com/riscotta/bayes-stan-r/issues/16) | Aberta P2 | Homepage e ciclo de releases; proteção da main ativa; homepage não reatestada |
| [PR #64](https://github.com/riscotta/bayes-stan-r/pull/64) | Único PR aberto na consulta anterior a T10.7; draft, HEAD `2851eee85315a5a7982e1fda8861fda2840b4252` | Preservar proposta/histórico; corpo registra decisão T07.6 revisada; não integrar proposta B+C por inércia |
| [Ruleset 26061035](https://github.com/riscotta/bayes-stan-r/rules/26061035) | Ativo, PR e check R 4.5.1 — syntax and tests, bloqueio de exclusão/non-fast-forward, zero aprovações obrigatórias | CI e QA adicional do plano não equivalem a aprovação formal humana; não alterar regras |
| Branches | 78 retornadas pela API antes da T10.7; main protegida no SHA base, seis branches T10.1–T10.6 nos HEADs acima e branch do draft #64 preservada | Não excluir branches; 78 é quantidade retornada no recorte, não obrigação de manutenção |
| [Deck](https://github.com/riscotta/bayes-stan-r/commit/24f6a6f4c827de6437799b2eb841afb4ae102261) | downloads/deck no commit indicado | Publicação específica já decidida; não extrapolar autorização para outros estudos |
| [Release v1.0.0](https://github.com/riscotta/bayes-stan-r/releases/tag/v1.0.0) | Única release retornada na consulta, publicada em 01/10/2026, sem assets anexados | Nenhuma nova release integral; não modificar tag, pacote ou publicação |

## 5. Matriz de pendências e condições para conclusão

A prioridade desta matriz é funcional e local à auditoria, não reclassificação oficial de issues. Os responsáveis indicados são papéis propostos; titularidade institucional não presumida.

| ID | Pendência / evidência | Bloqueia | Diligência e critério objetivo | Papel decisor |
| --- | --- | --- | --- | --- |
| P01 | P0: #14 fechada no escopo de permanência; licenças específicas, extrações e derivados ainda com lacunas; T01.3–T01.7 no Word histórico | Release integral e usos que exigem direitos/privacidade comprovados | Conferir por conjunto a origem, versão, termos, permissões e transformações; reconciliar individualmente T01.3–T01.7 com decisão posterior, documentar o que foi efetivamente aceito e o que não foi executado; não marcar inspeção/QA sem prova | Responsável pelo plano e competência sobre dados, a identificar quando institucional |
| P02 | T09.1: alerta de nomes de colunas Mortalidade SP e combinação de campos RS Seguro; ZIP/XLSX não classificados semanticamente de forma integral | Nova divulgação dos materiais afetados; avaliação de privacidade | Inspeção semântica autorizada em ambiente apropriado, recorte e parecer de risco; não confundir triagem com incidente confirmado ou anonimização | Responsável competente de dados/privacidade |
| P03 | T07.6: PNAD stdout/stderr históricos indisponíveis ao responsável, PENDENTE_CUSTODIA | Equivalência histórica, não equivalência operacional N2 contemporânea já aceita | Localizar pares autênticos, origem, homologia, autorização, catálogo/pin independentes e protocolo T06.4; report.json tem apenas contrato do wrapper no escopo revisado | Custodiante e QA independente |
| P04 | T07.7: adaptadores Mortalidade SP/Cães F5 SUSPENSO_POR_CONTRATO | Execução dessas rotas, não auditorias públicas originais N2 | Reabertura por rota com insumos, autorização e protocolo/critério específico; não executar por efeito deste relatório | Responsável científico e custodiante |
| P05 | T10.2: 363 PENDENTE; E00–E10 sem parecer suficiente por etapa | Homologação integral de estudo | Localizar evidências e decisão independente por unidade/etapa; não preencher retrospectivamente ou promover por categoria | Responsável científico/QA |
| P06 | T10.6: 20 registros, 15 ALTA, 4 MEDIA, 1 INFORMATIVO; quatro achados de código e limite log-t, sem efeito numérico medido | Alegações científicas incompatíveis, não encerramento de diagnóstico | Priorizar A09/A19 estimando de momento log-t, A11 sentinela, A15 holdout, A16 seleção/ancoragem; seguir critérios de encerramento de cada linha e protocolos antes de refit; preservar históricos | Responsável científico e QA independente |
| P07 | #15: teste nativo 2/20 sintéticos, PPC, R-hat, ESS, LOO, SBC e sensibilidade; outros 20 estudos fora da amostra direta T10.6 | Validação científica geral | Plano por modelo e tarefa preditiva; novos dados/ambiente autorizados, diagnósticos e revisão dos 20 não amostrados conforme risco | Responsável científico |
| P08 | T08.3: 11 ocorrências externas inconclusivas; T10.5: 38 ocorrências externas/GitHub fora do parser local, não necessariamente as mesmas | Afirmação de navegação externa integral | Verificar URLs e homepage, preservar estados inconclusivos quando bloqueio/timeout; renderização visual dos gráficos e Quarto ainda não demonstrada neste ciclo | Mantenedor |
| P09 | #16 / T09.7: release integral sem decisão | Nova release integral e comunicação pública que amplie divulgação | Manifesto do pacote exato, checagem de direitos/privacidade e riscos P01/P02, critérios de comunicação e autorização explícita; avaliar exclusão de conteúdo não elegível sem alterar histórico por inércia | Responsável pelo plano |
| P10 | Plano mestre de 25/11 preservado; estados posteriores no GitHub e neste relatório | Declaração de que Word mestre inteiro foi atualizado/encerrado | Atualizar o controle documental em versão sucessora, preservar histórico, vincular SHAs e QA, reconciliar pendências de T01 e T09.7; não marcar caixa final global sem deliberação | Responsável pelo plano/integrador |

## 6. Parecer do executor, condicionado ao QA

- **Etapa 10, escopo documental:** T10.1–T10.6 executadas, revisadas e integradas com CI pós-merge verde. T10.7 entrega o rastreamento e a matriz de condições. Recomendar encerramento da **Etapa 10 documental** somente após QA independente e CI do HEAD exato, integração e confirmação da CI pós-merge, se não houver achado bloqueante do escopo. O fato de estados científicos continuarem PENDENTE não é defeito por si só na entrega de reconciliação.
- **Plano de correção global:** não declarar 100% de todas as tarefas ou gates científicos/institucionais concluídos. Etapas 00/02–06 e 07 encerradas nos escopos históricos; 08 concluída pelo PR #75; T09.1–T09.6 pelo PR #76; T09.7 deliberada parcialmente para Deck, sem nova release integral. O fechamento da #14 não substitui comprovação individual de T01.3–T01.7 e nem deliberação de release. P01–P10 devem acompanhar o controle final, sem declarar ausência de bloqueadores para os objetivos que afetam.
- **Validade científica:** não homologada integralmente; nenhum novo N3/N4 ou estado formal E00–E10 aprovado nesta etapa. Corrigir leitura e planejar novos protocolos sem retroagir conclusões.
- **Publicação:** manter v1.0.0 e publicação específica do Deck; recomendar NÃO PUBLICAR nova release integral agora. Primeiro delimitar pacote, direitos, privacidade, nível de evidência e autorização. Não acionar release, tag, exclusão de branch, merge do draft #64, alterações de issue, dados ou ajustes neste PR.
- **Aceite de T10.7:** CI do HEAD final e revisão independente no mesmo SHA, sem bloqueadores aplicáveis ao escopo documental. O registro posterior à integração deve informar o SHA efetivo da main, execução pós-merge e parecer. A auditoria não se autoaprova e não antecipa esses identificadores.

