# Relatórios, matrizes e evidências

Esta pasta **não é apenas um placeholder**. Contém relatórios científicos por estudo, matrizes de evidências e registros técnicos de execução, QA e CI. A presença de um relatório ou de um resultado histórico congelado não indica que seu pipeline ou inferência tenham sido reexecutados na revisão atual.

## Guias de leitura do estado comprovado

- [T06.1 — manifesto e integridade documental](t06_1/README.md): inventário N1 dos 33 estudos, sem certificar licenças ou resultados.
- [T06.2 — cinco estados independentes](t06_2/README.md): integridade, síntese, pipeline, inferência e validação científica; o corte original documentou duas auditorias N2.
- [T06.3 — modos operacionais](t06_3/README.md): auditoria rápida, reexecução do modelo e reprodução desde fontes são processos distintos.
- [T06.4 — custódia de resultados](t06_4/README.md): preservar produtos históricos e separar novas saídas.
- [T06.5 — dependências externas](t06_5/README.md): caminhos, licenças e acessos não homologados.
- [T06.6 — piloto Cães F5](t06_6/README.md): 30/30 cotejos N2 sobre quatro CSVs agregados em CI, sem Stan/MCMC, sem validação N3/N4.
- [T06.7 — revisão dos READMEs](t06_7/README.md): escopo editorial e verificações de não promoção de evidências, sem execução inferencial.

**Etapa 07 — migração controlada:** o [registro T07.6](t07_6/README.md) preserva a avaliação histórica; a [revisão posterior T08.7](t08_7/README.md) registra o encerramento de T07.6 no escopo revisado de equivalência operacional contemporânea PNAD N2, não equivalência histórica. O [mapa T07.7](t07_7/README.md) distingue o único adaptador PNAD N2 executável das rotas R/Stan suspensas de Mortalidade SP e Cães F5. O cotejo com logs históricos independentes permanece pendente.

**Etapa 08 — revisão documental:** [T08.3](t08_3/README.md) registra verificações e limites do parser; [T08.7](t08_7/README.md) documenta a revisão editorial final. Os [sumários](../scripts/STUDY_SUMMARIES_T08_4.md) e [versões e evidências](../scripts/STUDY_EVIDENCE_T08_6.md) delimitam as sínteses históricas.

**Etapa 09 — auditoria final:** [T09.1–T09.6](t09/README.md) registram inventário, execução delimitada, amostra, riscos, governança e parecer. T09.7 publicou o [Deck em branch própria](https://github.com/riscotta/bayes-stan-r/commit/24f6a6f4c827de6437799b2eb841afb4ae102261); nova release integral sem decisão.

**Etapa 10 — documentação científica:** [T10.1 — inventário](t10_1/README.md), [T10.2 — reconciliação E00–E10 e reprodutibilidade](t10_2/README.md), [T10.3 — 33 adendos científicos](t10_3/README.md) e [T10.4 — templates e matrizes](t10_4/README.md). Cada registro conserva seu próprio corte, e os adendos não substituem os READMEs históricos.

Os relatórios [Cães F5](caes_raca_f5/relatorio_cientifico.md), [mortalidade SP](mortalidade_sp_0_74/relatorio_cientifico.md) e [PNAD Estudo I](pnadc_estudo_i/nota_cientifica.md) devem ser interpretados com suas ressalvas locais; nenhuma evidência pode ser transferida de um estudo a outro.

**Governança (26/11/2026):** [issue #14 P0](https://github.com/riscotta/bayes-stan-r/issues/14) fechada no [escopo documental da decisão de proveniência](../data/DATA_PROVENANCE_DECISIONS.md), sem homologação geral das licenças, sem resolver automaticamente alertas semânticos ou autorizar release. As issues [#15](https://github.com/riscotta/bayes-stan-r/issues/15) (Stan e diagnóstico científico) e [#16](https://github.com/riscotta/bayes-stan-r/issues/16) (governança) seguem abertas. Código MIT não licencia automaticamente dados de terceiros. Não foram realizados nesta atualização de README novos ajustes, downloads institucionais ou verificações de permissões.

Fontes de novos relatórios podem usar [modelo Quarto — não é resultado de estudo](template/template.qmd); saídas HTML/PDF regeneráveis devem ir a caminhos ignorados pelo Git, sem substituir documentos históricos.
