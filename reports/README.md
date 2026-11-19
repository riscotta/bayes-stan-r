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

**Etapa 07 — migração controlada:** [T07.7 — mapa das rotas adaptadas e dos caminhos que continuam históricos](t07_7/README.md). A documentação distingue o único adaptador PNAD N2 executável das rotas R/Stan suspensas; a T07.6 e a Etapa 07 não são declaradas concluídas.

Os relatórios [Cães F5](caes_raca_f5/relatorio_cientifico.md), [mortalidade SP](mortalidade_sp_0_74/relatorio_cientifico.md) e [PNAD Estudo I](pnadc_estudo_i/nota_cientifica.md) devem ser interpretados com suas ressalvas locais; nenhuma evidência pode ser transferida de um estudo a outro.

**Governança:** [issue #14 P0](https://github.com/riscotta/bayes-stan-r/issues/14) (proveniência, privacidade e redistribuição), [#15](https://github.com/riscotta/bayes-stan-r/issues/15) (Stan e diagnóstico científico) e [#16](https://github.com/riscotta/bayes-stan-r/issues/16) seguem abertas. Código MIT não licencia automaticamente dados de terceiros. Não foram realizados nesta atualização de README novos ajustes, downloads institucionais ou verificações de permissões.

Fontes de novos relatórios podem usar [reports/template](template/); saídas HTML/PDF regeneráveis devem ir a caminhos ignorados pelo Git, sem substituir documentos históricos.
