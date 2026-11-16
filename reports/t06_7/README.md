# T06.7 — atualização dos READMEs sem promover evidência

**Tarefa literal:** “Atualizar os READMEs sem declarar como executado aquilo que não foi rodado nesta revisão.”

**Corte documental:** 16/11/2026. **Base da branch:** `3e9ee72500779da978363766a2657b4455545284` (squash T06.6). **Papel:** CHAT EXECUTOR T06.7 — não emite QA independente.

## Escopo e fontes

Foram inspecionados os **33 READMEs dos estudos** (identidades do [manifesto T06.1](../t06_1/manifesto_estudos_T06_1.json)), o README principal, catálogo de scripts, índice de relatórios, README do estudo Cães F5, [matriz dos cinco estados T06.2](../t06_2/README.md), [modos T06.3](../t06_3/README.md), [dependências T06.5](../t06_5/README.md) e [piloto T06.6](../t06_6/README.md).

**Mudança intencional restrita:** README raiz; `scripts/README.md`; `reports/README.md`; `reports/t06_6/README.md`; este relatório; teste Python de contratos documentais; etapa nova no workflow para executá-lo. Os 33 READMEs locais (inclusive Cães F5) não são alterados: o manifesto T06.1 fixa os SHA históricos; um anexo posterior no README raiz documenta o piloto novo sem reescrever o snapshot. Não há alteração de modelos, dados, outputs históricos, scripts científicos, priors, likelihood, estimandos, nem downloads externos.

## Matriz de afirmações verificáveis

| Eixo | Fato documentado | Limite explícito |
| --- | --- | --- |
| Integridade | 33/33 READMEs e inventário Git T06.1 documentados | N1 documental não certifica procedência nem direitos |
| Síntese | PNAD E09 e mortalidade SP: auditorias N2 delimitadas; Cães F5: piloto N2 posterior com 30/30 cotejos | T06.2 é snapshot anterior e **não foi reescrita**; 30 outros estudos não ganham N2 |
| Pipeline | Comandos e dependências existentes T06.3/T06.5 | **Não aferido** end-to-end desde fontes primárias para o conjunto |
| Inferência | Sintaxe e contratos Stan, sem sampling na T06.6 | **Nenhuma reexecução N3** comprovada nesta tarefa |
| Validação científica | Relatórios históricos contêm diagnósticos e ressalvas | **Nenhuma certificação N4 ou validade externa** derivada destes READMEs |

**Evidências prévias, não executadas pela T06.7:** [PR #56](https://github.com/riscotta/bayes-stan-r/pull/56), [review técnico #5921256040](https://github.com/riscotta/bayes-stan-r/pull/56#pullrequestreview-5921256040) (`COMMENTED`, não approval humano), [CI PR #43857159571](https://github.com/riscotta/bayes-stan-r/actions/runs/43857159571) e [push/main #43881903884](https://github.com/riscotta/bayes-stan-r/actions/runs/43881903884). O piloto foi R `--run_stan=0` sobre quatro CSVs públicos agregados com tolerância absoluta `5e-13`, relativa zero.

## Testes de documentação e gates

O teste `tests/python/test_readmes_t06_7.py` confere cobertura de 33 IDs com caminhos existentes, ligações relativas de documentos editados, marcadores conservadores de N2/N3/N4 e rejeita mutações que retirem limites ou alterem a cardinalidade documental. O workflow da CI chama esse teste; não é teste de execução Stan ou validade científica.

**Governança:** [issue #14 P0](https://github.com/riscotta/bayes-stan-r/issues/14) de licença/proveniência/privacidade, [#15](https://github.com/riscotta/bayes-stan-r/issues/15) (Stan) e [#16](https://github.com/riscotta/bayes-stan-r/issues/16) abertas. Fonte Dryad CC0 não autentica nem licencia automaticamente quatro derivados. Insumos E04 externos e autorizações permanecem não homologados. **Sem revisão independente no SHA final desta PR, não há merge nem T06.7 QA**. A Etapa 06 permanece aberta nesse gate, e T07 não deve iniciar automaticamente.
