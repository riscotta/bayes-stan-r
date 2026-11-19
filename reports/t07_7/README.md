# T07.7 — Mapa de migração e caminhos que continuam históricos

**Tarefa literal do Plano de Correção:** “Atualizar documentação de migração e declarar os caminhos que continuam históricos.”

**Corte técnico:** `main` `c81994e82eee3d19feb56e6cb00ac9f25ed82799` (19/11/2026, após o squash do [PR #63](https://github.com/riscotta/bayes-stan-r/pull/63)). A [CI pós-merge #44587809790](https://github.com/riscotta/bayes-stan-r/actions/runs/44587809790) terminou com os jobs Python, R e Stan sintático em `success`. **Este documento mapeia rotas; não executa nem migra código científico.** Ele não encerra a T07.6, não altera a política T06.4 e não promove estados N2/N3/N4.

## 1. Mapa verificável de rotas

| Objeto | Interface/caminho vigente | Caminho original ou legado preservado | Estado efetivo e limite |
|---|---|---|---|
| PNAD Estudo I — auditoria E10 N2 pública | [`adapters/t07_3/runner.py`](../../adapters/t07_3/runner.py), modo `auditoria_rapida_N2`, ação `planejar` ou `executar`; escreve somente `outputs/pnadc_estudo_i/logs/<id>/` | [`scripts/pnadc_estudo_i/auditar_e10.py`](../../scripts/pnadc_estudo_i/auditar_e10.py) e [CSV de entrada E09 congelado](../../data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv) | **Adaptada apenas para auditoria pública N2.** A CI confirma identidade operacional contemporânea do texto entre adaptador e chamada direta ao mesmo script; não é comparação histórica independente |
| PNAD — cadeia E04–E09 e PowerShell E07 | **Sem rota no adaptador canônico** | [`scripts/pnadc_estudo_i/estudo_I/`](../../scripts/pnadc_estudo_i/estudo_I/) (E04 `saida/`, E05 `saida/`, E07 `run.ps1`/resumos, E08 e E09) | **Histórica, não migrada.** Entradas locais, escritas legadas e amostragem/reprocessamentos não são autorizados por este mapa |
| Mortalidade SP 0–74 | Runner T07.3 admite `planejar`, **recusa `executar`** | [`scripts/mortalidade_sp_0_74/reproduzir_E10.R`](../../scripts/mortalidade_sp_0_74/reproduzir_E10.R); diretórios históricos e `outputs/mortalidade_sp_0_74/` | **Rota do adaptador suspensa.** A auditoria pública R `--modo=auditar` executada pela CI é um fluxo original separado, não evidência de adaptação liberada, migração completa ou N3 |
| Cães F5 | Runner T07.3 admite `planejar`, **recusa `executar`** | [`scripts/caes_raca_f5/caes_raca_f5_cmdstanr.R`](../../scripts/caes_raca_f5/caes_raca_f5_cmdstanr.R) e [`scripts/caes_raca_f5/original/`](../../scripts/caes_raca_f5/original/) (incluindo `E08_MP_Entregaveis`) | **Rota do adaptador suspensa.** O piloto N2 de agregados T06.6 não libera o wrapper, o ajuste Stan nem os caminhos históricos |
| Outros scripts do repositório | **Fora do adaptador T07.3/v1** | Inventário [T07.1](../t07_1/inventario_scripts_T07_1.csv): 59/59 scripts executáveis inventariados; 9 sinalizados com `LEGADO` | **Sem alegação de migração geral.** A triagem lexical de caminhos/escritas não é prova de segurança de execução nem de ausência de efeitos colaterais |

O [manifesto T07.2](../t07_2/README.md) preserva o código original em `scripts/` sob verificação de hashes, sem renomear ou reescrever fontes científicas. A [matriz T07.4](../t07_4/README.md) especifica os limites das rotas com escrita; os [testes T07.5](../t07_5/README.md) verificam contrato e smoke sintético da interface.

## 2. Onde ficam entradas, saídas e referências

- **Entradas/insumos históricos:** permanecem sob `data/raw/<estudo>/` ou nos caminhos originais documentados por estudo. Não movê-los para `outputs/`; a presença em Git não homologa redistribuição ou privacidade.
- **Código histórico:** permanece sob `scripts/`, incluindo rotas E04–E09 PNAD, scripts R de mortalidade, Cães F5, PowerShell, instaladores e caminhos com escrita. Não substituir por wrappers nem alterar hashes do manifesto T07.2 sem autorização e controle de mudança.
- **Saídas do adaptador PNAD executável:** `outputs/pnadc_estudo_i/logs/<id>/stdout.txt`, `stderr.txt` e `report.json`, criados para um ID novo sem sobrescrever arquivos existentes. São **artefatos gerados pela execução**, não arquivos históricos versionados.
- **Saídas regeneráveis gerais:** seguir [`outputs/README.md`](../../outputs/README.md): `outputs/<estudo>/<tipo>/`, com `tipo` em `figures`, `tables`, `models`, `logs`, `cmdstan_csv` ou `data_stan`. A convenção não prova que scripts legados já a cumpram integralmente.
- **Reexecuções sob T06.4:** a política [T06.4](../t06_4/README.md) reserva separadamente `outputs/_reexecucoes_t06_4/<estudo>/<execucao_id>/` para seu protocolo próprio. Não confundir esse caminho com o output do adaptador T07.3.
- **Referências e resumos auditados:** preservar a localização e os bytes; `data/raw/<estudo>/resultados_auditados/` é exceção de preservação. Um CSV auditado de **entrada** não se transforma em baseline homóloga de `stdout.txt`, `stderr.txt` ou `report.json`.

## 3. Critérios objetivos de interpretação e aceite

1. **Aceite técnico da documentação T07.7:** conferir este mapa contra `STUDIES` no runner, inventário T07.1, manifesto T07.2, convenção de saídas, contratos T07.3–T07.5 e evidência parcial T07.6. Exigir links válidos, CI verde e **QA independente no SHA do PR** antes de marcar T07.7 Exec./QA. Não há migração de código nesta tarefa.
2. **T07.6 ainda pendente:** a [entrega parcial](../t07_6/README.md) demonstrou `IGUALDADE_OPERACIONAL_N2` apenas sob mesmo código, entrada e ambiente; existem **zero pares históricos independentes autenticados**. `PNAD_STDOUT` e `PNAD_STDERR` aguardam custódia; não inferir ausência de `stderr` a partir do silêncio do log. `report.json` foi introduzido pelo adaptador; a não aplicabilidade histórica específica é **somente proposta**, não homologada.
3. **Proposta B+C:** o [PR #64](https://github.com/riscotta/bayes-stan-r/pull/64) continua separado e em draft; a decisão declarada de direção do plano **não dispensa** ato competente, direitos, revisão de não aplicabilidade ou alteração formal de critério. Nada neste mapa altera o critério literal T07.6.
4. **Rotas suspensas:** `mortalidade_sp_0_74` e `caes_raca_f5` não recebem `PASS` pelo simples inventário. Reabrir apenas com autorização por objeto, custódia, isolamento das escritas, comparação protocolada sob T06.4 e QA. Não rodar R/Stan/MCMC por esta documentação.
5. **Riscos externos:** [#14 — P0 de direitos e proveniência](https://github.com/riscotta/bayes-stan-r/issues/14), [#15 — validação Stan](https://github.com/riscotta/bayes-stan-r/issues/15) e [#16 — governança/release](https://github.com/riscotta/bayes-stan-r/issues/16) continuam independentes. Não usar este mapa para publicar dados protegidos, certificar N3/N4 ou encerrar a Etapa 07.

**Resultado desta tarefa:** delimitação documental dos caminhos **efetivamente adaptados** versus **historicamente preservados**. A conclusão da documentação não é conclusão de migração global nem resolução retroativa dos bloqueios T07.6.
