# T06.2 — Cinco estados independentes por estudo

**Tarefa literal:** “Registrar cinco estados independentes: integridade, síntese, pipeline, inferência, validação científica.”

**Data do snapshot:** 12/11/2026. **Base \`main\`:** \`3cfa372ab260eec21c8474844d46e98d3a4d78e5\`, squash T06.1 do PR [#51](https://github.com/riscotta/bayes-stan-r/pull/51). Esta tarefa é um registro conservador, não uma reexecução de estudos.

## Artefato e interpretação

[estados_reprodutibilidade_T06_2.json](estados_reprodutibilidade_T06_2.json) associa **33 estudos** do manifesto [T06.1](../t06_1/manifesto_estudos_T06_1.json) a cinco eixos sem nível global e sem implicações automáticas entre eles:

| Eixo | Pergunta distinta | Situação registrada pela T06.2 |
| --- | --- | --- |
| **Integridade** | Existe inventário com caminhos e objetos Git inspecionados? | N1 documental/estrutural registrado para 33/33 no snapshot T06.1, com QA e CI históricos. Não é verificação do conteúdo ou licença de dados externos. |
| **Síntese** | Há evidência delimitada de auditoria de indicadores ou produtos descritivos? | Duas referências documentadas: auditoria pública E09/PNAD Contínua (N2 descritivo) e auditoria pública de agregados da mortalidade SP. Para os demais 31, **não aferido nesta tarefa**. |
| **Pipeline** | Foi reproduzido o processo integral desde fontes originais e versões dos insumos? | Não aferido para 33/33. |
| **Inferência** | O modelo foi reestimado e diagnósticos/estimandos reproduzidos? | Não aferido para 33/33 neste contrato por estudo. |
| **Validação científica** | Houve avaliação independente de hipóteses, calibração, generalização e validade? | Não aferido para 33/33. |

**“Não aferido” significa ausência de evidência qualificada neste registro, não impossibilidade de execução nem invalidação de estudos históricos.** Um teste Stan sintético, a sintaxe validada ou uma CI verde não demonstram inferência histórica reexecutada. Mesmo evidência N2 não implica N3/N4.

## Evidências positivas limitadas

- **PNAD Contínua, Estudo I:** [matriz de evidências E10](../pnadc_estudo_i/e10_matriz_evidencias_reproducao_T04_6.md) e [CI \`push/main\` após T06.1](https://github.com/riscotta/bayes-stan-r/actions/runs/43234521388), passo \`Audit published PNAD E09 projection (N2 only)\`. Testes sobre a projeção pública congelada; não reconstrói SIDRA, E04–E08, MCMC ou 104 probabilidades.
- **Mortalidade de São Paulo (0–74):** [modo público de auditoria](../../scripts/mortalidade_sp_0_74/reproduzir_E10.R) e a mesma CI, passo \`Verify published Sao Paulo mortality indicators\`. Não significa reprocessamento integral das fontes territoriais ou reestimativa de modelo.

Essas referências são **evidências históricas já existentes**, não comandos executados pela T06.2. O teste desta tarefa verifica a integridade das declarações, fontes versionadas, cobertura e o veto à promoção automática; não executa pipelines nem acessa conteúdos de insumos institucionais.

## Governança, limites e continuidade

A [issue #14](https://github.com/riscotta/bayes-stan-r/issues/14) permanece **P0 aberta**: proveniência, redistribuição, permissões institucionais e possível tratamento de dados pessoais não homologados. As issues [#15](https://github.com/riscotta/bayes-stan-r/issues/15) e [#16](https://github.com/riscotta/bayes-stan-r/issues/16) continuam abertas. Não remover ou disponibilizar novos datasets, alterar modelos e outputs históricos, nem publicar releases com base nesta classificação.

Os cinco estados **não são um ranking ordinal de qualidade** e não devem ser agregados por máximo, soma, pontuação ou qualquer regra de transitividade. A T06.2 não substitui T06.3 (modos executáveis), T06.4 (política de artefatos), T06.5 (lacunas de dependências), T06.6 (piloto) ou T06.7 (READMEs por estudo). A Etapa 06 permanece aberta.

## Teste de contratos e mutações

\`\`\`bash
python -m unittest discover -s tests/python -p 'test_estados_t06_2.py' -v
\`\`\`

\`tests/python/test_estados_t06_2.py\` utiliza somente manifesto/README/árvore de caminhos do Git públicos, confere 33 IDs e cinco estados e rejeita estudos/eixos omitidos, licença falsa, promoção indevida de pipeline ou validade científica, síntese inventada e referências adulteradas. Não valida materialmente a execução remota histórica: o resultado dos jobs é evidenciado pelo GitHub, não por estes contratos locais.
