# Etapa 09 — Auditoria final

Abertura de T09.1–T09.6 aprovada pelo responsável em 26/11/2026. Base de auditoria: `799f9356e18eb58a7048d1bc00e77c1c3b7b8208`, após o [encerramento documental da Etapa 08](../t08_7/README.md). T09.7 tem deliberação própria.

## T09.1 — Inventário e proveniência

O [manifesto](../../data/TRACKED_DATA_MANIFEST.csv) contém 117 caminhos distintos, classificados em 18 conjuntos. O cotejo com a árvore Git retornou 117/117 coincidências de caminho, blob e tamanho e nenhum arquivo adicional em `data/raw/` fora do manifesto, excluídos Markdown e `.gitkeep`. O [inventário de proveniência](../../data/DATA_PROVENANCE.csv) cobre 25 conjuntos, incluindo insumos externos. Entre os arquivos do manifesto, 83 estão em conjuntos com `review_required` e 34 com `aggregates_only`.

A [decisão da issue #14](../../data/DATA_PROVENANCE_DECISIONS.md) registra a declaração do responsável e a preservação de 14 conjuntos, associados a 83 arquivos, com rastreabilidade parcial. A publicação futura tem escopo próprio. O inventário conserva as lacunas sobre termos específicos e identificação das extrações. A composição de um eventual release e a autorização nesse escopo serão verificadas na deliberação T09.7.

## T09.2 — Execução e ambientes

A [CI da base](https://github.com/riscotta/bayes-stan-r/actions/runs/45813650404) passou com Python 3.12.8 para N2, stanc 2.40.0 para sintaxe representativa e R 4.5.1 para sintaxe, testes e auditorias delimitadas. A CI deste PR repete a configuração no candidato. A [execução científica selecionada](../t05_5/protocolo_prior_predictive_diagnosticos_T05_5.md#verificação-t092) usa CmdStan 2.40.0 e somente fixtures sintéticas, com checks preditivos e HMC predefinidos para dois modelos. As rotas com insumos externos e as [suspensões do adaptador](../t07_7/README.md) seguem seus contratos.

## T09.3 — Amostra de contratos e manifestos

Amostra intencional de quatro unidades, escolhida para cobrir laboratório sintético, laboratório com insumo real, estudo aplicado com restrição territorial e estudo aplicado com tipos de evidência distintos. O [manifesto T06.1](../t06_1/README.md), a [taxonomia](../../scripts/STUDY_TAXONOMY_T02_1.md) e o [contrato por etapa](../../scripts/STUDY_STAGE_STATUS_T02_6.md) foram usados na leitura.

| Unidade | Teste documental e limite |
| --- | --- |
| [Deck](../../scripts/deck_15wins/README.md), laboratório | Identificação, pergunta e método no README, script no manifesto e dados simulados internamente. Separar comparação analítica do exercício e validação empírica. |
| [Baseball hierárquico](../../scripts/hierarchical/README.md), laboratório | Código e insumo identificados no manifesto. Exemplo de pooling parcial com licença específica ainda sem identificação no inventário. |
| [Mortalidade SP](../../scripts/STUDY_PILOT_T02_7_MORTALIDADE_SP.md), aplicado | Piloto preenche os oito campos e documenta estados E00–E10. O E00 é reconstrução editorial e a correspondência distrital permanece provisória. |
| [PNAD Estudo I](../pnadc_estudo_i/e10_matriz_evidencias_reproducao_T04_6.md), aplicado | Reconstrução E00, fonte histórica, diagnósticos e projeção pública N2 têm referências distintas. A E10 documenta insumos externos e limites de cada rota. |

Os testes locais dos contratos T06.1, PNAD N2 e T08.3 passaram (55 testes). Os testes R configurados incluem os templates e o piloto T02.7.

## T09.4 — Conclusões e riscos residuais

O cotejo editorial usa os [sumários](../../scripts/STUDY_SUMMARIES_T08_4.md), [exemplos](../../scripts/STUDY_EXAMPLES_T08_5.md) e [versões e evidências](../../scripts/STUDY_EVIDENCE_T08_6.md). Pontos críticos para o parecer final:

- Cães/F5 apresenta ganho interno pequeno. Mortalidade SP usa padronização descritiva e correspondência territorial provisória. A PNAD combina contrastes descritivos e cenários gaussianos condicionais com correlação postulada.
- Mega-Sena limita a estabilidade à escala examinada e trata a posição como estatística de ordem. IOM distingue contagens registradas de risco por migrante e registra falhas preditivas em zeros e dispersão.
- Sherlock Holmes restringe as taxas ao corpus e vocabulário. Porto Alegre mede baixa administrativa. UNODC distingue diferenças observadas de PPC e preserva a limitação de transporte para novos países.

## T09.5 — Governança do repositório

Em 26/11/2026, o [ruleset da main](https://github.com/riscotta/bayes-stan-r/rules/26061035) estava ativo, com PR e check obrigatório de R. O [PR #75](https://github.com/riscotta/bayes-stan-r/pull/75) está integrado e o [#64](https://github.com/riscotta/bayes-stan-r/pull/64) permanece em rascunho. As issues [#15](https://github.com/riscotta/bayes-stan-r/issues/15) e [#16](https://github.com/riscotta/bayes-stan-r/issues/16) estão abertas. O [CHANGELOG](../../CHANGELOG.md) e a [versão publicada v1.0.0](https://github.com/riscotta/bayes-stan-r/releases/tag/v1.0.0) foram cotejados. A [citação](../../CITATION.cff) foi lida como YAML e registra título, autor e URL do software. A verificação de links internos da T08.7 é evidência de base no seu recorte.

## T09.6 — Parecer independente

O parecer final será emitido após a conferência das execuções do candidato e a revisão dos achados documentais. A decisão de publicação é tratada na T09.7.
