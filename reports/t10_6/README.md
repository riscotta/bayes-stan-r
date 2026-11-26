# T10.6 — Revisão técnico-científica transversal

**Data:** 26/11/2026. Base fixa: [147704cb7ac491b35dfe4e57eb0182c8f2a58b42](https://github.com/riscotta/bayes-stan-r/commit/147704cb7ac491b35dfe4e57eb0182c8f2a58b42), árvore `c0f211b75954cb10ffc897dd034cb7c9979fe39e`. T10.5 integrada no [PR #81](https://github.com/riscotta/bayes-stan-r/pull/81), [CI pós-merge #45873046729](https://github.com/riscotta/bayes-stan-r/actions/runs/45873046729) success. Parecer do executor em leitura estática; não QA independente nem nova execução inferencial.

## Seleção e método

Amostra intencional, não probabilística: 13/33 unidades. Inclui os oito estudos aplicados com síntese histórica rastreável na [T08.4](../../scripts/STUDY_SUMMARIES_T08_4.md), prioritários pelo risco de extrapolação; dois laboratórios (Deck: conjugação/simulação e tempo de espera; Inhaler: ordinal/crossover/holdout); três aplicações sem síntese final localizada (RS Seguro: seleção/custódia; ISUS/SIA: bootstrap/precificação/custódia; Importações: log-Student-t, painel e prior). Cobertura por finalidade e risco, não representatividade. Os 20 restantes não receberam revisão científica direta T10.6.

Confrontar [adendos T10.3](../t10_3/README.md), fontes R/Stan, READMEs, ponte E06/E07/E08/E09, [gate de diagnósticos históricos](../SCIENTIFIC_VALIDITY_GATE_2026-10-28.md) e [decisão de permanência #14](../../data/DATA_PROVENANCE_DECISIONS.md). Avaliar suporte, priors, likelihood, estimando, interpretação, PPC/convergência/sensibilidade quando pertinentes, origem e direitos. A matriz identifica caminho, intervalo de linha e blob no SHA base, com localizadores da leitura seletiva; não é prova de revisão integral de todos os modelos/variantes. Nenhum dado bruto/individual aberto; nenhuma execução científica.

## Matriz de achados e correções

[Matriz de achados](matriz_achados.csv): 20 registros, 13 unidades, 37 caminhos-fonte distintos e 62 localizadores. Gravidade ALTA = risco relevante à interpretação, uso ou publicação; MEDIA = diligência delimitada; INFORMATIVO = confronto estático. Não são prioridades oficiais de issue ou estados E00–E10.

| Natureza | Registros | Significado |
| --- | ---: | --- |
| CONFRONTO_ESTATICO | 4 | Propriedade verificada no fonte, não PASS inferencial. |
| RESSALVA_DOCUMENTADA | 7 | Limitação histórica, não nova medição. |
| RISCO_METODOLOGICO | 2 | Risco deduzido, requer teste ou protocolo. |
| LIMITE_MATEMATICO | 1 | Consequência analítica demonstrável. |
| ACHADO_CODIGO | 4 | Comportamento constatado no fonte; impacto empírico não aferido. |
| PENDENCIA_EVIDENCIA | 2 | Evidência insuficiente no recorte lido. |

| Unidade | Registros | Foco |
| --- | --- | --- |
| [caes_raca_f5](../t10_3/caes_raca_f5.md) | A01, A02 | Tutor indisponível, ganho interno e subcobertura. |
| [mega_sena_dezenas](../t10_3/mega_sena_dezenas.md) | A03, A04 | Multinomial agregada, sem reposição e divergência histórica. |
| [missing_migrants](../t10_3/missing_migrants.md) | A05 | PPC zeros/dispersão e convergência, carga registrada. |
| [mortalidade_sp_0_74](../t10_3/mortalidade_sp_0_74.md) | A06 | GLM, território provisório, não causalidade. |
| [pnadc_estudo_i](../t10_3/pnadc_estudo_i.md) | A07, A08 | Marginal versus cenário, covariâncias e diagnósticos. |
| [prisons_prisoners_unodc](../t10_3/prisons_prisoners_unodc.md) | A09, A10 | Log-t, mistura Beta, LOO/SBC e transporte. |
| [sherlock_holmes_terms](../t10_3/sherlock_holmes_terms.md) | A11, A12 | Sentinela RNG, exposição e corpus. |
| [sobrevivencia_cadastral_poa](../t10_3/sobrevivencia_cadastral_poa.md) | A13 | LOO interno sem unidade comprovada no recorte, baixa administrativa. |
| [deck_15wins](../t10_3/deck_15wins.md) | A14 | Conjugação confirmada, SBC não demonstrado. |
| [inhaler_ordinal](../t10_3/inhaler_ordinal.md) | A15 | Holdout intrassujeito, não transporte a novos sujeitos. |
| [rs_seguro](../t10_3/rs_seguro.md) | A16, A17 | Prior empírica, seleção condicional e alerta de custódia. |
| [isus_sia](../t10_3/isus_sia.md) | A18 | Bootstrap CNES, perda e validade de preço. |
| [importacoes_world_bank](../t10_3/importacoes_world_bank.md) | A19, A20 | Log USD corrente, transformação, prior empírica e validação. |

## Correções de leitura científica sem reescrita de histórico

- A02: agrupamento por tutor requer obter identificador autorizado inexistente no README. Não propor execução imediata com campo inexistente.
- A09/A19: para Z Student-t com graus de liberdade finitos e escala positiva, a densidade na cauda direita é assintoticamente proporcional a z^(-nu-1). A integral de exp(z) vezes essa densidade diverge no infinito. Assim, exp(Z) tem esperança infinita. A massa positiva do hurdle UNODC não elimina essa propriedade; o desfecho de importações é log de USD correntes positivos. Mediana e quantis condicionais na escala original são definidos. Trata-se de consequência da likelihood, não novo MCMC ou prova de erro em média histórica específica. Em importações exp(mean(beta)) é transformação do resumo de beta, não média posterior de exp(beta) nem média preditiva em USD; inclinação total de grupo com interação combina coeficientes relevantes.
- A10: mu da mistura Beta é média interior; a média marginal condicional é (1-p_boundary)*mu+p_boundary*p_one_given_boundary. Contrastes observados no painel não são posteriores.
- A11: sentinela -1 do RNG Sherlock está fora do suporte Poisson; auditar incidência e tratamento antes de interpretar PPC. Não se sabe se o ramo foi acionado historicamente.
- A15: holdout do Inhaler é primariamente por linha dentro de sujeito, não holdout de sujeitos inteiros. O alvo deve explicitar se o sujeito foi observado.
- A16/A19: prior centrada em estatística da amostra usada é ancoragem empírica; declarar e testar sensibilidade, sem afirmar viés observado ou alterar prior sem protocolo.
- A03/A13: multinomial de slots não representa literalmente subconjuntos sem reposição; LOO agregado exige unidade de predição e estrutura de dependência explícitas. Questões de adequação/evidência, não prova de invalidade numérica geral.

## Controles e limites

- Gravidades: 15 ALTA, 4 MEDIA, 1 INFORMATIVO. Não são 20 falhas inéditas; são registros de evidência, correção de leitura e diligência. 4 achados de código e 1 limite matemático classificados estaticamente; nenhum impacto numérico medido.
- 37 caminhos e blobs confrontados com a árvore base não truncada, após leitura direta seletiva dos fontes e documentos indicados. Os intervalos referem-se aos trechos inspecionados; não se executou uma segunda leitura integral de todos os arquivos para validar intervalos. Todos os IDs pertencem ao índice T10.3.
- Não abrir bases individuais, cadeias ou executar Stan/R científico. Não exigir prior/MCMC de GLM frequentista ou bootstrap. Os três N2 seguem conforme [T10.2](../t10_2/README.md); nenhum E00–E10, N3/N4 ou validade científica promovido. Sem inferência de falha por ausência de artefato.
- #14 encerrada no escopo documental de permanência, não licença geral, saneamento dos alertas semânticos ou autorização de release. T09.1 registra alertas de Mortalidade SP e RS Seguro, não classificação LGPD definitiva.
- Correções científicas de leitura documentadas nesta entrega; alterações de modelos, código ou dados e novos fits exigem protocolos próprios, autorização e QA. QA independente do diff e CI no HEAD exato antes de integrar. T10.7 não iniciada. Release integral sem decisão.
