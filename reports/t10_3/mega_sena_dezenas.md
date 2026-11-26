# mega_sena_dezenas — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Histórico com síntese rastreável (T08.4). **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/mega_sena_dezenas/README.md) preservado, blob `d875f490f04f6d482f24c36264e116add878893f`.

## 1. Pergunta e objeto

Como se distribuem as dezenas por fatores temporais e nominais históricos?

**Objeto documentado:** Distribuição das dezenas sorteadas segundo fatores nominais e temporais; recorte de concursos não explicitado no README.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

Dados históricos e artefatos auditáveis versionados do estudo Mega-Sena; identificação e direitos do extrato pendentes. mega_sena_dezenas (redistribution_status=review_required, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** mega_sena_dezenas: license_status=not_identified, redistribution_status=review_required.

**Situação posterior:** A decisão de 26/11/2026 abrange `mega_sena_dezenas`: preservar cópias históricas com rastreabilidade parcial, apoiada na declaração do responsável. Não certifica vintage, licença específica, semântica ou autorização de release. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Composição de probabilidades das dezenas por fator e contraste de estabilidade prática; ranking publicado é estatística de ordem, não previsão de concurso.

Vetor de contagens y_g ~ Multinomial(p_g); modelo log-linear por fator com α ~ Normal(0; 0,35), efeitos z_delta ~ Normal(0; 1), τ_delta positiva Normal(0; 0,20). A especificação global é distinta. O registro alternativo E07 usou Dirichlet-multinomial em ambiente sem Stan; não é o ajuste histórico Stan nem a configuração planejada.

Notação condensada editorial da especificação inspecionada; ponto e vírgula separa parâmetros, vírgula representa decimal. Normal usa média e desvio-padrão; Lognormal usa localização e escala no log; Exponential usa taxa, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Modelo loglinear multinomial por fator e multinomial global (dois Stan nomeados no README). Código identificado: mega_sena_dezenas_cmdstanr.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/mega_sena_dezenas/modelo_loglinear_multinomial_fator.stan`](../../scripts/mega_sena_dezenas/modelo_loglinear_multinomial_fator.stan) — linhas 25–43; blob `0f52a58c999af9560ab5a9affa934f8f905ca844`.
- [`scripts/mega_sena_dezenas/README.md`](../../scripts/mega_sena_dezenas/README.md) — linhas 20–22, 59–61; blob `d875f490f04f6d482f24c36264e116add878893f`.

- [`scripts/STUDY_EVIDENCE_T08_6.md`](../../scripts/STUDY_EVIDENCE_T08_6.md) — blob `5e96cf04e4ffb4ad557463c208eb21431c035d9f`.

## 5. Diagnósticos e resultados

Síntese E09 indica estabilidade prática; comparação por ano exploratória com uma divergência MCMC. Rota padrão recompõe congelados; não houve reamostragem nesta tarefa.

**Natureza da saída descrita no catálogo:** Tabelas, figuras e logs da síntese histórica; conclusão qualitativa declarada no README: estabilidade prática na escala avaliada, sem suporte a rankings ou previsão de concursos; sem republicar estimativas. Seções Saídas geradas e Conclusão substantiva.

A interpretação histórica também é delimitada nos [sumários T08.4](../../scripts/STUDY_SUMMARIES_T08_4.md) e [evidências T08.6](../../scripts/STUDY_EVIDENCE_T08_6.md).

## 6. Limites e reprodutibilidade

Resultados históricos de loteria não fornecem vantagem preditiva; licença/origem do extrato pendente.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Conferir extração e concursos, dependência entre dezenas no mesmo sorteio, diagnóstico por fator e critério de estabilidade; não oferecer ranking preditivo.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
