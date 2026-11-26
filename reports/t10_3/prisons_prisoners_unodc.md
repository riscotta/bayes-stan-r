# prisons_prisoners_unodc — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Histórico com síntese rastreável (T08.4). **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/prisons_prisoners_unodc/README.md) preservado, blob `645842fb3ee0756dee711b0936ca3327285704b6`.

## 1. Pergunta e objeto

Como variam taxas de encarceramento, prisão sem sentença e contraste por sexo entre países?

**Objeto documentado:** Países, taxa held_rate, proporção unsentenced_pct e contraste Female−Male em painel pareado.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

Base nacional UNODC Prisons and Prisoners, entradas Stan congeladas e síntese auditável; vintages e comparabilidade a rever. prisons_prisoners_unodc (redistribution_status=review_required, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** prisons_prisoners_unodc: license_status=attribution_documented_license_unverified, redistribution_status=review_required.

**Situação posterior:** A decisão de 26/11/2026 abrange `prisons_prisoners_unodc`: preservar cópias históricas com rastreabilidade parcial, apoiada na declaração do responsável. Não certifica vintage, licença específica, semântica ou autorização de release. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Três objetos separados: heterogeneidade de held_rate; nível de unsentenced_pct; contraste observado Female−Male em painel pareado e avaliação via M3/PPC.

M1 REV5 hurdle: massa em zero via Bernoulli e Student-t para log(held_rate) quando held_rate > 0, com efeitos de país/região e random walks; ν−2 ~ Exponential(0,10). M2/M3 misturas Beta com massas em 0 e 1 (probabilidade de fronteira e probabilidade condicional de 1); interior Beta(μφ; (1−μ)φ). M3 inclui pareamento e efeitos de sexo específicos; α ~ Normal(logit(0,30); 1,2), logφ ~ Normal(log(20); 1) em M2/M3.

Notação condensada editorial da especificação inspecionada; ponto e vírgula separa parâmetros, vírgula representa decimal. Normal usa média e desvio-padrão; Lognormal usa localização e escala no log; Exponential usa taxa, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

M1 REV5 hurdle + Student-t log(held_rate); M2 REV4.1 zero-one-inflated Beta; M3 REV4.1 pareado. Código identificado: prisons_prisoners_unodc_cmdstanr.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/prisons_prisoners_unodc/modelo_M1_held_rate_hurdle_REV5.stan`](../../scripts/prisons_prisoners_unodc/modelo_M1_held_rate_hurdle_REV5.stan) — linhas 59–88; blob `298e4bacf7cbf06d199e012445769ca095506f89`.
- [`scripts/prisons_prisoners_unodc/modelo_M2_unsentenced_total.stan`](../../scripts/prisons_prisoners_unodc/modelo_M2_unsentenced_total.stan) — linhas 72–110; blob `9f6c3681b2065d66db1871d439bf7521e0a210a6`.
- [`scripts/prisons_prisoners_unodc/modelo_M3_unsentenced_sex.stan`](../../scripts/prisons_prisoners_unodc/modelo_M3_unsentenced_sex.stan) — linhas 71–113; blob `a62eb954816c8c13a78d827b22538c699f4bd3ac`.
- [`scripts/prisons_prisoners_unodc/README.md`](../../scripts/prisons_prisoners_unodc/README.md) — linhas 37–55; blob `645842fb3ee0756dee711b0936ca3327285704b6`.


## 5. Diagnósticos e resultados

README registra exp(σ_country)≈1,94 por DP de efeito; média observada sem sentença ≈32,1%; diferenças observadas +3,72 p.p. e +1,79 p.p. com composição temporal fixada, não contrastes posteriores. LOO agrupado não validou transporte a países novos; M2 reproduz média melhor que mediana.

**Natureza da saída descrita no catálogo:** Sínteses de heterogeneidade por país, proporção sem sentença e contraste por sexo; seção Resultados centrais do estudo; ressalva PSIS-LOO agrupado não validou transporte a países novos; sem republicar métricas.

A interpretação histórica também é delimitada nos [sumários T08.4](../../scripts/STUDY_SUMMARIES_T08_4.md) e [evidências T08.6](../../scripts/STUDY_EVIDENCE_T08_6.md).

## 6. Limites e reprodutibilidade

Citação UNODC não prova licença de redistribuição; comparabilidade internacional e vintages precisam de revisão.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Conferir comparabilidade e denominadores internacionais, adequação dos extremos, sensibilidade a cobertura temporal e validação agrupada; não extrapolar causalidade nem transporte.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
