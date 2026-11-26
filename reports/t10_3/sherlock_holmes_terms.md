# sherlock_holmes_terms — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Histórico com síntese rastreável (T08.4). **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/sherlock_holmes_terms/README.md) preservado, blob `ef7276644ed479076d6f1e780c9a9e6f87dc4fc4`.

## 1. Pergunta e objeto

Como variam frequências de termos/personagens entre textos do corpus Sherlock Holmes?

**Objeto documentado:** Frequências de termos/personagens por obra no corpus Sherlock Holmes, com exposição em tokens.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

CSV compilado de textos Sherlock Holmes, entradas Stan e tabelas históricas auditadas; direitos da compilação pendentes. sherlock_holmes_terms (redistribution_status=review_required, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** sherlock_holmes_terms: license_status=not_identified, redistribution_status=review_required.

**Situação posterior:** A decisão de 26/11/2026 abrange `sherlock_holmes_terms`: preservar cópias históricas com rastreabilidade parcial, apoiada na declaração do responsável. Não certifica vintage, licença específica, semântica ou autorização de release. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Taxas posteriores de menções por 10 mil tokens e contrastes condicionais no corpus obra–termo selecionado.

y~Poisson_log(η), offset textual; interceptos de termo hierárquicos α~Normal(μ_α; σ_α), μ_α~Normal(log(3); 2), σ_α positiva Normal(0; 1,5); β_novel~Normal(0; 0,75), β_year~Normal(0; 0,35), efeitos não centrados obra e obra×termo.

Notação condensada editorial da especificação inspecionada; ponto e vírgula separa parâmetros, vírgula representa decimal. Normal usa média e desvio-padrão; Lognormal usa localização e escala no log; Exponential usa taxa, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Poisson-lognormal hierárquico com offset textual; modelo_operacional_poisson_lognormal_e07.stan. Código identificado: sherlock_holmes_terms_cmdstanr.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/sherlock_holmes_terms/modelo_operacional_poisson_lognormal_e07.stan`](../../scripts/sherlock_holmes_terms/modelo_operacional_poisson_lognormal_e07.stan) — linhas 46–89; blob `30eb457612078133056d6e6aca510dcbc9ea0142`.
- [`scripts/sherlock_holmes_terms/README.md`](../../scripts/sherlock_holmes_terms/README.md) — linhas 21–23, 61–63; blob `ef7276644ed479076d6f1e780c9a9e6f87dc4fc4`.


## 5. Diagnósticos e resultados

Síntese histórica de taxas globais ponderadas; esparsidade de termos raros e sensibilidade a priors/NB2 pendente. Rota padrão recompõe congelados; nenhum novo ajuste.

**Natureza da saída descrita no catálogo:** Auditorias, tabelas, figuras, logs e objeto RDS do ajuste; README seção Conclusão substantiva orienta comunicar taxas por exposição, não contagem bruta; sem republicar conteúdo do corpus ou estimativas.

A interpretação histórica também é delimitada nos [sumários T08.4](../../scripts/STUDY_SUMMARIES_T08_4.md) e [evidências T08.6](../../scripts/STUDY_EVIDENCE_T08_6.md).

## 6. Limites e reprodutibilidade

Domínio público das obras não homologa licença da compilação CSV; dependência do corpus preservada.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Identificar edição e montagem, regras de tokenização e dicionário de termos; verificar zeros, dependência obra–termo e robustez a NB2; não extrapolar a literatura em geral.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
