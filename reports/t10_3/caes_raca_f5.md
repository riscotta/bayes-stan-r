# caes_raca_f5 — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Histórico com síntese rastreável (T08.4). **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/caes_raca_f5/README.md) preservado, blob `7656e617336ff5928af83c556edf48bcd5c69462`.

## 1. Pergunta e objeto

A raça pura declarada pelo tutor acrescenta previsão fora do treino do F5 além de idade, sexo, porte e esterilização?

**Objeto documentado:** Cães de raça pura declarada pelo tutor; F5 (Agonistic Threshold), idade, sexo, porte e esterilização; avaliação preditiva interna.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

Base analítica E04 individual externa, questionário F5 e agregados congelados de métricas e folds; raça declarada, não genótipo. caes_raca_f5 (redistribution_status=aggregates_only, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** caes_raca_f5: license_status=source_terms_verified, redistribution_status=aggregates_only.

**Situação posterior:** Sem decisão específica de permanência identificada entre os 14 conjuntos da issue #14 para esta unidade; permanecem os limites do inventário e da fonte. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Diferença de erro de previsão interno entre M1 (covariáveis) e M2 (covariáveis + intercepto por raça declarada), por família; RMSE, logscore, interval score, cobertura e ΔRMSE pareado. Não é efeito causal de raça.

M2: μ_i = α + x_iᵀβ + τ z_raça[i], z ~ Normal(0; 1). O fonte E07 escolhe Normal(μ; σ) ou Student-t com 5 graus de liberdade; α ~ Normal(0; 1,5), β ~ Normal(0; 0,5), σ e τ positivas com priors normais truncadas; escala τ fornecida como dado. Na candidata skew-normal E08_MP, a localização é deslocada por ωδ√(2/π), δ = λ/√(1+λ²), para preservar a média condicional; τ tem escala 0,5. Isto identifica as especificações versionadas, não certifica que cada variante foi executada.

Notação condensada editorial da especificação inspecionada; ponto e vírgula separa parâmetros, vírgula representa decimal. Normal usa média e desvio-padrão; Lognormal usa localização e escala no log; Exponential usa taxa, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

M0 média de treino; M1 covariáveis; M2 interceptos hierárquicos de raça; famílias Normal, Student-t e skew-normal; comparação interna por folds e bootstrap pareado. Código identificado: caes_raca_f5_cmdstanr.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/caes_raca_f5/original/E08_MP_Entregaveis/03_referencias_congeladas/modelo_M2_E07.stan`](../../scripts/caes_raca_f5/original/E08_MP_Entregaveis/03_referencias_congeladas/modelo_M2_E07.stan) — modelo completo; blob `4b470134f8d027a95d042c7bd990c9ea51f21134`.
- [`scripts/caes_raca_f5/original/E08_MP_Entregaveis/02_codigo/stan/M2_skew_normal_E08_MP.stan`](../../scripts/caes_raca_f5/original/E08_MP_Entregaveis/02_codigo/stan/M2_skew_normal_E08_MP.stan) — modelo completo; blob `de87079eb2cc170438e378abb5098a22764125e0`.
- [`scripts/caes_raca_f5/README.md`](../../scripts/caes_raca_f5/README.md) — linhas 25–39; blob `7656e617336ff5928af83c556edf48bcd5c69462`.

- [`reports/caes_raca_f5/relatorio_cientifico.md`](../../reports/caes_raca_f5/relatorio_cientifico.md) — blob `acfcd5afece447769b1c17ca75a13aa4ba1ace2b`.
- [`reports/t06_6/README.md`](../../reports/t06_6/README.md) — blob `8a8fc680919ac17916af61e5930bc9b2a37bd5ab`.
- [`reports/t07_7/README.md`](../../reports/t07_7/README.md) — blob `5c08cd99a6b1f5301f3a74feab07949ec91920d6`.

## 5. Diagnósticos e resultados

O README registra 5 folds, bootstrap pareado por cão e cobertura por subgrupo. A redução interna de RMSE M2 frente a M1 foi 0,85%–1,41%; piora no fold 1; cobertura condicional não validada. Folds após exploração prévia, sem holdout externo; raça declarada e F5 não mede mordidas.

**Natureza da saída descrita no catálogo:** Síntese histórica de comparação M1/M2, RMSE, cobertura, logscore, interval score e contrastes por fold; relatório histórico e agregados identificados no README, seção Modelos e resultados; sem transcrição das métricas ou estimativas.

A interpretação histórica também é delimitada nos [sumários T08.4](../../scripts/STUDY_SUMMARIES_T08_4.md) e [evidências T08.6](../../scripts/STUDY_EVIDENCE_T08_6.md).

## 6. Limites e reprodutibilidade

Sem inferência genética causal, previsão individual de mordidas, calibração condicional nem validação externa; E04 individual externa.

N2 T06.6: 30/30 cotejos de quatro agregados, sem ajuste Stan; confirmação dos passos N2 na CI da base T10.2, não nova execução desta T10.3. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Conferir base E04 e fits originais em ambiente autorizado, separação por tutor/fold, calibração por subgrupo e validação externa; não alterar definição de F5 ou alegar causalidade. Adaptador suspenso; piloto N2 limitado a quatro agregados.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
