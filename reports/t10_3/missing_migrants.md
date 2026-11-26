# missing_migrants — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Histórico com síntese rastreável (T08.4). **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/missing_migrants/README.md) preservado, blob `17876280be00c2497604041f9f051553896ea2c1`.

## 1. Pergunta e objeto

Como variam contagens registradas de mortes/desaparecimentos por rota e mês?

**Objeto documentado:** Mortos e desaparecidos registrados por rota migratória e mês, não mortalidade total real.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

CSV IOM Missing Migrants Project, entradas de modelagem congeladas e síntese auditável; exposição em dias, cobertura limitada a registros. missing_migrants (redistribution_status=review_required, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** missing_migrants: license_status=not_identified, redistribution_status=review_required.

**Situação posterior:** A decisão de 26/11/2026 abrange `missing_migrants`: preservar cópias históricas com rastreabilidade parcial, apoiada na declaração do responsável. Não certifica vintage, licença específica, semântica ou autorização de release. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Intensidade/carga de mortos e desaparecidos registrados por rota e mês, tendência temporal e sazonalidade, com exposição em dias. Não é risco por pessoa migrante.

y ~ NB2_log(η,φ), η incorpora offset de dias, intercepto/inclinação hierárquicos de rota e mês. α ~ Normal(log(0,5),2), β_time ~ Normal(0,0,3), φ ~ Lognormal(log(5),1), efeitos não centrados e escalas positivas. Var(y|μ,φ)=μ+μ²/φ.

Notação condensada editorial da especificação inspecionada; Normal usa média e desvio-padrão, Lognormal usa localização e escala no log, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

NB2 hierárquico rota-mês com offset de dias; modelo_nb_hierarquico_rota_mes_estavel.stan. Código identificado: missing_migrants_cmdstanr.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/missing_migrants/modelo_nb_hierarquico_rota_mes_estavel.stan`](../../scripts/missing_migrants/modelo_nb_hierarquico_rota_mes_estavel.stan) — linhas 41–68; blob `7c6f4330eb3c5a4976d8baa35707ed159ba42273`.
- [`scripts/missing_migrants/README.md`](../../scripts/missing_migrants/README.md) — linhas 21–23, 61–63; blob `17876280be00c2497604041f9f051553896ea2c1`.


## 5. Diagnósticos e resultados

Síntese histórica: concentração por rota e tendência média anual positiva; R-hat acima do alvo estrito em poucos parâmetros e PPC de zeros/dispersão desfavoráveis. Reamostragem não aferida.

**Natureza da saída descrita no catálogo:** Tabelas, figuras, logs e entradas Stan; README seção Conclusão substantiva documenta interpretação da carga registrada e ressalvas Rhat, zeros e dispersão; sem transcrever achados protegidos, estimativas ou tabelas.

A interpretação histórica também é delimitada nos [sumários T08.4](../../scripts/STUDY_SUMMARIES_T08_4.md) e [evidências T08.6](../../scripts/STUDY_EVIDENCE_T08_6.md).

## 6. Limites e reprodutibilidade

Registros IOM incompletos não equivalem ao total de mortes; README assinala ressalvas de diagnóstico Rhat e PPC em zeros/dispersão; licença do snapshot não identificada (license_status=not_identified); redistribuição requer revisão (redistribution_status=review_required).

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Rever cobertura, mudanças de registros e exposição; investigar zeros/dispersion e sensibilidade; sem denominador de fluxo não estimar mortalidade real nem risco por migrante.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
