# retractions_trend — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Aplicado com método/rota descritos; síntese final não localizada na triagem. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/retractions_trend/README.md) preservado, blob `820574b8c9b34f1d5c682d171f9eab452b436647`.

## 1. Pergunta e objeto

Como se comporta a tendência temporal de retratações científicas?

**Objeto documentado:** Contagens anuais globais de retratações por editora ou país; anos completos até 2025 por padrão, 2026 incompleto.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

CSV externo Kaggle Global Scientific Retractions 1927–2026; agrega grupos raros em Outros, exclui anos incompletos. retractions_time_to_retraction (redistribution_status=not_versioned, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** retractions_time_to_retraction: license_status=page_specific_unverified, redistribution_status=not_versioned.

**Situação posterior:** Sem decisão específica de permanência identificada entre os 14 conjuntos da issue #14 para esta unidade; permanecem os limites do inventário e da fonte. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Trajetória e contraste de contagens anuais de retratações por grupos. Sem denominador de publicações, não é taxa de retratação ou medida causal de integridade.

y ~ NB2(μ; φ) com efeitos hierárquicos e random walk de ordem 1, α ~ Normal(log_y_mean; 1,5); efeitos não centrados, escalas positivas Normal(0; 0,8), Normal(0; 0,3), Normal(0; 0,35); φ ~ Exponential(1).

Notação condensada editorial da especificação inspecionada; ponto e vírgula separa parâmetros, vírgula representa decimal. Normal usa média e desvio-padrão; Lognormal usa localização e escala no log; Exponential usa taxa, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Binomial negativa hierárquica com random walk de ordem 1 via rstan. Código identificado: retractions_trend_rstan.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/retractions_trend/retractions_trend_rstan.R`](../../scripts/retractions_trend/retractions_trend_rstan.R) — linhas 387–406; blob `55cee767b56a93b6339b6b9a8151f8a40bc32346`.


## 5. Diagnósticos e resultados

Curvas, contrastes e diagnósticos console-only previstos; síntese final não localizada. Exclusão de anos incompletos documentada.

**Natureza da saída descrita no catálogo:** Console de diagnósticos, contrastes e curvas posteriores, seção Arquivos e abertura; nenhum resultado analítico gravado por design.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

Base Kaggle não versionada; contagem de retratações não é taxa causal de integridade científica.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Fixar vintage, anos elegíveis e macrogrupos; avaliar exposição de publicação e mudanças de detecção/reporting, PPC/sensibilidade temporal.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
