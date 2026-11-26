# inhaler_ordinal — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Laboratório metodológico. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/inhaler_ordinal/README.md) preservado, blob `5b9cc172d38d9767bb31511e7be17a8a09e90372`.

## 1. Pergunta e objeto

Como especificar respostas ordinais num desenho crossover de tratamentos e períodos?

**Objeto documentado:** Resposta ordinal em crossover 2×2 por sujeito, dois tratamentos e dois períodos.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

Dataset inhaler do brms ou CSV de mesmas colunas; holdout documentado; vintage não aferida. Sem vínculo individual no T06.1; identidade/vintage e termos específicos PENDENTE_DE_VERIFICACAO.

**Registro de direitos no catálogo (corte histórico, não licença nova):** Sem vínculo individual em T06.1/DATA_PROVENANCE: PENDENTE_DE_VERIFICACAO; fonte externa descrita no README não recebe licença presumida.

**Situação posterior:** Sem decisão específica de permanência identificada entre os 14 conjuntos da issue #14 para esta unidade; permanecem os limites do inventário e da fonte. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Probabilidades de resposta ordinal e contrastes condicionais aos tratamentos/períodos e sujeito no crossover; desempenho preditivo no holdout documentado.

y_tr ~ OrderedLogistic(η_tr; c), β ~ Normal(0; 1,5), intercepto aleatório z_u ~ Normal(0; 1), σ_u ~ Student-t(3; 0; 0,7) positiva; cutpoints construídos por c1 e gaps softplus. O código computa log-likelihood de teste.

Notação condensada editorial da especificação inspecionada; ponto e vírgula separa parâmetros, vírgula representa decimal. Normal usa média e desvio-padrão; Lognormal usa localização e escala no log; Exponential usa taxa, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Ordered logistic, intercepto aleatório por sujeito e cutpoints com gaps softplus. Código identificado: inhaler_ordinal_softplus_cmdstanr.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/inhaler_ordinal/inhaler_ordinal_softplus_cmdstanr.R`](../../scripts/inhaler_ordinal/inhaler_ordinal_softplus_cmdstanr.R) — linhas 208–250, 359–386; blob `aea6b6c6fd410b74cb35940fac1a9f7f199169ce`.


## 5. Diagnósticos e resultados

PIT randomizado, Dunn–Smyth, calibração por thresholds e holdout são saídas descritas. Nenhum desempenho atual verificado.

**Natureza da saída descrita no catálogo:** PIT randomizado, Dunn–Smyth, calibração por thresholds e métricas de holdout; PDF de gráficos e TXT quando habilitados, seção Saídas.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

Dados externos do pacote brms ou CSV alternativo; fonte e compatibilidade de insumos a conferir.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Conferir versão/fonte, sequência e carryover do crossover, divisão train/test por sujeito, interpretação de holdout e calibração ordinal.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
