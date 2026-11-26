# fruitflies_aft — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Laboratório metodológico. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/fruitflies_aft/README.md) preservado, blob `3dac347171dfc11a582f88810f03c4796900d130`.

## 1. Pergunta e objeto

Como aplicar um modelo de sobrevivência AFT log-normal ao exemplo FruitFlies?

**Objeto documentado:** Tempo de vida de moscas no experimento FruitFlies, tratamento e tórax padronizado.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

Dataset demonstrativo Stat2Data::FruitFlies; variável sleep lida, excluída por potencial pós-tratamento. Sem vínculo individual no T06.1; identidade/vintage e termos específicos PENDENTE_DE_VERIFICACAO.

**Registro de direitos no catálogo (corte histórico, não licença nova):** Sem vínculo individual em T06.1/DATA_PROVENANCE: PENDENTE_DE_VERIFICACAO; fonte externa descrita no README não recebe licença presumida.

**Situação posterior:** Sem decisão específica de permanência identificada entre os 14 conjuntos da issue #14 para esta unidade; permanecem os limites do inventário e da fonte. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Contrastes de tempo até evento na escala log e razão de tempos por tratamento e covariáveis; médias ajustadas são saídas descritas.

log(y_i) ~ Normal(x_iᵀβ; σ), β ~ Normal(0; 1,5), σ ~ Lognormal(log(0,3); 0,5). O fator exp(Δxᵀβ) representa razão de tempos na escala AFT sob modelo comum. Sleep excluída por potencial pós-tratamento.

Notação condensada editorial da especificação inspecionada; ponto e vírgula separa parâmetros, vírgula representa decimal. Normal usa média e desvio-padrão; Lognormal usa localização e escala no log; Exponential usa taxa, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

AFT log-normal parametrizado em log(y), matriz de tratamento e tórax; Stan/cmdstanr. Código identificado: fruitflies_aft_lognormal_cmdstanr.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/fruitflies_aft/fruitflies_aft_lognormal_cmdstanr.R`](../../scripts/fruitflies_aft/fruitflies_aft_lognormal_cmdstanr.R) — linhas 102–121, 225–238; blob `c41863def322c79cf4dfc8c50be2aee0a90bcd1d`.
- [`scripts/fruitflies_aft/README.md`](../../scripts/fruitflies_aft/README.md) — linhas 68–69; blob `3dac347171dfc11a582f88810f03c4796900d130`.


## 5. Diagnósticos e resultados

HMC/NUTS, PPC e médias ajustadas previstos; nenhum valor atual aferido. O dataset do pacote é didático.

**Natureza da saída descrita no catálogo:** Console com HMC/NUTS e resumos posteriores; PPC e gráfico de médias ajustadas; PDF/TXT opcionais, seção Arquivos.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

Dados do pacote externo Stat2Data e termos de reutilização não verificados; sleep potencialmente pós-tratamento, excluída do modelo principal.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Fixar versão do pacote, checar desenho e matriz X, adequação lognormal e interpretação de tratamento sem condicionar indevidamente variável pós-tratamento.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
