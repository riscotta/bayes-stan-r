# retractions_reason_composition — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Aplicado com método/rota descritos; síntese final não localizada na triagem. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/retractions_reason_composition/README.md) preservado, blob `712695bc4404aa03b5617dcebad0ecaf94bddf50`.

## 1. Pergunta e objeto

Como se compõem os grupos de motivos de retratação ao longo de categorias editoriais?

**Objeto documentado:** Composição de macrogrupos de motivos de retratação por editora, área, país e período; série 1927–2026.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

CSV externo Kaggle Global Scientific Retractions 1927–2026, motivos multi-rótulo expandidos e taxonomia de macrogrupos. retractions_time_to_retraction (redistribution_status=not_versioned, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** retractions_time_to_retraction: license_status=page_specific_unverified, redistribution_status=not_versioned.

**Situação posterior:** Sem decisão específica de permanência identificada entre os 14 conjuntos da issue #14 para esta unidade; permanecem os limites do inventário e da fonte. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Composição condicional dos macrogrupos de motivos de retratação e contrastes temporais; unidade é motivo codificado, com multi-rótulos, não prevalência causal de fraude.

y_n ~ DirichletMultinomial(conc_n); efeitos hierárquicos de editora, área, país e período, α ~ Normal(0,1), escalas positivas com priors Normal(0,0,35) (período 0,25), φ ~ Lognormal(log(20),0,5). Implementação explícita da log-pmf no Stan.

Notação condensada editorial da especificação inspecionada; Normal usa média e desvio-padrão, Lognormal usa localização e escala no log, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Dirichlet-multinomial hierárquico com pooling parcial; regra de parada para tokens não mapeados por padrão. Código identificado: retractions_reason_composition_rstan.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/retractions_reason_composition/retractions_reason_composition_rstan.R`](../../scripts/retractions_reason_composition/retractions_reason_composition_rstan.R) — linhas 670, 760–790, 817; blob `1372e05a852e6938f3e5947e695939acfbe58670`.
- [`scripts/retractions_reason_composition/README.md`](../../scripts/retractions_reason_composition/README.md) — linhas 46, 85; blob `712695bc4404aa03b5617dcebad0ecaf94bddf50`.


## 5. Diagnósticos e resultados

PPC e composição posterior/contrastes previstos; síntese final não localizada. Script para por padrão em tokens não mapeados.

**Natureza da saída descrita no catálogo:** Console: diagnósticos, PPC, composições posteriores e contrastes temporais; seção Arquivos e abertura; sem gravação padrão.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

CSV Kaggle externo sem licença verificada; codificação e seleção editorial não medem fraude causalmente.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Fixar vintage e taxonomia, validar expansão multi-rótulo e denominador, sensibilidade a tokens descartados, heterogeneidade e seleção de registros.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
