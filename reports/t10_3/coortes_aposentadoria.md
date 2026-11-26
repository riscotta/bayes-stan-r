# coortes_aposentadoria — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Aplicado com método/rota descritos; síntese final não localizada na triagem. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/coortes_aposentadoria/README.md) preservado, blob `97c14c49ceb7473b836576b65a8848270c7e83d6`.

## 1. Pergunta e objeto

Como a ordem dos retornos e o início da aposentadoria afetam patrimônio em janela fixa?

**Objeto documentado:** Coortes brasileiras de contribuição mensal real constante por 30 anos, diferenciadas pelo ano de aposentadoria e sequência de retornos.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

Séries mensais Ibovespa (Yahoo Finance Adj Close), Selic SGS 4390 e IPCA SGS 433; conversão para retornos reais; início padrão 1990, fim móvel. Sem vínculo individual no T06.1; identidade/vintage e termos específicos PENDENTE_DE_VERIFICACAO.

**Registro de direitos no catálogo (corte histórico, não licença nova):** Sem vínculo individual em T06.1/DATA_PROVENANCE: PENDENTE_DE_VERIFICACAO; fonte externa descrita no README não recebe licença presumida.

**Situação posterior:** Sem decisão específica de permanência identificada entre os 14 conjuntos da issue #14 para esta unidade; permanecem os limites do inventário e da fonte. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Patrimônio final real por estratégia e coorte histórica de 360 meses com aporte real comum; distribuição empírica entre janelas e risco de sequência. Não é uma posterior.

r_p,m = w_m r_eq,m + (1−w_m)r_rf,m; W₀=0 e W_m=(W_{m−1}+aporte_real)(1+r_p,m). Compara quantis empíricos e estratégias em janelas móveis; não há likelihood nem priors nesta recorrência.

Notação condensada editorial da especificação inspecionada; ponto e vírgula separa parâmetros, vírgula representa decimal. Normal usa média e desvio-padrão; Lognormal usa localização e escala no log; Exponential usa taxa, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Comparação histórica de janelas de 360 meses e estratégias de alocação; diagnóstico de sequência nos últimos anos. Código identificado: coortes_aposentadoria_console_only_v2.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/coortes_aposentadoria/coortes_aposentadoria_console_only_v2.R`](../../scripts/coortes_aposentadoria/coortes_aposentadoria_console_only_v2.R) — linhas 270–313, 471, 516–531; blob `d5bd92fc7fa047b6da4869fa0390734be0e14dea`.


## 5. Diagnósticos e resultados

Quantis de patrimônio, dispersão e comparação de coortes são saídas descritas; síntese final versionada não localizada. Janelas sobrepostas não são observações independentes.

**Natureza da saída descrita no catálogo:** Console: quantis, média, dispersão e melhores/piores coortes por estratégia; gráficos opcionais apenas na tela; README seção Saída.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

Fontes/vintages externos sem vínculo individual homologado no inventário; cenário ilustrativo não é previsão garantida.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Fixar série de preços ajustados, juros e inflação; checar retornos reais, alinhamento e custos/tributos não modelados; não extrapolar resultado histórico como retorno garantido.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
