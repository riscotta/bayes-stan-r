# kidney_survival — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Laboratório metodológico. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/kidney_survival/README.md) preservado, blob `76c147e6d7acd95623e22e4250d47f4d1849bba4`.

## 1. Pergunta e objeto

Como analisar tempo até evento com censura à direita e heterogeneidade por paciente?

**Objeto documentado:** Tempo até evento renal com censura à direita e medidas repetidas por paciente.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

Dataset demonstrativo kidney do pacote survival (McGilchrist & Aisbett, 1991); status convertido em indicador de censura. Sem vínculo individual no T06.1; identidade/vintage e termos específicos PENDENTE_DE_VERIFICACAO.

**Registro de direitos no catálogo (corte histórico, não licença nova):** Sem vínculo individual em T06.1/DATA_PROVENANCE: PENDENTE_DE_VERIFICACAO; fonte externa descrita no README não recebe licença presumida.

**Situação posterior:** Sem decisão específica de permanência identificada entre os 14 conjuntos da issue #14 para esta unidade; permanecem os limites do inventário e da fonte. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Distribuição de tempo até evento, probabilidades de sobrevivência e efeitos condicionais de idade, sexo, doença e heterogeneidade por paciente; censura à direita explícita.

Para evento observado, contribuição lognormal_lpdf(t|μ,σ); para censura, lognormal_lccdf(t|μ,σ). μ inclui Xβ e efeitos correlacionados de intercepto/inclinação por paciente. β ~ Normal(0,2,5); σ e τ ~ Exponential(1), L_Ω ~ LKJ_corr_cholesky(2).

Notação condensada editorial da especificação inspecionada; Normal usa média e desvio-padrão, Lognormal usa localização e escala no log, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Sobrevivência lognormal, age*sex+disease e intercepto/inclinação de idade correlacionados por paciente. Código identificado: kidney_lognormal_survival_cmdstanr.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/kidney_survival/kidney_lognormal_survival_cmdstanr.R`](../../scripts/kidney_survival/kidney_lognormal_survival_cmdstanr.R) — linhas 203–243; blob `bee1501c0298f236ac69d81868fd8efb6455a3f9`.


## 5. Diagnósticos e resultados

Prior predictive, HMC/NUTS e PPC numérico descritos; não há diagnóstico observado nesta tarefa.

**Natureza da saída descrita no catálogo:** Prior predictive de escala, diagnóstico HMC/NUTS e PPC numérico; README abertura; local de arquivo final não descrito, PENDENTE_DE_VERIFICACAO.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

Dataset externo do pacote survival; parametrização e diagnóstico requerem execução verificada.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Fixar versão do pacote survival, conferir codificação evento/censura, dependência intra-paciente, pressupostos de censura e sensibilidade AFT.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
