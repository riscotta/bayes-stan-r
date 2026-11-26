# pms_servicos — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Aplicado com método/rota descritos; síntese final não localizada na triagem. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/pms_servicos/README.md) preservado, blob `90fb414151788c508c1b80a5091eb6715c0d84e6`.

## 1. Pergunta e objeto

Como interpretar a variação na Pesquisa Mensal de Serviços no período delimitado?

**Objeto documentado:** Série Brasil/Total de serviços, índice ajustado sazonalmente, manchete janeiro de 2026.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

Base analítica consolidada PMS, CSV pms_base_analitica_stan, derivado versionado; vintage original não homologada. pms_servicos (redistribution_status=review_required, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** pms_servicos: license_status=public_source_no_explicit_dataset_license, redistribution_status=review_required.

**Situação posterior:** A decisão de 26/11/2026 abrange `pms_servicos`: preservar cópias históricas com rastreabilidade parcial, apoiada na declaração do responsável. Não certifica vintage, licença específica, semântica ou autorização de release. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Nível latente e tendência da série de serviços; consistência aritmética da manchete e contraste temporal, não causalidade.

y ~ Normal(level_std; σ_obs), nível ancorado e tendência AR(1), φ ~ Beta(3; 2); priors de nível inicial/tendência usam estatísticas observadas (y[1],dy0,dy_mean), escalas positivas configuráveis.

Notação condensada editorial da especificação inspecionada; ponto e vírgula separa parâmetros, vírgula representa decimal. Normal usa média e desvio-padrão; Lognormal usa localização e escala no log; Exponential usa taxa, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Verificação aritmética da manchete e modelo de nível ancorado + tendência AR(1) via rstan. Código identificado: pms_servicos_rstan.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/pms_servicos/pms_servicos_rstan.R`](../../scripts/pms_servicos/pms_servicos_rstan.R) — linhas 424–433, 532; blob `8ad34413255fdc94558fe71a5560244aa736f4b4`.


## 5. Diagnósticos e resultados

MCMC, PPC RMSE e relatório console-only previstos; não localizada síntese final versionada. Não inferir melhora apenas pela execução da rotina.

**Natureza da saída descrita no catálogo:** Console com diagnóstico MCMC e relatório de consistência/manchete; README abertura e Observações; nenhum artefato final padrão.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

Extrato analítico SIDRA versionado não equivale a fonte bruta; licença/vintage não homologada.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Congelar extrato PMS e regra de ajuste sazonal; conferir escala, janelas, efeito de revisão de dados e priors ancoradas; auditar relatório.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
