# deter_mensal_bioma_uf — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Aplicado com método/rota descritos; síntese final não localizada na triagem. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/deter_mensal_bioma_uf/README.md) preservado, blob `9bbbf7c2e8d9c14089039bf1116af5c133869b79`.

## 1. Pergunta e objeto

Como se comparam ciclos de alertas DETER entre biomas, UFs e meses?

**Objeto documentado:** Área de alertas DETER por bioma-UF e mês, agosto a janeiro, ciclos 2024/25 e 2025/26 como opção documentada.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

CSV mensal de alertas DETER, área em km², bioma, UF, ano, mês e ciclo reconstruível; fonte exata do extrato não aferida. deter_mensal_bioma_uf (redistribution_status=not_versioned, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** deter_mensal_bioma_uf: license_status=not_identified, redistribution_status=not_versioned.

**Situação posterior:** Sem decisão específica de permanência identificada entre os 14 conjuntos da issue #14 para esta unidade; permanecem os limites do inventário e da fonte. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Contraste de ciclo global β_cycle e contraste específico por bioma::UF, na escala log1p da área de alertas; efeitos mensais e predição de área sob transformação explicitada.

y=log(1+área km²); y_i ~ Normal(μ_i,σ), μ_i=α+a_estrato+m_mês+(β_cycle+b_estrato)I_ciclo. Efeitos não centrados, efeito mensal centrado; β_cycle ~ Normal(0,1), escalas ~ Exponential(1); intercepto prior centrado em estatísticas dos dados. Recorte agosto–janeiro, exatamente dois ciclos.

Notação condensada editorial da especificação inspecionada; Normal usa média e desvio-padrão, Lognormal usa localização e escala no log, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Modelo hierárquico de efeitos global de ciclo, por estrato e mensal; prior predictive e PPC. Código identificado: deter_mensal_bioma_uf_cmdstanr.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/deter_mensal_bioma_uf/deter_mensal_bioma_uf_cmdstanr.R`](../../scripts/deter_mensal_bioma_uf/deter_mensal_bioma_uf_cmdstanr.R) — linhas 390–465, 466–508; blob `1bd2bbb8bd9509f4fb6df01dd6b81027e6cddd28`.


## 5. Diagnósticos e resultados

Prior predictive, PIT, cobertura e PPC descritos. Não localizada síntese final versionada no recorte; alerta DETER não equivale a desmatamento confirmado.

**Natureza da saída descrita no catálogo:** Console com diagnóstico, cobertura preditiva e PIT; relatório TXT e gráficos PPC em PDF opcionais, README seção Arquivos.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

Extrato DETER não versionado e sem endpoint exato/licença homologados; alerta não equivale a desmatamento confirmado.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Conferir extrato/vintage e consistência de células ausentes/zeros, comparabilidade dos ciclos, interpretação na escala original e sensibilidade do intercepto dependente dos dados.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
