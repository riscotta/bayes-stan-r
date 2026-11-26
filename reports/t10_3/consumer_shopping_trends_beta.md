# consumer_shopping_trends_beta — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Laboratório metodológico. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/consumer_shopping_trends_beta/README.md) preservado, blob `6627d311bbf5d9af4a11ddd30ad67330487c6bd0`.

## 1. Pergunta e objeto

Como descrever a variável de gasto em um exemplo Bayesiano Beta?

**Objeto documentado:** Gasto online médio (alternativa: gasto em loja) no catálogo Consumer Shopping Trends; unidade registro do CSV.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

CSV externo Kaggle Consumer Shopping Trends Analysis, alvo reescalado a (0,1); arquivo não versionado, vintage exata não aferida. consumer_shopping_trends (redistribution_status=not_versioned, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** consumer_shopping_trends: license_status=page_specific_unverified, redistribution_status=not_versioned.

**Situação posterior:** Sem decisão específica de permanência identificada entre os 14 conjuntos da issue #14 para esta unidade; permanecem os limites do inventário e da fonte. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Parâmetros de forma α e β da distribuição da variável reescalada; média da Beta α/(α+β) na escala transformada, e média de y na escala original se aplicada a transformação registrada.

z_i ~ Beta(α,β), α,β ~ Lognormal(0,1), suporte aberto (0,1). O script contém o modelo Stan em memória; não é regressão causal.

Notação condensada editorial da especificação inspecionada; Normal usa média e desvio-padrão, Lognormal usa localização e escala no log, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Beta(alpha,beta), priors lognormais e Stan em memória via rstan; validação de suporte do alvo. Código identificado: consumer_shopping_trends_beta_rstan.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/consumer_shopping_trends_beta/consumer_shopping_trends_beta_rstan.R`](../../scripts/consumer_shopping_trends_beta/consumer_shopping_trends_beta_rstan.R) — linhas 462–464, 518–522, 673–698; blob `448d564a1d12b0faf9056ad0e98fec16289269e2`.


## 5. Diagnósticos e resultados

Sumários e PPC da distribuição/quantis previstos pelo script; nenhum valor posterior aferido nesta T10.3.

**Natureza da saída descrita no catálogo:** Sumários posteriores, diagnósticos MCMC, PPC e gráficos comparativos; CSV/TXT, PNG e RDS descritos na seção Arquivos.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

CSV Kaggle não versionado; licença específica e adequação do suporte Beta não homologadas.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Registrar vintage e licença Kaggle, transformação e tratamento de extremos; verificar adequação Beta e efeito da transformação no estimando e PPC.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
