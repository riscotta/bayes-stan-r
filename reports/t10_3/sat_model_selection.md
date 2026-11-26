# sat_model_selection — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Laboratório metodológico. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/sat_model_selection/README.md) preservado, blob `05c8db1d877b896ab070ce8538dfd8299e37d19e`.

## 1. Pergunta e objeto

Como comparar subconjuntos de preditores do exemplo SAT por evidência e previsão?

**Objeto documentado:** Comparação de subconjuntos dos quatro preditores clássicos da base SAT; resposta e preditores padronizados.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

Dataset demonstrativo mosaicData/SAT via Rdatasets; vintage e termos específicos não aferidos. sat (redistribution_status=not_versioned, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** sat: license_status=not_identified, redistribution_status=not_versioned.

**Situação posterior:** Sem decisão específica de permanência identificada entre os 14 conjuntos da issue #14 para esta unidade; permanecem os limites do inventário e da fonte. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Probabilidades posteriores de modelos e inclusão, coeficientes BMA e desempenho preditivo relativo; distinguir evidência marginal e LOO.

Modelos Gaussianos y~Normal(α+X_Mβ; σ); modelo nulo y~Normal(α; σ). α~Normal(0; 1,5), β~Normal(0; 1), σ~Exponential(1). Espaço de 2⁴ modelos e probabilidade de inclusão a priori configurável; bridge sampling e LOO têm alvos distintos.

Notação condensada editorial da especificação inspecionada; ponto e vírgula separa parâmetros, vírgula representa decimal. Normal usa média e desvio-padrão; Lognormal usa localização e escala no log; Exponential usa taxa, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Comparação de todos os subconjuntos, bridge sampling, PSIS-LOO e Bayesian Model Averaging via rstan. Código identificado: sat_stan_model_selection_rstan.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/sat_model_selection/sat_stan_model_selection_rstan.R`](../../scripts/sat_model_selection/sat_stan_model_selection_rstan.R) — linhas 366–407; blob `e76625fb7f56d3298a611c27f59b1fe9797f0596`.
- [`scripts/sat_model_selection/README.md`](../../scripts/sat_model_selection/README.md) — linhas 9–12, 57–81; blob `05c8db1d877b896ab070ce8538dfd8299e37d19e`.


## 5. Diagnósticos e resultados

HMC/PPC por modelo, Bayes factors, inclusão, BMA e comparação LOO são saídas descritas. Nenhum resultado de seleção aferido neste ciclo.

**Natureza da saída descrita no catálogo:** Console: diagnóstico por modelo, PPC, evidência marginal/Bayes factors, LOO, inclusão posterior, coeficientes BMA e melhor modelo por critério; seção O que o script entrega.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

Base Rdatasets externa sem termos verificados; sensibilidade de seleção e suposições não auditadas.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Fixar dataset, escala, prior de inclusão e parâmetros; revisar estabilidade da evidência marginal, PSIS-LOO e sensibilidade das probabilidades.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
