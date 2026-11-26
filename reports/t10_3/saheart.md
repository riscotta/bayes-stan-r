# saheart — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Laboratório metodológico. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/saheart/README.md) preservado, blob `ed08118b20675530d7074db9a68ae42ac51b451c`.

## 1. Pergunta e objeto

Como usar regressão logística Bayesiana para o desfecho CHD em SAheart?

**Objeto documentado:** Resposta binária chd e preditores de risco no dataset SAheart (ESL).

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

Dataset demonstrativo SAheart do Elemental Statistics Learning, local ou download ESL; famhist binário e preditores opcionalmente padronizados. saheart (redistribution_status=not_versioned, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** saheart: license_status=not_identified, redistribution_status=not_versioned.

**Situação posterior:** Sem decisão específica de permanência identificada entre os 14 conjuntos da issue #14 para esta unidade; permanecem os limites do inventário e da fonte. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Probabilidade condicional de chd e contrastes log-odds por preditores; avaliação interna preditiva, não diagnóstico clínico validado.

y~Bernoulli_logit(α+Xβ); α~Normal(0; alpha_scale), β~Normal(0; beta_scale), escalas configuráveis. Stan inline.

Notação condensada editorial da especificação inspecionada; ponto e vírgula separa parâmetros, vírgula representa decimal. Normal usa média e desvio-padrão; Lognormal usa localização e escala no log; Exponential usa taxa, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Regressão Bernoulli-logit com priors Normal configuráveis, Stan inline/cmdstanr. Código identificado: saheart_logistic_cmdstanr.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/saheart/saheart_logistic_cmdstanr.R`](../../scripts/saheart/saheart_logistic_cmdstanr.R) — linhas 243–255; blob `61ac944b2fe929919c86b24830f738e2861deaa7`.
- [`scripts/saheart/README.md`](../../scripts/saheart/README.md) — linhas 69–94; blob `ed08118b20675530d7074db9a68ae42ac51b451c`.


## 5. Diagnósticos e resultados

HMC, PPC de prevalência, Brier, AUC e ECE descritos; nenhum valor atual aferido.

**Natureza da saída descrita no catálogo:** Console: sumários alpha/beta, HMC, PPC de prevalência e Brier, AUC por ranking e ECE por bins; seção O que você deve ver ao final.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

Download externo não equivale a licença; sem verificação de generalização clínica.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Fixar fonte/vintage e padronização, conferir desenho e avaliação fora da amostra, calibração por subgrupo; não generalizar uso clínico.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
