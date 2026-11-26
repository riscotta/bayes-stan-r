# importacoes_world_bank — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Aplicado com método/rota descritos; síntese final não localizada na triagem. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/importacoes_world_bank/README.md) preservado, blob `7d24ad2763ee505da3f5e08831f9b1b0c1bda088`.

## 1. Pergunta e objeto

Como modelar importações anuais por país com heterogeneidade e caudas?

**Objeto documentado:** Importações anuais por país; heterogeneidade entre países; período exato não explicitado no README.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

CSV World Bank Import USD enriquecido; natureza observacional, colunas enriquecidas com proveniência incompleta. importacoes_world_bank (redistribution_status=review_required, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** importacoes_world_bank: license_status=partial_verified, redistribution_status=review_required.

**Situação posterior:** A decisão de 26/11/2026 abrange `importacoes_world_bank`: preservar cópias históricas com rastreabilidade parcial, apoiada na declaração do responsável. Não certifica vintage, licença específica, semântica ou autorização de release. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Nível/contrastes de importações na escala analítica do modelo, com interceptos e inclinações por país. Não identificar efeito causal de covariáveis observacionais.

y ~ Student-t(ν; μ; σ), μ=Xβ com intercepto/inclinação por país não centrados. β₁ ~ Normal(y_mean; 2,5), demais Normal(0; 1); escalas de efeitos de país 1 e 0,2 em normais positivas; ν−2 ~ Exponential(0,1), σ positiva Normal(0; 1). A transformação e unidade do desfecho devem ser verificadas no pipeline E04 antes de back-transformar.

Notação condensada editorial da especificação inspecionada; ponto e vírgula separa parâmetros, vírgula representa decimal. Normal usa média e desvio-padrão; Lognormal usa localização e escala no log; Exponential usa taxa, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Modelo principal hierárquico Student-t (modelo_principal_hierarquico_student_t.stan), preparação, EDA, ajuste e diagnóstico. Código identificado: Script.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/importacoes_world_bank/modelo_principal_hierarquico_student_t.stan`](../../scripts/importacoes_world_bank/modelo_principal_hierarquico_student_t.stan) — linhas 38–65; blob `b9545e515c6b8b7b3e0d8a3a34739d177926bafa`.


## 5. Diagnósticos e resultados

Tabelas E04/E05/E07/E08 e diagnósticos/figuras descritos; síntese final rastreável não localizada. Sem ajuste nesta tarefa.

**Natureza da saída descrita no catálogo:** Tabelas E04/E05/E07/E08, figuras E05/E07/E08 e CSV CmdStan, seção Saídas geradas; sem RDS padrão; estimativas não transcritas.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

Colunas enriquecidas têm proveniência incompleta; CC BY da fonte não cobre automaticamente o derivado local.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Identificar indicador, moeda/deflator e proveniência das colunas enriquecidas; revisar escala, painel, dependência e sensibilidade das priors.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
