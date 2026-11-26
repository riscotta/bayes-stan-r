# mortality — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Laboratório metodológico. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/mortality/README.md) preservado, blob `b9a32d700a6296275279c6e2bd910144852df5fd`.

## 1. Pergunta e objeto

Como estimar taxas municipais com suavização hierárquica e offset de população?

**Objeto documentado:** Taxa de eventos por município com exposição populacional; período e território do Excel não identificados.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

Excel com Contagem, POPULACAO e CODMUNRES; agrega municípios repetidos; origem do extrato pendente. mortality (redistribution_status=review_required, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** mortality: license_status=not_identified, redistribution_status=review_required.

**Situação posterior:** A decisão de 26/11/2026 abrange `mortality`: preservar cópias históricas com rastreabilidade parcial, apoiada na declaração do responsável. Não certifica vintage, licença específica, semântica ou autorização de release. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Taxa municipal de eventos com shrinkage e probabilidade de excesso frente ao benchmark do modelo; predição de contagens.

y_i ~ Poisson_log(logλ_i+log(pop_i)), efeito municipal não centrado η=ση_raw; α ~ Normal(alpha_loc; alpha_scale), σ positiva Normal(0; sigma_scale), η_raw ~ Normal(0; 1). O README informa centragem do intercepto na taxa bruta observada.

Notação condensada editorial da especificação inspecionada; ponto e vírgula separa parâmetros, vírgula representa decimal. Normal usa média e desvio-padrão; Lognormal usa localização e escala no log; Exponential usa taxa, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Poisson hierárquico com log(pop) como offset, efeito municipal não centrado e prior de alpha centrado na taxa bruta. Código identificado: mortality_poisson_offset_cmdstanr_v2.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/mortality/mortality_poisson_offset_cmdstanr_v2.R`](../../scripts/mortality/mortality_poisson_offset_cmdstanr_v2.R) — linhas 225–245; blob `be0a1705ae923c6cd7ba8925d3e07d3ed587530c`.
- [`scripts/mortality/README.md`](../../scripts/mortality/README.md) — linhas 3–17; blob `b9a32d700a6296275279c6e2bd910144852df5fd`.


## 5. Diagnósticos e resultados

HMC, PPC de zeros/total, PPC municipal e flags de excesso descritos; nenhum ranking ou ajuste aferido nesta tarefa.

**Natureza da saída descrita no catálogo:** Console com benchmark posterior de excesso, diagnósticos, PPC municipal/global, rankings e gráficos no device; seção Saídas no console; sem reproduzir rankings.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

Planilha de mortalidade de origem/termos não identificados; não afirmar inferência reexecutada.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Identificar origem da planilha, população, período e regra de soma de municípios repetidos; verificar overdispersion, influência da prior ancorada nos dados e incerteza de excesso.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
