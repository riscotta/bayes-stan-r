# ipca_sidra_1419 — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Aplicado com método/rota descritos; síntese final não localizada na triagem. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/ipca_sidra_1419/README.md) preservado, blob `2ed6412c4f81ed957244a782a46d09407c9e1c63`.

## 1. Pergunta e objeto

Como analisar a evolução mensal e a desaceleração do IPCA com séries oficiais?

**Objeto documentado:** IPCA mensal brasileiro, headline e nove grupos; janela padrão documentada 202001–202602.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

Consultas online SIDRA/IBGE tabelas 1737 e 7060, pesos e acumulado de 12 meses; vintage não aferida. Sem vínculo individual no T06.1; identidade/vintage e termos específicos PENDENTE_DE_VERIFICACAO.

**Registro de direitos no catálogo (corte histórico, não licença nova):** Sem vínculo individual em T06.1/DATA_PROVENANCE: PENDENTE_DE_VERIFICACAO; fonte externa descrita no README não recebe licença presumida.

**Situação posterior:** Sem decisão específica de permanência identificada entre os 14 conjuntos da issue #14 para esta unidade; permanecem os limites do inventário e da fonte. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Trajetória latente da inflação mensal por grupo e total, questões probabilísticas de desaceleração e consistência do acumulado de 12 meses; distinguir agregado ponderado de efeito causal.

y[t,g] ~ Normal(θ[t,g]; σ_obs,g), y_total[t] ~ Normal(total_θ[t]; σ_obs_total). Modelo temporal com μ0 ~ Normal(0,4; 0,5), inovação não centrada, efeitos de grupo e parâmetros de persistência; escalas observacionais Exponential(3) e Exponential(4). O modelo calcula log-likelihood total.

Notação condensada editorial da especificação inspecionada; ponto e vírgula separa parâmetros, vírgula representa decimal. Normal usa média e desvio-padrão; Lognormal usa localização e escala no log; Exponential usa taxa, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Validação aritmética/cobertura e modelo temporal hierárquico Stan/cmdstanr. Código identificado: ipca_sidra_1419_cmdstanr.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/ipca_sidra_1419/ipca_sidra_1419_cmdstanr.R`](../../scripts/ipca_sidra_1419/ipca_sidra_1419_cmdstanr.R) — linhas 750–777; blob `d13efec25b7b1afdc3b5ad1abdf5fbe655049d2b`.
- [`scripts/ipca_sidra_1419/README.md`](../../scripts/ipca_sidra_1419/README.md) — linhas 76; blob `2ed6412c4f81ed957244a782a46d09407c9e1c63`.


## 5. Diagnósticos e resultados

Relatório de probabilidades e verificação aritmética/cobertura descritos; síntese final versionada não localizada. Consulta online não reproduz vintage automaticamente.

**Natureza da saída descrita no catálogo:** Relatório de perguntas probabilísticas sobre desaceleração e consistência do acumulado; console, TXT/CSV opcionais, seção Arquivos.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

Vintages e tabelas oficiais dependem de consulta on-line; coleta integral e condições de uso não auditadas.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Congelar consultas SIDRA 1737/7060, pesos e calendário; validar recomposição acumulada, escala e sensibilidade temporal; conferir saídas e diagnósticos antes de publicar probabilidade.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
