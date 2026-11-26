# hidrometro_degradacao — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Laboratório metodológico. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/hidrometro_degradacao/README.md) preservado, blob `c773cc4a753d3e16b81eb2420e65ec51ef767374`.

## 1. Pergunta e objeto

Como erro de medição de hidrômetros varia com idade, volume, tipo e região em simulação?

**Objeto documentado:** Erro percentual de aferição por tipo de hidrômetro, idade, log do volume acumulado e região/setor; simulação.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

Dataset sintético gerado no script, dimensões de aferições, tipos, regiões e idade máxima configuráveis. Sem vínculo individual no T06.1; dados sintéticos, licença de insumo externo NAO_APLICAVEL para a simulação descrita.

**Registro de direitos no catálogo (corte histórico, não licença nova):** Sem vínculo individual em T06.1/DATA_PROVENANCE: PENDENTE_DE_VERIFICACAO; simulação sintética não pressupõe direitos sobre insumos externos.

**Situação posterior:** Dados sintéticos no escopo descrito; não há licença de insumo externo dessa simulação a homologar. Isso não decide aplicabilidade formal de E01 nem licença do software. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Erro percentual de medição esperado em função de idade, log(volume), tipo e região em dados simulados.

y_i ~ Normal(μ_i; σ_y), μ inclui intercepto de modelo, efeito de região, idade e log-volume. α0 ~ Normal(0; 5), coeficientes Normal(0; 1); efeitos não centrados e escalas Exponential(1).

Notação condensada editorial da especificação inspecionada; ponto e vírgula separa parâmetros, vírgula representa decimal. Normal usa média e desvio-padrão; Lognormal usa localização e escala no log; Exponential usa taxa, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Modelo hierárquico com interceptos de tipo e região, idade e log(volume); HMC/NUTS. Código identificado: hidrometro_degradacao_cmdstanr.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/hidrometro_degradacao/hidrometro_degradacao_cmdstanr.R`](../../scripts/hidrometro_degradacao/hidrometro_degradacao_cmdstanr.R) — linhas 295–311, 588–682; blob `645ffe5b1e933156fef4f5b76799704575543c91`.


## 5. Diagnósticos e resultados

PPC/PIT, resumos e gráficos previstos; nenhum valor observado neste ciclo. A simulação não valida degradação real.

**Natureza da saída descrita no catálogo:** Resumo no console; PDF de gráficos e TXT opcionais, incluindo PPC/PIT conforme opções; seção Saídas.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

Pipeline ilustrativo simulado não valida degradação observacional de equipamentos reais.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Conferir gerador, recuperação de parâmetros, identificação de idade/volume e calibração; aplicação real exigiria amostra e processo de aferição documentados.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
