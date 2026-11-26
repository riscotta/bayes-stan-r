# media_ruido_mu — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Laboratório metodológico. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/media_ruido_mu/README.md) preservado, blob `5b0d1c59a80d5a9417eb6bf395479bbb50d9fc52`.

## 1. Pergunta e objeto

Como amostra crescente reduz variância da média e incerteza posterior de mu num exemplo?

**Objeto documentado:** Concentração da média amostral e posterior de mu conforme N em experimento didático.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

Amostras sintéticas, mu_true, sigma_true, Ns e número de replicações configuráveis. Sem vínculo individual no T06.1; dados sintéticos, licença de insumo externo NAO_APLICAVEL para a simulação descrita.

**Registro de direitos no catálogo (corte histórico, não licença nova):** Sem vínculo individual em T06.1/DATA_PROVENANCE: PENDENTE_DE_VERIFICACAO; simulação sintética não pressupõe direitos sobre insumos externos.

**Situação posterior:** Dados sintéticos no escopo descrito; não há licença de insumo externo dessa simulação a homologar. Isso não decide aplicabilidade formal de E01 nem licença do software. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Média subjacente μ e largura do intervalo posterior em função de N; SD da média amostral para dados simulados iid.

y_i ~ Normal(μ; σ), μ ~ Normal(0; 10), σ ~ Exponential(1). No gerador iid, SD(ȳ)=σ_true/√N; regressão log-log da largura posterior confronta inclinação teórica aproximada −1/2.

Notação condensada editorial da especificação inspecionada; ponto e vírgula separa parâmetros, vírgula representa decimal. Normal usa média e desvio-padrão; Lognormal usa localização e escala no log; Exponential usa taxa, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Simulação de médias amostrais e ajuste Bayesiano Stan para mu em diferentes N. Código identificado: media_ruido_mu_cmdstanr.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/media_ruido_mu/media_ruido_mu_cmdstanr.R`](../../scripts/media_ruido_mu/media_ruido_mu_cmdstanr.R) — linhas 165–193, 242–246, 319–338; blob `cbeb7a15972b634a06afa99b8c05d999d3638650`.


## 5. Diagnósticos e resultados

CSV de resumo e gráficos previstos; inclinação estimada e cobertura não aferidas nesta tarefa.

**Natureza da saída descrita no catálogo:** PDF de gráficos e CSV de resumo da simulação e posterior, seção Como rodar; comportamento de concentração é objetivo didático, não resultado aferido neste ciclo.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

Experimento didático sintético; evidência não transfere para dados com dependência e modelo incorreto.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Conferir recuperação repetida, efeito de σ desconhecido, prior e N pequeno; não generalizar taxa N⁻¹/² a dependência/modelo incorreto.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
