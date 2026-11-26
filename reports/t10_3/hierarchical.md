# hierarchical — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Laboratório metodológico. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/hierarchical/README.md) preservado, blob `48e03852fdb4c4a7b1a3539d0ee52a23dcfb1d4b`.

## 1. Pergunta e objeto

Como partial pooling se comporta para batting average em três níveis?

**Objeto documentado:** Batting average no baseball: liga, posição e jogador; estimativas com pooling parcial.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

CSV demonstrativo BattingAverage; índices posição/jogador; origem primária e vintage não comprovadas. batting_average (redistribution_status=review_required, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** batting_average: license_status=not_identified, redistribution_status=review_required.

**Situação posterior:** A decisão de 26/11/2026 abrange `batting_average`: preservar cópias históricas com rastreabilidade parcial, apoiada na declaração do responsável. Não certifica vintage, licença específica, semântica ou autorização de release. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Probabilidade de acerto individual por jogador e distribuição por posição, com pooling parcial em três níveis.

y_i ~ Binomial(ab_i; logit⁻¹θ_i); hierarquia de localização e dispersão por posição, z_player ~ Normal(0; 1). μ_α ~ Normal(logit(0,25); 1), μ_logσ ~ Normal(log(0,5); 0,75), escalas de nível superiores Exponential(1).

Notação condensada editorial da especificação inspecionada; ponto e vírgula separa parâmetros, vírgula representa decimal. Normal usa média e desvio-padrão; Lognormal usa localização e escala no log; Exponential usa taxa, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Modelo Stan hierárquico em três níveis, pooling parcial, opção prior_only. Código identificado: baseball_batting_by_position_3level_cmdstanr.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/hierarchical/baseball_batting_by_position_3level_cmdstanr.R`](../../scripts/hierarchical/baseball_batting_by_position_3level_cmdstanr.R) — linhas 224–246; blob `7aabaedf358cbf300ddc1916b6022d519fa33ca4`.


## 5. Diagnósticos e resultados

Resumos/QC, possível LOO e gráficos são descritos. Nenhuma execução histórica individual reatestada.

**Natureza da saída descrita no catálogo:** Console com resumos e QC, possibilidade de LOO e gráficos conforme trecho habilitado; seção O que você deve ver ao final; sem gravação padrão.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

Compilação de BattingAverage sem licença homologada; não publicar a fonte nem atestar ajuste.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Identificar vintage e direitos do CSV, conferir sucessos/at-bats e pooling de posição, prior predictive e PPC de jogadores/posições com tamanhos diferentes.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
