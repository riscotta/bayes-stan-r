# deck_15wins — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Laboratório metodológico. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/deck_15wins/README.md) preservado, blob `39631212e9123f3d10a9c24bf7f003cc26ee3f60`.

## 1. Pergunta e objeto

Como a inferência da chance de vitória por deck compara Stan e posterior analítica em sessões simuladas?

**Objeto documentado:** Probabilidade de vitória por deck, derrotas antes de r vitórias e total de partidas por sessão simulada.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

Sintéticos, gerados no script; r, sessões por deck e p_true configuráveis; nenhuma amostra real. Sem vínculo individual no T06.1; dados sintéticos, licença de insumo externo NAO_APLICAVEL para a simulação descrita.

**Registro de direitos no catálogo (corte histórico, não licença nova):** Sem vínculo individual em T06.1/DATA_PROVENANCE: PENDENTE_DE_VERIFICACAO; simulação sintética não pressupõe direitos sobre insumos externos.

**Situação posterior:** Dados sintéticos no escopo descrito; não há licença de insumo externo dessa simulação a homologar. Isso não decide aplicabilidade formal de E01 nem licença do software. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Probabilidade de vitória p_d por deck simulado; número de partidas N=r+perdas por sessão. Comparação posterior analítica versus Stan.

p_d ~ Beta(a0; b0), L_s | p_d ~ NegBin(r; p_d), perdas antes de r vitórias. Em Stan, beta_nb=p/(1−p) na parametrização shape/inverse scale; posterior conjugada Beta(a0+rS_d; b0+ΣL_s).

Notação condensada editorial da especificação inspecionada; ponto e vírgula separa parâmetros, vírgula representa decimal. Normal usa média e desvio-padrão; Lognormal usa localização e escala no log; Exponential usa taxa, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Prior Beta, binomial negativa para derrotas até r sucessos e posterior conjugada comparada a Stan/cmdstanr. Código identificado: deck_15wins_negbin_beta_cmdstanr.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/deck_15wins/README.md`](../../scripts/deck_15wins/README.md) — linhas 8–24; blob `39631212e9123f3d10a9c24bf7f003cc26ee3f60`.
- [`scripts/deck_15wins/deck_15wins_negbin_beta_cmdstanr.R`](../../scripts/deck_15wins/deck_15wins_negbin_beta_cmdstanr.R) — linhas 245–254, 469–481; blob `19eb699d73ac884344c122351cd51b5657a81e48`.


## 5. Diagnósticos e resultados

Recuperação de p_true, diferenças entre média/mediana Stan e analítica, HMC e PPC da média N por deck são controles descritos. Nenhum novo valor produzido nesta tarefa. Publicação do Deck não demonstra desempenho em partidas reais.

**Natureza da saída descrita no catálogo:** Console: recuperação de p_true por IC, delta_mean/delta_q50 Stan versus posterior analítica, divergências, treedepth, Rhat, ESS, E-BFMI e PPC da média de N por deck; README seção Modelo e exemplo T02.2. Valores observados não aferidos.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

Dados sintéticos: demonstração metodológica, sem extrapolação para partidas reais.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Validar equivalência com posterior conjugada sob tolerância pré-definida, recuperação repetida e PPC; separar publicação de software e inferência empírica.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
