# T03.6 — Dicionário de rótulos inferenciais e nota de apresentação (PNAD Contínua, Estudo I)

**Finalidade:** evitar que nomes técnicos dos CSVs históricos E07–E09 sejam interpretados como probabilidades posteriores temporais ou de melhoria simultânea identificadas. Esta é uma **camada documental de apresentação**, não uma recodificação dos CSVs, dos resultados ou dos modelos. A nomenclatura original permanece preservada para rastreabilidade.

**Unidade de leitura:** cada contraste é definido por indicador, janela (`anual`: 202509→202609; `recente`: 202606→202609), direção favorável estabelecida no código e hipótese de correlação temporal. Há 13 indicadores e duas janelas, totalizando 26 pares descritivos; a E08.2 avalia quatro valores postulados de `rho = -0,5; 0; 0,5; 0,9` por par. Os valores históricos probabilísticos integrais não estão disponíveis no clone e **não foram reproduzidos**.

## 1. Correspondência entre campos técnicos e rótulos para leitura pública

| Origem e campo histórico (não renomear) | Rótulo recomendado | Significado admissível e limite obrigatório |
| --- | --- | --- |
| E07: resumos de `theta`, `y_rep` | Posterior **marginal** do nível em 202609 / réplica preditiva marginal | O modelo Stan histórico ajusta separadamente os 13 parâmetros no último período; não há draws de diferenças entre períodos. |
| E08.1: `ppc_cdf_obs` | Frequência de réplica preditiva `y_rep ≤ y` | Checagem preditiva marginal; não é probabilidade de melhora. |
| E08.2: `RhoHipotetico` | Correlação **temporal postulada** no cenário | Valores −0,5; 0; 0,5; 0,9 não são estimativas de covariância amostral. Em E09, `rho=0` é hipótese de covariância nula no **par**, não independência comprovada. |
| E08.2: `ProbDeltaPositivo` | Probabilidade de **Δ > 0 sob Normal condicional** | `Φ((y1−y0)/sΔ)` para `sΔ²=s0²+s1²−2rho*s0*s1`. Não é a probabilidade posterior obtida dos draws E07. |
| E08.2 e E09: `ProbSentido` | Probabilidade de sinal **favorável sob cenário gaussiano, rho fixo** | Seleciona `P(Δ>0)` ou `P(Δ<0)` conforme a direção favorável do indicador; exige informar janela, sinal, unidades e `rho`. Não equivale à chance empírica de melhora nem à classificação oficial do IBGE. |
| E08.2 e E09: `IC95Inf`, `IC95Sup` | Extremos da **faixa normal condicional nominal de 95%** para Δ | Calculados por `Δ ± 1,959963984540054*sΔ`; não foram demonstrados como intervalo credível posterior temporal ou intervalo de confiança com cobertura do desenho amostral. Evitar a abreviação pública **IC95** isolada. |
| E08.2: `InfProbSentidoAbre`, `SupProbSentidoAbre` | Limites algébricos de cenários com `rho→±1` | Não formam intervalo de inferência para correlação observada; o cálculo descreve casos-limite hipotéticos. |
| E08.3: `ProbSentido_NormMomentos` | Probabilidade **aproximada pela Normal ajustada aos momentos** sob prior e rho postulados | Quadratura determinística e aproximação de momentos; não equivale à integração exata da cauda nem a novos draws temporais MCMC. |
| E08.3: `ProbMinExtremos`, `ProbMaxExtremos` | Mínimo/máximo entre **16 vértices hipotéticos de arredondamento** | Extremos finitos avaliados em hipóteses de valor e CV; não se comprovou extremo global contínuo ou incerteza de arredondamento empiricamente calibrada. |
| E08.4: `MinProbCenariosRho`, `MaxProbCenariosRho` | Mínimo/máximo na **grade de cenários rho** | Extremos entre valores postulados, não quantis posteriores nem intervalo de confiança/credibilidade para uma probabilidade desconhecida. |
| E08.4: `LimiteInferiorFrechet`, `LimiteSuperiorFrechet` | Limites de Fréchet para **cinco eventos proxy** | Condicionais às probabilidades proxy de E08.2 sob mesmo rho temporal hipotético; não são a probabilidade conjunta identificada de que os cinco eixos substantivos melhoraram. |
| E09: `ProbMinCenariosRho`, `ProbMaxCenariosRho` | Mínimo/máximo de `ProbSentido` na **grade de quatro rho** | Agregação E08.2, não intervalo inferencial. `ProbSentido` da E09 usa especificamente `RhoHipotetico==0`, escolhido por construção. |
| Projeção pública E09: `Delta`, `SituacaoOficialIBGE` | Diferença **descritiva** de agregados arredondados / classificação informada pelo IBGE | A projeção de 26 linhas não contém probabilidades nem draws; deltas por subtração podem divergir dos deltas oficiais por arredondamento. |

## 2. Política mínima para títulos, legendas e conclusões

1. Identificar a procedência: **marginal E07**, **PPC E08.1**, **cenário Normal E08.2/E09**, **sensibilidade aproximada E08.3**, **proxy/limite E08.4** ou **descrição oficial IBGE**. Não usar simplesmente «probabilidade bayesiana» como rótulo comum.
2. Em qualquer apresentação de `ProbSentido` ou da faixa `IC95Inf/Sup`, exibir **indicador, janela, direção favorável, escala, `rho` postulado, método** e aviso de covariância temporal não observada. Na E09, explicitar **`rho=0` assumido**.
3. Para `ProbMinCenariosRho` e `ProbMaxCenariosRho`, escrever «extremos dos quatro cenários analisados»; não escrever «IC», «intervalo credível», «faixa de confiança» ou «posterior de rho».
4. Em E08.4, identificar explicitamente os **cinco proxies**, sem substituí-los verbalmente por todos os indicadores ou todos os eixos. A construção de Fréchet não estima dependência entre indicadores.
5. Tratar `SituacaoOficialIBGE` separadamente de probabilidades de cenário; significância/estabilidade oficial e eventos gaussianos hipotéticos não são critérios intercambiáveis.
6. Não inferir causalidade, melhoria simultânea, correlação temporal empírica nem posterior conjunta identificada. Suporte, erro de arredondamento, CV, desenho rotativo e ausência de covariâncias continuam limitações materiais.

**Exemplo prioritário com registro PNAD verificável:** «`desocupacao_pct`, Brasil, janela recente `202606→202609`: **5,4%→5,2%** (`Delta=-0,2` p.p.; `SituacaoOficialIBGE=caiu` no CSV descritivo E09). A subtração e a classificação são descritivas; o CSV público **não** contém `ProbSentido`, CVs nem covariâncias e não autoriza atribuir probabilidade de queda». Se uma probabilidade histórica de E08.2/E09 vier a ser apresentada em ambiente autorizado, a legenda **deve acrescentar** direção favorável `menor`, `rho` **postulado** (inclusive `rho=0`), método Normal condicional, fonte dos CVs e limites, **sem inventar valor numérico nem chamá-la de posterior temporal**.

**Complemento: modelo genérico de legenda (não é registro observado):** «Indicador [identificado], janela [identificada]: probabilidade de mudança no sentido favorável em cenário gaussiano condicional com `rho` postulado, usando CVs agregados; não é posterior temporal conjunta nem probabilidade oficial do IBGE». Os colchetes são campos a preencher somente com evidência disponível.

**Legenda inadmissível:** «Stan demonstrou com 95% de confiança que todos os eixos da PNAD melhoraram». O modelo E07 é marginal de 202609, E08.2/E09 não estimam covariâncias temporais, e o evento conjunto dos cinco eixos não foi identificado.

## 3. Decisão de escopo e pendências

Esta revisão não altera nomes internos, hashes ou conteúdo de `estudo_I/`, CSVs e projeções históricas; trata apenas a **interpretação editorial** dos campos. Não gera probabilidades novas, não reproduz E04–E09 e não comprova cobertura frequentista, credibilidade posterior ou identificação dos contrastes. Alteração futura do contrato físico de arquivos requer migração explícita e outro ciclo, com revisão independente.

**Governança:** Etapa 01/P0 (T01.3–T01.7; Issue #14) permanece aberta; dados institucionais e autorização não foram examinados nesta tarefa. O escopo inferencial de T03.7 (testes de invariantes e sintéticos) permanece separado. QA independente no SHA exato e CI são pré-condições para a integração de T03.6; a simples existência deste documento não constitui QA.

**Rastreabilidade:** [Nota científica](nota_cientifica.md); [T03.1](inventario_estimandos_e07_T03_1.md); [T03.2](derivacao_contrastes_gaussianos_T03_2.md); [T03.3](rastreabilidade_probabilidades_T03_3.md); [T03.4](auditoria_suporte_cv_arredondamento_T03_4.md); [T03.5](comparacao_alternativas_inferenciais_T03_5.md). Fontes preservados: `e07/R/ajustar_marginais_e07.R`, `e08/diagnostico_e081.py`, `e08/sensibilidade_e082.py`, `e08/sensibilidade_e083.py`, `e08/avaliacao_e084.py`, `e09/consolidar_e09.py`.
