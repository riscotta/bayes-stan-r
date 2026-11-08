# T03.3 — Trilha de origem das probabilidades E08/E09 (PNAD Contínua, Estudo I)

**Natureza:** auditoria estática dos fontes versionados em \`main\` \`bd7a67be1ab92917f365149d7c23d05894b0f86f\` (08/11/2026). **Não** reexecuta E04–E09, não recupera amostras históricas, não estima correlações, não valida o desenho amostral e não homologa os dados. Os scripts históricos são preservados. A tarefa responde exclusivamente à T03.3: distinguir probabilidades provenientes de draws posteriores marginais de probabilidades decorrentes de cenários e aproximações.

## 1. Critério de classificação e caminho de cálculo

- **Draw posterior marginal:** estatística computada diretamente sobre amostras de \`theta\` ou \`y_rep\` produzidas por E07 em **202609**, sem par temporal. Presença de draws não transforma uma estatística preditiva marginal em probabilidade posterior de melhoria.
- **Cenário condicional gaussiano:** probabilidade de sinal de um contraste calculada por \`norm.cdf\` a partir de estimativas publicadas, CVs, direção favorável e \`rho\` hipotético. **Não** deriva de draws de diferenças E07 nem de modelo temporal conjunto.
- **Integração/propagação aproximada:** momentos obtidos por quadratura determinística para priors alternativas e aproximação normal aos momentos, ou envelopes sobre hipóteses de arredondamento e dependência. Não são amostras MCMC temporais.
- **Limite derivado:** desigualdade de Fréchet de eventos proxy condicionais; **não** probabilidade de todos os cinco eixos melhorarem, nem posterior multivariada estimada.
- **Consolidação:** seleção e transformação de resultados de E08 sem inferência nova. **Descrição oficial:** classificação de significância do IBGE ou delta de agregados arredondados, não probabilidade bayesiana.

## 2. Mapa por variável/arquivo de saída

| Etapa / saída (prevista em diretório local histórico) | Campo probabilístico ou estatístico | Origem computacional efetiva no fonte | Natureza permitida / restrição |
| --- | --- | --- | --- |
| E07 / \`resumo_theta.csv\`, \`resumo_marginais.csv\` | \`theta\`: \`mean\`, \`sd\`, quantis e diagnósticos da summary | \`ajustar_marginais_e07.R\`: \`fit$summary(variables = 'theta')\`, modelos marginais separados de 202609 | Resumos de **draws posteriores marginais**, não \`Delta\` nem evento entre períodos. CSVs não disponíveis no clone. |
| E07 / \`chains_*.csv\` | \`theta\` e \`y_rep\` | Stan de taxa, contagem e rendimento, \`generated quantities\` | Amostras marginais e réplicas preditivas em 202609; **sem pares temporais**. Cadeias não versionadas. |
| E08.1 / \`diagnosticos_e081.csv\` | \`ppc_cdf_obs\` | \`diagnostico_e081.py\`: \`(yrep<=y).mean()\` com \`y_rep\` de CSVs das cadeias | Frequência preditiva empírica **sobre draws de \`y_rep\` de E07**; não \`P(Delta>0)\`. \`rhat_rank\`, \`ess_*\`, \`mcse_mean\`, \`ebfmi_*\` são diagnósticos, não probabilidades de melhora. |
| E08.2 / \`cenarios_contrastes_e082.csv\` | \`ProbDeltaPositivo\` | \`sensibilidade_e082.py\`: \`norm.cdf(delta/sd)\` com \`delta=y1-y0\`, \`sd=sqrt(s0²+s1²-2*rho*s0*s1)\` | **Cenário gaussiano condicional** para cada par indicador–janela–\`rho\`; nenhum draw de contraste é consumido. |
| E08.2 / mesma saída | \`ProbSentido\` | \`ProbDeltaPositivo\` se maior é favorável; \`1-ProbDeltaPositivo\` se menor é favorável | Probabilidade de direção **condicionada ao cenário**, não posterior temporal identificada. |
| E08.2 / mesma saída | \`IC95Inf\`, \`IC95Sup\` | \`delta ± 1.959963984540054*sd\` | Faixa normal **condicional**; nomes \`IC95*\` históricos **não comprovam** credibilidade posterior/cobertura do desenho. |
| E08.2 / \`limites_temporais_e082.csv\` | \`InfProbSentidoAbre\`, \`SupProbSentidoAbre\` | \`norm.cdf(k*delta/s)\` nos extremos algébricos \`|s1-s0|\` e \`s1+s0\`, com convenção degenerada para \`s≈0\` | Limites em **\`rho→±1\` hipotético**; não intervalo inferencial para correlação estimada. |
| E08.2 / \`checagem_marginais_e082.csv\` | \`MediaE07\`, \`DP_E07\` e razões | Lê \`diagnosticos_e081.csv\` de E08.1 para cotejo com estimativas/CV de E04 | **Checagem marginal**, não usa draws para gerar cenários de contraste. |
| E08.3 / \`priors_marginais_e083.csv\` | \`MediaPosteriorAnalitica\`, \`DPPosteriorAnalitico\` | \`sensibilidade_e083.py\`: quadratura Gauss–Hermite 101 nós, pesos por prior \`logprior\` | Momentos **aproximados numericamente** sob priors alternativas; não cadeias E07 novas nem probabilidade temporal. |
| E08.3 / \`sensibilidade_priors_contrastes_e083.csv\` | \`ProbSentido_NormMomentos\` | \`contrast_posterior\` calcula momentos bivariados via GH sob \`rho\` e prior; \`norm.cdf(sign*mu/sd)\` | **Aproximação normal aos momentos da distribuição construída pela quadratura**; não integração exata da cauda, nem draws MCMC, nem covariância estimada. Termo “posterior” no código é condicionado ao modelo hipotético. |
| E08.3 / mesma saída | \`ProbE082Base\` | Cópia de \`cenarios_contrastes_e082.csv:ProbSentido\` | Referência ao cenário E08.2, não evidência independente. |
| E08.3 / \`sensibilidade_arredondamento_e083.csv\` | \`ProbNominal\`, \`ProbMinExtremos\`, \`ProbMaxExtremos\`, \`ProbMinApenasValor\`, \`ProbMaxApenasValor\`, \`ProbMinApenasCV\`, \`ProbMaxApenasCV\` | \`ProbNominal\` de E08.2; os demais campos são \`min/max\` de avaliações normais nos **16 cantos** de perturbações hipotéticas de valores e CV | **Envelopes finitos de vértices**, não limites globais garantidos nem distribuição do arredondamento. |
| E08.3 / \`quadro_eixos_e083.csv\` | \`ProbNominal\`, \`ProbMin*\`, \`ProbMax*\`, \`ProbAdversarial\`, amplitudes | Seleção \`recente\`, \`rho=0\`, consolidação dos cenários de arredondamento e priors | Agregação de cenários; sem posterior conjunta dos eixos. |
| E08.4 / \`sintese_eixos_e084.csv\` | \`MinProbCenariosRho\`, \`MaxProbCenariosRho\`, \`Min/MaxProbCenariosRhoComArredondamento\`, \`AmplitudeProbPriorsRegulares_pp\` | \`avaliacao_e084.py\`: mínimos, máximos e amplitudes sobre cenários E08.2/E08.3 | Extremos **condicionais** de uma grade discreta hipotética, não intervalos credíveis ou de confiança de \`rho\`. |
| E08.4 / \`limites_conjuntos_proxy_e084.csv\` | \`LimiteInferiorFrechet\`, \`LimiteSuperiorFrechet\` | Cinco \`ProbSentido\` de E08.2 (\`desocupacao_pct\`, \`nivel_ocupacao_pct\`, \`subutilizacao_pct\`, \`informalidade_pct\`, \`rendimento_real_habitual_rs\`): \`max(0,sum(p)-4)\`, \`min(p)\` | Limites algébricos da interseção de **cinco eventos proxy**, condicionados ao mesmo \`rho\` temporal hipotético; dependência entre indicadores não identificada. **Não** \`P(todos os indicadores/eixos melhoraram | dados)\`. |
| E09 / \`contrastes_e09_consolidado.csv\` | \`ProbSentido\`, \`ProbMinCenariosRho\`, \`ProbMaxCenariosRho\` | \`consolidar_e09.py\`: filtra E08.2 em \`RhoHipotetico==0\` e calcula \`min/max\` de \`ProbSentido\` nos quatro \`rho\` | **Reapresentação de E08.2** (26 linhas); \`rho=0\` é hipótese, não estimação. Nenhuma amostragem E09. |
| E09 / mesma saída | \`SituacaoOficialIBGE\`, \`DeltaOficialIBGE\` | Merge à esquerda de \`confronto_quadro_e084.csv\` | Situação de teste de significância e delta publicado, **não** probabilidade bayesiana. |
| Projeção pública E09 / \`data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv\` | \`Delta\`, \`SituacaoOficialIBGE\` | Projeção congelada de **26 diferenças descritivas**, ver \`auditar_e10.py\` | **Não contém campos probabilísticos** nem amostras MCMC. Não confundir com CSV E09 integral ausente do GitHub. |

**Crítica de evidência:** as linhas acima descrevem expressões e arquivos *previstos no código*, não valores recalculados dos CSVs históricos. A ausência dos insumos E04/E08 e cadeias E07 no clone impede checar a consistência numérica dos 104/416 cenários e 26 linhas consolidadas. \`E08.3\` introduz uma construção bivariada **condicional a \`rho\` e priors postulados**, não identifica dependência amostral real. A existência de \`y_rep\` só sustenta a classificação de PPC marginal em E08.1.

## 3. Cobertura de cada afirmação temporal da E08/E09

Os mesmos **13 IDs** se cruzam com **duas janelas** (\`anual\`: 202509→202609; \`recente\`: 202606→202609), totalizando 26 unidades. Para **cada** linha, \`E08.2:ProbSentido\` possui quatro cenários (\`rho=-0.5,0,0.5,0.9\`), e \`E09:ProbSentido\` seleciona o cenário \`rho=0\` do mesmo par; \`ProbMinCenariosRho\` e \`ProbMaxCenariosRho\` correspondem aos extremos **daqueles quatro cenários**, não a posterior sobre \`rho\`.

| Eixo | Identificadores (cada um com anual + recente) | Pares |
| --- | --- | ---: |
| A | \`desocupacao_pct\`, \`desocupados_mil\` | 4 |
| B | \`forca_trabalho_mil\`, \`nivel_ocupacao_pct\`, \`ocupados_mil\`, \`participacao_pct\` | 8 |
| C | \`desalentados_mil\`, \`desalento_pct\`, \`subocupacao_horas_pct\`, \`subutilizacao_pct\` | 8 |
| D | \`contrib_previdencia_pct\`, \`informalidade_pct\` | 4 |
| E | \`rendimento_real_habitual_rs\` | 2 |
| **Total** | **13 indicadores distintos** | **26** |

**Cadeia de atribuição por par:** \`E04:Valor,CVPercentual\` → \`E08.2:delta,s0,s1,rho,ProbDeltaPositivo,ProbSentido\` → \`E09:ProbSentido(rho=0),ProbMin/MaxCenariosRho\`. A \`E07\` entra **apenas** como checagem marginal de \`theta\` em 202609 via E08.1; \`E08.3/4\` estudam sensitividades/limites distintos, não substituem essa proveniência. O CSV público permite conferir 26 IDs/janelas/deltas, mas **não** verificar seus valores probabilísticos.

## 4. Consequência interpretativa e gates

1. **Permitido:** “Sob a aproximação gaussiana com CVs publicados e \`rho=0\` assumido, o cenário E08.2/E09 atribui determinada probabilidade de movimento no sentido indicado.” É necessário citar indicador, janela, direção, método e \`rho\`; os números não foram verificados neste ciclo.
2. **Não demonstrado:** “A posterior Stan E07 atribuiu probabilidade \`p\` à melhora de 202509 para 202609”, “\`rho\` foi estimado”, “o IBGE informou probabilidade bayesiana”, ou “probabilidade de que todos os cinco eixos melhoraram”.
3. **Pendente T03.4:** suporte, CV e arredondamento, sobreposição trimestral, desenho amostral e covariâncias ausentes. **Pendente T03.5:** comparação e identificação de alternativas. **Pendente T03.6:** correção de rótulos/nota e interpretação pública. **Pendente T03.7:** testes adicionais de invariantes; o teste T03.3 verifica somente a origem estrutural e relações matemáticas sintéticas.
4. **P0 não resolvida:** T01.3–T01.7 e Issue #14. Não se inspecionou, transcreveu, criou nem publicou dado restrito. Não houve refit, alteração de Stan, E04–E09, estimandos, resultados ou parâmetros históricos.

**Gate T03.3:** confrontar todos os campos acima aos fontes e contratos de arquivos, validar testes e CI no HEAD do PR, registrar eventual divergência, obter parecer de QA **independente** da execução; marcar QA e integrar **somente depois**. A CI, mesmo verde, não valida empiricamente as probabilidades de E08/E09.
