# T04.4 — Matriz de rastreabilidade E06 → E07: matemática, Stan e R

**Natureza:** `RECONSTRUCAO_EDITORIAL`, não E06 contemporâneo, execução Stan nem homologação empírica. **Tarefa literal:** “Produzir ponte E06 → E07 linha a linha, com correspondência matemática–Stan–R e testes simples”. **Commit-base inspecionado:** `85b6946244d82b170d4fdd9175539f7f0d797b57`, 09/11/2026. Contrato normativo: [T02.4](../../scripts/STUDY_E06_E07_MATRIX_T02_4.md). Este mapa não modifica fontes históricos.

## 1. Fontes inspecionadas no mesmo SHA

Localizadores `T:10`, `C:11`, `M:9`, `A:38`, `F:14`, `P:22` e `S:20` significam linha 10 de taxa, linha 11 de contagem, linha 9 de rendimento, linha 38 do runner, linha 14 de funções, linha 22 do preflight e linha 20 do reprocessamento. Todas são **linhas físicas 1-based** na referência acima. Os hashes listados são hashes *blob* Git SHA-1.

| Alias | Fonte e linha total | Blob SHA-1 |
| --- | --- | --- |
| E06 | [E06 reconstruído T04.3](e06_especificacao_modelos_T04_3.md), 1–118 | `772b94db51d72d1c8412749c5339569f92a4233c` |
| T | [taxa_marginal.stan](../../scripts/pnadc_estudo_i/estudo_I/e07/stan/taxa_marginal.stan), 1–16 | `1e323a6d037b9347c776def214db6586c49a3118` |
| C | [contagem_marginal.stan](../../scripts/pnadc_estudo_i/estudo_I/e07/stan/contagem_marginal.stan), 1–17 | `54c90cb5f0ce2c56704a3ccec01dbe9f6caa902d` |
| M | [rendimento_marginal.stan](../../scripts/pnadc_estudo_i/estudo_I/e07/stan/rendimento_marginal.stan), 1–16 | `e867b4c9f5cf7ea144c41b1a33a8c88acbab5672` |
| A | [ajustar_marginais_e07.R](../../scripts/pnadc_estudo_i/estudo_I/e07/R/ajustar_marginais_e07.R), 1–95 | `25f13970abdd062bafa2e8488aa6376248f29c98` |
| F | [funcoes_e07.R](../../scripts/pnadc_estudo_i/estudo_I/e07/R/funcoes_e07.R), 1–41 | `c88d57f74a097353e6e10a713ed7a5306ed4ca50` |
| P | [preflight_e07.R](../../scripts/pnadc_estudo_i/estudo_I/e07/R/preflight_e07.R), 1–54 | `82997ca1cae83599382b43930f7774b3ac20e819` |
| S | [reprocessar_resumos_e07.R](../../scripts/pnadc_estudo_i/estudo_I/e07/R/reprocessar_resumos_e07.R), 1–28 | `c055746b0c35b33b3bd65db40d3f3280e9c54c1c` |
| DIC | [Dicionário T04.2](contratos_e01_e05_T04_2.md), `3 | `3e87c809d8e454f212d5f401a52ccdb2f64d9b62` |

Confrontadas também as auditorias [T03.1](inventario_estimandos_e07_T03_1.md), [T03.2](derivacao_contrastes_gaussianos_T03_2.md), [T03.3](rastreabilidade_probabilidades_T03_3.md), [T03.4](auditoria_suporte_cv_arredondamento_T03_4.md), [T03.5](comparacao_alternativas_inferenciais_T03_5.md), [T03.6](rotulos_inferenciais_T03_6.md) e [T03.7](invariantes_sinteticos_T03_7.md), além do [QA T04.3](https://github.com/riscotta/bayes-stan-r/pull/39#pullrequestreview-5828746554). Ausência de E06 original e de artefatos de execução está registrada em E06/T03.1.

## 2. Matriz por ocorrência matemática/instrução

Cada linha identifica uma **ocorrência verificável**, sem condensar prior, likelihood, parâmetros ou quantidades geradas. Os três Stan têm blocos `data`, `parameters`, `model` e `generated quantities`; não têm `transformed data` nem `transformed parameters`. `CONFORME_CODIGO` significa inspeção estática e prova matemática local, **nunca execução comprovada**. `DIVERGENCIA` é validação operacional diferente entre preflight e runner, não prior ou likelihood alterada.

| ID | Classe | E06: referência/equação | Stan: bloco, linhas e símbolo | R: localizador | Dimensão, unidade, suporte e transformação | Evidência/limite | Estado |
| --- | --- | --- | --- | --- | --- | --- | --- |
| G01 | DADO_INDICE | E06:12–17, y e CV | T:2–3; C:2–4; M:2–3 | A:14–15,26–40 | Um agregado por ajuste, s em unidade de y | código apenas | CONFORME_CODIGO |
| G02 | EQUACAO | E06:16,24–28; s=abs(y)CV/100 | T:3; C:3; M:3 | A:36–40 | CV em %, s é desvio-padrão, s>0 no runner | guarda do R | CONFORME_CODIGO |
| G03 | DADO_INDICE | E06:6,19; t=202609, 13 principais | Stan escalares | A:14–17,26–32; F:14–16 | 8 taxas/4 totais/1 renda, P14 apoio | DIC + seleção | CONFORME_CODIGO |
| G04 | DADO_INDICE | E06:19; ID-período único, CV publicado | três entradas escalares | F:6–21; P:12–29 | Estados e cinco eixos A–E | somente guardas declaradas | CONFORME_CODIGO |
| G05 | EQUACAO | E06:23–28,47–54; posterior ∝ likelihood × prior | T:8–11; C:9–12; M:8–11 | A:60–75 | Posteriores condicionais individuais, sem termos cruzados | código e álgebra | CONFORME_CODIGO |
| G06 | DADO_INDICE | E06:19,111; E04 local | Stan não lê CSV | F:2–5; A:11–14 | Insumo externo E04 não versionado | falta execução e vintage | ABERTA |
| T01 | DADO_INDICE | E06:14–16,34; y em p.p. | T:2 | A:36,40 | y escalar real, estimativa agregada | leitura | CONFORME_CODIGO |
| T02 | DADO_INDICE | E06:16; s operacional | T:3, lower=0 | A:38–39 | s em p.p., zero excluído pelo runner | guarda anterior à likelihood | CONFORME_CODIGO |
| T03 | PARAMETRO | E06:34; p ∈ [0,1] | T:5–7 | A:30–35,63–64 | p escalar latente; não proporção individual observada | suporte | CONFORME_CODIGO |
| T04 | PRIOR | E06:34,38–40; p~Beta(1,1) | T:8–9 | A:53–55, rótulo | theta=100p induz Uniform(0,100) com Jacobiano 1/100 | prior e derivação | CONFORME_CODIGO |
| T05 | TRANSFORMACAO_RESTRICAO | E06:34,57–60; theta=100p | T:12–13, GQ | A:78–80 | theta em p.p., intervalo [0,100] | determinística | CONFORME_CODIGO |
| T06 | LIKELIHOOD | E06:24–28,34; y~N(100p,s²) | T:10 | A:36–41,63–64 | Normal recebe s (SD), y real; runner exige 0≤y≤100 | não Binomial | CONFORME_CODIGO |
| T07 | GENERATED_QUANTITY | E06:57–62; y_rep~N(theta,s²) | T:14 | A:78–80 | réplica não truncada, pode sair [0,100]; resumo só theta | não houve PPC | CONFORME_CODIGO |
| T08 | DADO_INDICE | E06:6,34; oito taxas | T:1–16 | A:30–35 | oito IDs _pct, sem índice entre modelos | DIC | CONFORME_CODIGO |
| C01 | DADO_INDICE | E06:14–17,35; y mil pessoas | C:2 | A:36,40,42–48 | agregado contínuo, não contagem individual | leitura | CONFORME_CODIGO |
| C02 | DADO_INDICE | E06:16; s | C:3 | A:37–40 | SD em milhares, runner s>0 | sem covariância | CONFORME_CODIGO |
| C03 | DADO_INDICE | E06:17,35; P14 fixo | C:4 | A:43–47 | população 14+ em milhares, >0 no runner | um apoio no período | CONFORME_CODIGO |
| C04 | PARAMETRO | E06:35; q ∈ [0,1] | C:6–8 | A:30–35,63–64 | q escalar distinto por total, não probabilidade de pessoa | suporte | CONFORME_CODIGO |
| C05 | PRIOR | E06:35,41; q~Beta(1,1) | C:9–10 | A:53–56 | theta/P=q induz Uniform(0,P), Jacobiano 1/P | P fixo | CONFORME_CODIGO |
| C06 | TRANSFORMACAO_RESTRICAO | E06:35,57–60; theta=P*q | C:13–14 | A:43–47,78–80 | theta∈[0,P], mil pessoas; P sem posterior | GQ | CONFORME_CODIGO |
| C07 | LIKELIHOOD | E06:24–28,35; y~N(Pq,s²) | C:11 | A:36–48,63–64 | Normal de agregado, não Poisson ou Binomial | R→Stan | CONFORME_CODIGO |
| C08 | GENERATED_QUANTITY | E06:57–62; y_rep~N(theta,s²) | C:15 | A:78–80 | pode ser <0 ou >P, sem truncamento | sem PPC observado | CONFORME_CODIGO |
| C09 | DADO_INDICE | E06:6,35; quatro contagens | C:1–17 | A:30–35,42–48 | quatro IDs _mil; P14 apoio compartilhado fixo | DIC | CONFORME_CODIGO |
| C10 | DADO_INDICE | E06:19; y≤P14 | C:2,4 | A:45–47; P:22–25 | runner não testa y<0 na contagem; preflight rejeita y<0 | D2-O1 | DIVERGENCIA |
| R01 | DADO_INDICE | E06:14–16,36; y em R$ | M:2 | A:30–40 | média agregada em reais | leitura | CONFORME_CODIGO |
| R02 | DADO_INDICE | E06:16; s=abs(y)CV/100 | M:3 | A:37–40 | SD em R$ | escala, sem estimar s | CONFORME_CODIGO |
| R03 | PARAMETRO | E06:36,42; mu≥0 | M:5–7 | A:30–35,63–64 | mu escalar, real não negativo | suporte | CONFORME_CODIGO |
| R04 | PRIOR | E06:36,42; mu~N(0,10000) restrita | M:8–9 | A:53–55, rótulo | meia-Normal SD 10000; densidade 2*dnorm(mu,0,10000) para mu≥0 | normalização induzida | CONFORME_CODIGO |
| R05 | TRANSFORMACAO_RESTRICAO | E06:36,57–60; theta=mu | M:12–13 | A:78–80 | theta em R$, identidade | GQ | CONFORME_CODIGO |
| R06 | LIKELIHOOD | E06:24–28,36; y~N(mu,s²) | M:10 | A:36–40,63–64 | média de agregado, não rendimentos individuais | leitura | CONFORME_CODIGO |
| R07 | GENERATED_QUANTITY | E06:57–62; y_rep~N(mu,s²) | M:14 | A:78–80 | réplica pode ser negativa; não truncada | sem PPC observado | CONFORME_CODIGO |
| R08 | DADO_INDICE | E06:6,36; único ID rendimento | M:1–16 | A:30–35 | rendimento_real_habitual_rs, eixo E | DIC | CONFORME_CODIGO |
| R09 | DADO_INDICE | E06:19,42; observação y | M:2,6 | A:36–40; P:22–25 | runner não testa y<0 no rendimento, preflight sim | D2-O2 | DIVERGENCIA |
| X01 | EQUACAO | E06:47–54; posteriors individuais | três blocos model | A:26–27,63–75 | 13 análises marginais, não joint draws | código | CONFORME_CODIGO |
| X02 | DADO_INDICE | E06:6,19; smoke/completo | três modelos | A:7–17,63–74 | smoke 1 indicador; completo 13 selecionados | código, não execução | CONFORME_CODIGO |
| X03 | EQUACAO | E06:62,68–83; sem Delta temporal | nenhum Stan contém t0/t1 ou rho | A:14,23,26–27,81 | apenas theta_i,202609; sem correlação estimada | fronteira E08 | CONFORME_CODIGO |
| X04 | GENERATED_QUANTITY | E06:57–62; theta e y_rep | T:12–15; C:13–16; M:12–15 | A:78–80; S:19–23 | R resume theta; reprocessa CSV existente sem novo MCMC | sem PPC validado | CONFORME_CODIGO |
| X05 | DADO_INDICE | E06:111; diagnóstico/saídas externos | nenhuma saída auditada | A:76–91; S:14–27 | fit.rds/chains/resumos não disponíveis neste ciclo | D4, sem execução | ABERTA |
| X06 | DADO_INDICE | E06:111; checagem ambiente | sem Stan executado no preflight | P:31–53; F:23–35 | preflight não compila nem faz MCMC | P:53 explícito | CONFORME_CODIGO |
| X07 | EQUACAO | E06:86–94; rho E08.2 postulado | sem rho nos Stan | A:14,23,78–80 | rho não é parâmetro E07; quatro cenários externos | fora E07 | NAO_APLICAVEL |
| X08 | EQUACAO | E06:96–105; alternativas A–D | B/C/D não codificadas | A:33–35,63–75 | opções de pesquisa não implementadas | fora E07 | NAO_APLICAVEL |

## 3. Conferência nominal de todos os 13 ajustes

| Família | IndicadoresId e eixos | Quantidade e suporte |
| --- | --- | --- |
| Taxas | desocupacao_pct (A); nivel_ocupacao_pct (B); participacao_pct (B); subutilizacao_pct (C); subocupacao_horas_pct (C); desalento_pct (C); informalidade_pct (D); contrib_previdencia_pct (D) | 8 × theta∈[0,100] p.p. |
| Contagens | desocupados_mil (A); ocupados_mil (B); forca_trabalho_mil (B); desalentados_mil (C) | 4 × theta∈[0,P14] mil |
| Rendimento | rendimento_real_habitual_rs (E) | 1 × theta≥0 R$ |
| Apoio não ajustado | populacao_14mais_mil (B) | Entrada fixa para 4 totais, não 14º ajuste |

## 4. Derivação e testes com oráculos independentes

Para prior Beta(1,1), com theta=L*p e L>0 fixo, a transformação de densidade exige `f_theta(t)=f_p(t/L)/L=1/L` em [0,L]. Testar L=100 e L=180000 separadamente demonstra a escala da taxa e da contagem, inclusive intervalos de probabilidade 0,25 e 0,5 sob integração numérica. Para a meia-Normal restrita, a densidade é `2*dnorm(mu,0,10000)` **somente para mu≥0** e integra 1 em [0,∞).

Para `theta` uniforme em [a,b] e likelihood `y|theta,s~Normal(theta,s²)`, a posterior é Normal(y,s²) **truncada no parâmetro**. Sua CDF fechada em `a≤c≤b` é `[Phi((c-y)/s)-Phi((a-y)/s)]/[Phi((b-y)/s)-Phi((a-y)/s)]`. A suíte T04.4 confronta a CDF analítica com integração numérica **da likelihood em theta**, em cenários interiores e adversos, incluindo y<0, sem simular PNAD. Confere também massa preditiva Normal fora do suporte latente; `y_rep` não foi truncado no Stan.

Arquivo executável: [test-pnadc-ponte-e06-e07-t04-4.R](../../tests/testthat/test-pnadc-ponte-e06-e07-t04-4.R). O teste verifica sequência completa das instruções Stan (remoção de comentários), dicionário dos 13 IDs + apoio, runner/preflight, fórmulas e domínios. **Não compila Stan**, não reexecuta E04/E07, não estima CVs nem obtém probabilidades históricas. A CI no SHA exato do PR será registrada após seu término.

## 5. Divergências, lacunas e fronteiras de afirmação

**D2-O1 (C10):** R-P:22–25 rejeita observação negativa em qualquer indicador; R-A:42–48, chamado diretamente sem preflight, não possui `y<0` para contagens, apenas `y>P`. O Stan declara `y` real e theta não negativo. Não há evidência de contagens negativas históricas. Diferença de validação, não demonstra alteração inferencial.

**D2-O2 (R09):** R-P:22–25 também rejeita rendimento observado negativo; R-A:36–40 não o rejeita explicitamente para o ramo de rendimento quando s>0. `mu≥0` não restringe `y`. A assimetria não autoriza remendar o código neste escopo documental.

**Desvio `s=0`:** Stan declara `real<lower=0> s`, cuja fronteira inclui zero, mas Normal requer SD estritamente positivo; R-A:39 barra s≤0 antes do ajuste. Não inferir que a density Normal seria definida em s=0.

**D4 – execução indisponível:** entrada E04 integral, CVs numéricos históricos, saídas das 52 cadeias, diagnósticos ESS/R-hat, PPC, covariâncias amostrais e metadados de execução não foram verificados. Preflight declara explicitamente não compilar nem fazer MCMC; reprocessador necessita CSVs já existentes. Nenhuma linha recebe estado `CONFORME_EXECUCAO`.

**Identificação:** os Stan não incluem t0/t1, `rho`, matrizes de covariância, efeitos entre indicadores, prior temporal ou `Delta`. Os 26 contrastes E09 são descritivos; E08.2 usa `rho=-0,5, 0, 0,5, 0,9` como hipótese de cenário, não correlação observada. A repetição do destino 202609 impede tratar os dois deltas como independentes por construção.

**Governança:** PR desta T04.4 exigirá **revisão independente em outro chat** sobre HEAD e CI finais antes de qualquer merge. T04.4 Exec. pendente de CI / QA ☐; T04.5–T04.7 ☐/☐, Etapa 04 ABERTA. P0 T01.3–T01.7 ☐/☐, Issue #14 e Issues #15–#16 ABERTAS, D00-01–D00-06 abertas. Dados, priors, likelihoods, inferências e resultados históricos preservados.
