# E08_MP — Aditivo metodológico prospectivo, **proposto e não congelado**

**Data:** 19 de outubro de 2026. **Natureza:** aditivo da E08 após exploração de todos os 2.084 cães. **Status atual:** MP0 auditado; MP1 pendente de aprovação explícita; MP2–MP4 não executados. **Não é pré-registro externo**, não é E09 e não altera E06/E07.

## 1. Escopo, evidência já observada e pergunta

**Pergunta histórica imutável:** a raça pura declarada acrescenta capacidade preditiva fora da amostra para o escore F5 além das características individuais? **Nova pergunta:** uma família condicional assimétrica melhora a distribuição preditiva do F5 sem comprometer a comparabilidade M1/M2?

Evidências existentes (somente históricas): n=2.084, 196 raças; média F5=0,156536, DP=0,937889, assimetria de Fisher=-0,663463, min=-3,0668, max=2,5096. No cenário principal M2 Student-t(5), violações q05/q95 são 8,277% e 1,775%, com cobertura central próxima a 90%; Normal S1 M2: 7,606% e 2,519%. RMSE histórico M1=0,935531, M2=0,922602, Δ=0,012929. Estas evidências motivam a hipótese, mas não autorizam atribuir a assimetria **marginal** diretamente à distribuição **condicional** nem atribuir as falhas somente à verossimilhança.

O F5 é `factor_scores.score.norm` (fator 5) publicado, maior = polo *diffident*. Não há demonstração de limite teórico do escore. Extremos observados não são limites da escala. Candidatas com suporte limitado/truncado ficam **bloqueadas por MP0** até nova documentação de mensuração. O desfecho não representa mordida, agressão clínica ou causalidade genética.

## 2. Unidade, população, estimandos e agregação

- Unidade: **cão**, identificado por `dog_id`; cães de mesmo tutor podem ser dependentes, mas `tutor_id` inexiste. População-alvo é restrita a registros com critérios E04 semelhantes aos da base; não população canina geral.
- Predição pontual de cada cão: `E[Y_i|D_treino,x_i]`, integrada sobre a posterior; para M2 raça inédita, também sobre efeito de raça nova.
- Estimando histórico principal preservado: `Δ_RMSE = RMSE_OOF(M1)-RMSE_OOF(M2)` **dentro de uma mesma família**; positivo favorece raça. Referência M0: média **somente do treino**.
- Calibração por cauda: `P_OOF(Y<q_0,05)` e `P_OOF(Y>q_0,95)`, meta nominal 0,05 em cada; diferenças e bootstrap por cão. Reportar também cobertura 50/90, largura 90 e intervalo de Wilson/intervalo bootstrap como descritivos.
- Scores de distribuições **completas**: `logscore_i = log[∫ f(y_i|θ,x_i)p(θ|D_tr)dθ]` (maior melhor); `IS90_i = (q95-q05)+20(q05-y_i)1[y_i<q05]+20(y_i-q95)1[y_i>q95]` (menor melhor). Quantis são da mistura preditiva, **não** média de quantis por draw.
- RMSE, MAE, logscore e IS90, sempre na escala original do F5. Com repetições, médias de métricas calculadas primeiro por repetição, não pseudoamostra de cães repetidos; nesta versão **propõem-se cinco folds novos e uma repetição** para limitar custo, sem alegar independência nova.

## 3. Família, covariáveis e priors: especificações propostas

Defina para o cão i o vetor fixo E06 `x_i=(male,size_0,size_1,size_3,size_4,sterilized_no,age_z,age_z²)`, com sexo referência *female*, tamanho referência 2, castração referência *yes* e `age_z=(idade-media_treino)/dp_treino`. M1: `η_i = α + x_iᵀβ`. M2: `η_i = α + x_iᵀβ + u_{r(i)}`, `u_j = τ z_j`, `z_j~Normal(0,1)`. Raças e índices são definidos **exclusivamente no treino**.

**C0a — Student-t histórico:** `Y_i|θ~t_5(η_i,σ)`. Priors E06 congeladas: `α~N(0,1,5²)`, `β_k~N(0,0,5²)`, `σ~HalfNormal(0,1²)`; em M2 `τ~HalfNormal(0,0,5²), z_j~N(0,1)`. Média condicional Student-t=η (ν=5). As cópias Stan E07 são literais e não serão modificadas. Para comparar nos novos folds são necessários **novos ajustes**, que não serão chamados de resultados E07.

**C0b — Normal histórica S1:** `Y_i|θ~N(η_i,σ²)` com as mesmas priors e hierarquia C0a; altera somente a família. Não confundir 5 graus de liberdade da Student-t com estimativa de ν.

**C1 — Skew-normal comum, única candidata nova codificada para consideração:**

`Y_i|θ ~ SN(ξ_i,ω,λ)`, densidade `f(y|ξ,ω,λ)= (2/ω) φ((y−ξ)/ω) Φ[λ(y−ξ)/ω]`; `ω>0`, `λ∈ℝ`, `δ=λ/√(1+λ²)`.

`E(Y_i|θ)=ξ_i + ωδ√(2/π)`; para manter `η` como **média** condicional, fixar **`ξ_i=η_i−ωδ√(2/π)`**. Portanto, M1 e M2 variam somente no termo hierárquico de raça, não em uma mudança arbitrária de interpretação de intercepto ou RMSE. A variância condicional é `ω²(1−2δ²/π)`.

Priors propostas C1: `α~N(0,1,5²)`, `β_k~N(0,0,5²)`, `ω~HalfNormal(0,1²)`, `λ~N(0,1,5²)`; M2 `τ~HalfNormal(0,0,5²)`, `z_j~N(0,1)`. A prior λ contraí à simetria e permite sinal negativo; sua escala é uma **escolha metodológica pós-diagnóstico**, ainda não aprovada. `ω` é parâmetro de escala, não desvio-padrão condicional; comparar plausibilidade *a priori* de dispersões antes de validar a escolha, sem ajustar a prior às taxas OOF vistas.

**Checagem a priori adicional executada somente por simulação sintética Python (não é ajuste de Stan):** 50.000 réplicas por combinação M1/M2 e idade padronizada z∈{−2,0,2}, seed 20261022, usando as priors propostas. Em M1, para z=0 os percentis 1/99 são aproximadamente −4,06/+4,11; para z=2, −6,53/+6,60. Em M2, z=2, aproximadamente −6,72/+6,68. Essa dispersão é ampla frente ao intervalo **observado** [−3,0668,+2,5096], sem que isso faça desse intervalo um suporte teórico. É um **alerta de plausibilidade pré-ajuste**, não evidência de inadequação posterior. A aceitação das priors — sobretudo da combinação com idade² em z extremos — é ponto explícito para decisão MP1. Resultados e semente: `05_auditoria/prior_predictive_sintetico_python.json`. A etapa de prior predictive em R/Stan ainda não foi executada.

**C2 — Heterocedasticidade parcimoniosa: NÃO selecionada/codificada:** alternativa se PPC condicional e evidência histórica sustentarem padrão residual por covariável, por exemplo `log ω_i = log ω_0 + γ z(idade_i)` com `γ~N(0,0,2²)`. Sua inclusão mudaria a especificação de C1 e exige **outro aditivo e autorização**. Skew-t e famílias truncadas/limitadas tampouco estão autorizadas.

### Raça inédita: distribuição e pooling corretos

Para M2 em teste com raça não observada no treino, `u_novo|τ~N(0,τ²)`, não `u_novo=0` na distribuição probabilística. A **média** condicional marginalizada recebe contribuição zero, mas a incerteza aumenta.

- Normal: `σ_novo=√(σ²+τ²)`, distribuição `N(α+xβ, σ_novo²)`.
- Student-t(5): convolução `∫t_5(y|α+xβ+u,σ)N(u|0,τ²)du`; aproximar por quadratura de Gauss–Hermite de 21 nós e verificar erro numérico contra 31 nós em teste posterior, se necessário.
- Skew-normal: somar `u~N(0,τ²)` preserva a família: `ω_novo=√(ω²+τ²)`, `δ_novo=ωδ/ω_novo`, `λ_novo=δ_novo/√(1−δ_novo²)` e `ξ_novo=(α+xβ)−ωδ√(2/π)`. Assim `E[Y_novo|θ]=α+xβ`. Esta identidade permite densidade/CDF exatas por draw, sem inventar coeficiente de raça.

## 4. Design de validação e controle de leakage

**Opção A preferida:** reservar dados **verdadeiramente novos** de origem comparável, não utilizados em exploração E05–E08, idealmente com chave de tutor. O pacote não os contém. Captura, compartilhamento ou substituição de fonte dependem de autorização e documentação de comparabilidade.

**Opção B codificada para decisão:** 5-fold OOF por cão, uma repetição, SHA256(`20261021:dog_id`) para ordenar IDs e atribuir fold por posição módulo 5. Todas as famílias C0a/C0b/C1 e M1/M2 **recebem os mesmos treinos e testes**, com ajuste novo em cada fold. As covariáveis, parâmetros e priors não são alterados após ver scores desses folds. Não fazer escolha/tuning adaptativos com o resultado dos testes externos; registrar os três painéis sem eleger automaticamente um vencedor. **Esta avaliação é interna, pós-diagnóstico, não independente** apesar de novo seed. Se a pergunta futura exigir escolher/tunar família automaticamente, exigir inner 3-fold completo dentro de cada treino outer e outro protocolo autorizado, ou preferencialmente fonte externa inédita.

Faixas diagnósticas previamente propostas: idade `≤3`, `(3,7]`, `(7,12]`, `>12` anos; sexo, tamanho, castração; suporte de raça no **treino**: inédita, 1–4, 5–19, ≥20. Quintis por `yhat` OOF do próprio modelo (descritivos, sem usar F5 observado para estratificar); reportar `n_caes` e marcar n<50 como **insuficiente para decisão por estrato**. Análises por quintis do F5 **observado** são somente descrições e não calibragem preditiva.

Dependência entre cães de um tutor desconhecido é limitação estrutural. Bootstrap de 2.000 reamostragens por `dog_id`, **pareando M1/M2 e não tratando linhas/repetições como independentes**; intervalos não corrigem clustering por tutor. Proibir inferência confirmatória por folds históricos já usados para elaborar a família.

## 5. Execução, diagnósticos e verificações

**MP0, antes da aprovação:** conferir 114 hashes do ZIP histórico, coorte 2.084, 196 raças, dobras 4.168 linhas, schema F5 e sentido, origem `score.norm`, inexistência de suporte teórico comprovado. Inspecionar PPC/resíduos históricos sem selecionar números favoráveis. Já verificado neste chat com o script herdado e auditoria Python independente.

**MP1, ainda pendente:** autorizar ou ajustar famílias, priors, tolerâncias, escolhas de particionamento e política de comparação. Vincular versão final aos hashes do Markdown e do JSON; o executor bloqueia smoke/full se não houver aprovação explícita. Este documento **não constitui aprovação**.

**MP2, somente após MP1:** captura de versões R/cmdstanr/CmdStan, `pedantic=TRUE`, compilação Stan, testes de identidade `SN(λ=0)=Normal` e recuperação de parâmetros em miniatura (SBC opcional mediante justificativa), prior predictive na escala F5 incluindo z=-2,0,2, smoke de ~40 cães, HMC completo com 4 cadeias, 1.000 warmup e 1.000 amostras por cadeia, seed por família/fold. Auditoria proposta: todos os parâmetros com R-hat ≤1,01, ESS bulk/tail ≥400; zero divergências, zero saturação `max_treedepth=12`, E-BFMI ≥0,3 por cadeia; MCSE registrada, geometria e sensibilidade investigadas quando falhar. Esses limiares são **critérios de triagem**, não prova automática de validade. CSVs CmdStan e config completos obrigatórios.

**MP3, somente após MP2:** quantis de mistura `F_pred(q)=D⁻¹∑_d F(q|θ_d)` por inversão numérica, com CDF SN calculada via normal bivariada; logscore por `logmeanexp` das densidades marginais dos draws; `yhat` pela média posterior das **médias condicionais**; IS90. PPC no treino compara média, desvio, assimetria e extremos internos sem declarar isso OOF. Avaliar q05/q95 separadamente, RMSE/MAE, IS90, logscore e contrastes pareados por cão; estratos somente com contagens. A regra de `draw_limit=1000` exige sensibilidade a 500/2.000 draws em uma amostra de casos antes de interpretação, particularmente para logscore/quantis. Para Student-t com raça inédita verificar aproximação por quadratura.

**Critérios numéricos candidatos de decisão — NÃO APROVADOS:** taxas de cauda entre 3,5% e 6,5% (±1,5 ponto percentual de 5%) **para ambas** e IC95 bootstrap por cão inteiramente dentro de [2,5%,7,5%]; scores log/IS90 sem deterioração sistemática sobre C0 mais forte em comparações pareadas e sem piora pontual material (magnitude apresentada, não decisão baseada em ranking de 1 ponto); diferença M1/M2 reportada com bootstrap descritivo. Os intervalos e tolerâncias aqui propostos não são derivados de avaliação nova nem garantem equivalência real/externa. Especificar uma função de perda ou critério de não-inferioridade se for necessário escolher automaticamente um modelo.

**MP4, gate honesto:** sem execução nova e sem auditoria independente de todos os ajustes, **BLOQUEADO para aprovação probabilística**. Após execução adequada, pode aprovar **somente validade preditiva interna com ressalvas**, nunca transportabilidade a novos tutores sem evidência adicional. Resultado desfavorável à skew-normal é resultado válido e não abre licença para ajuste oportunista de candidatos. A aprovação pontual original não é reaberta aqui. E09 e E10 não iniciadas.

## 6. Tradução equação → código (protótipo não executado)

| Matemática | Código literal novo | Verificação planejada |
|---|---|---|
| `η_i=α+xβ` | `alpha + X * beta` | Conferir oito colunas, treino-only, β por ordem E06 |
| `δ=λ/√(1+λ²)` | `lambda / sqrt(1 + square(lambda))` | Identidade λ=0, δ=0 |
| `ξ_i=η_i−ωδ√(2/π)` | `alpha + X * beta - rep_vector(mean_shift, N)` | Média preditiva η, não ξ |
| `Y_i~SN(ξ_i,ω,λ)` | `y ~ skew_normal(xi, omega, lambda);` | Confrontar logdens com `2 φ Φ /ω` e referência Stan |
| `u_j=τ z_j` | `xi[n] += tau * z_breed[breed[n]];` | Índices por raça do treino; z~N(0,1) |
| `F_pred(q)=média_d F(q|θ_d)` | `mean(component(q,eta,sigma,tau,lambda,family,new,'cdf'))` | Inverter para q05/q25/q75/q95 sem usar y_test |
| `log p_pred(y)` | `logmeanexp(component(y,...,'pdf'))` | Integração posterior por draws, não `log(media densidades)` errada |

*Para família skew-normal na expressão R, o argumento `sigma` armazena o vetor `omega`; na família Normal/Student-t armazena `sigma`. O nome de argumento genérico não muda a distinção substantiva entre escala e DP condicional.*

## 7. Separação entre decisão e proposta

**Já constatado:** integridade do pacote herdado, falha de calibração assimétrica, ganho pontual M2 histórico, ausência de `tutor_id`, impossibilidade de inferir limite do F5 do mínimo/máximo. **Proposto, NÃO congelado:** C1 skew-normal com λ comum, priors, outer folds com seed 20261021, critérios quantitativos, configuração computacional e comparação nova com C0. **Explicitamente NÃO aprovado:** modificar E06/E07, ajustar/selecionar C2/skew-t/limitadas, ajuste pleno, anúncio de sucesso ou início E09.

**Pergunta de decisão MP1:** aprovar C0 (Student-t/Normal históricos reestimados), C1 (skew-normal M1/M2), suas priors, cinco folds novos por cão e critérios descritos, ou solicitar revisão **antes de qualquer ajuste novo**. Sem decisão, entregam-se apenas especificação, código protótipo e auditoria da origem.

## 8. Fontes técnicas

Stan User’s Guide 2.40 (anexado ao projeto); Stan Functions Reference 2.40, skew normal: https://mc-stan.org/docs/2_40/functions-reference/unbounded_continuous_distributions.html ; CmdStanR sample: https://mc-stan.org/cmdstanr/reference/model-method-sample.html ; `pbivnorm` R: https://search.r-project.org/CRAN/refmans/pbivnorm/html/pbivnorm.html ; E06 JSON congelada, E07 R/Stan e diagnóstico E08 local. Fontes teóricas não substituem verificação com versão instalada de R/CmdStan.


---

## 9. MP1 — revisão técnica de 19/10/2026 (proposta v1.1, ainda não aprovada)

A revisão é **posterior à exploração dos 2.084 cães** e não cria validação independente. Os comparadores C0 Student-t(5)/Normal e a candidata C1 skew-normal permanecem os **únicos** modelos previstos para novos ajustes. Para manter a interpretação comparável da média `eta`, ficam inicialmente **inalteradas** as priors da regressão, do intercepto e da raça já descritas. A prior proposta da nova forma `lambda ~ N(0,1.5^2)` **permanece uma proposta**, sem ajuste empírico ou aprovação automática.

### 9.1 A priori comparativa estendida (sem ajuste)

Simulações independentes (Python, 70.000 sorteios por perfil/modelo/família, seed 20261019) examinaram Student-t(5), Normal e skew-normal **com os mesmos priors sobre alpha, beta e parâmetro nominal de escala**, em quatro perfis: referência z=0, referência z=2, referência z=4,1193 (idade máxima observada de 22,00 anos) e covariáveis amostradas das linhas existentes. O uso da matriz de covariáveis é **apenas descritivo**, não representa holdout inédito, e o confronto com extremos F5 observados não determina suporte teórico. Arquivos: `05_auditoria/MP1/checagem_priors_familias_MP1.csv`, `checagem_priors_MP1.json` e script integral.

No perfil de referência z=2, percentis 1/99 da prior predictive M1 são aproximadamente: t5 [-7,07, 7,04]; Normal [-6,69, 6,73]; skew-normal [-6,54, 6,60]. Em z=4,1193, todas as três famílias apresentam intervalos 1/99 em torno de ±20. O crescimento é impulsionado em grande parte por `beta_age_z_sq * z²`, termo/regularização **herdados da E06** e comuns a todas as famílias. **Não é justificativa para endurecer apenas a prior da skew-normal**, o que mudaria o preditor fixo da nova candidata e confundiria comparação de famílias. Trata-se de risco de extrapolação que exige relatório por idade e checagem posterior.

A distribuição induzida pela prior proposta de `lambda` tem cerca de 5% de probabilidade de uma assimetria **condicional absoluta** superior a 0,66. A assimetria **marginal** observada do F5 é -0,663, mas não se pode exigir nem inferir o mesmo valor para o erro condicional. Interpretar a prior de forma, inclusive seu viés para a simetria, antes da estimativa é obrigatório; observar o OOF histórico para escolher sua escala seria ajuste pós-dados e deve ser declarado como tal.

### 9.2 Correção de integridade do código R

A versão proposta anterior utilizava `cmdstanr::cmdstan_model(p, pedantic=TRUE)` sem especificar `dir`, permitindo criação de executáveis nos diretórios de `.stan` congelados. A versão v1.1 usa `dir=file.path(out, "stan_compilados")`, registra executáveis e testa sua existência. **Esta alteração é de isolamento de saída, não é mudança da verossimilhança, priors, estimandos ou preditores.** Confirmar na execução real que os hashes dos arquivos E06/E07 e dados-base permanecem idênticos.

### 9.3 Regras de decisão refinadas

- **Regra substantiva:** manter todos os 30 ajustes somente como *protocolo proposto*, não executá-los sem aprovação de MP1. Rodar `verify`/`prior` não autoriza `smoke/full`.
- **Desenho:** os cinco novos folds por cão são uma **avaliação interna condicionada**; não são teste confirmatório nem permitem afirmar que a candidata foi validada em uma amostra independente.
- **Scores:** relatar logscore, IS90, RMSE e caudas de M1 e M2 de todas as famílias sem pós-seleção. Intervalos bootstrap por cão são descritivos e não corrigem dependência entre tutores.
- **Limiar exploratório:** bandas de cauda 3,5%–6,5% e intervalos bootstrap dentro de 2,5%–7,5% passam a ser *sinais diagnósticos*, NÃO critérios suficientes nem testes de equivalência/não inferioridade, pois não houve desenho com potência/precisão previamente justificada.
- **MP2:** antes do ajuste custoso, inspecionar prior predictive na **matriz de covariáveis** e nos extremos de idade, testar funções analíticas/densidades, efetuar smoke e recuperação simulada em pequena escala. A presença do arquivo Python de prior predictive não prova que R ou Stan executaram.
- **MP3:** confirmar numericamente mistura/posterior e quantis por convergência de draws (ex.: 500/1000/2000) e GH 21/31 nós para raças inéditas, antes de interpretar diferenças pequenas; relatório de erro absoluto e decisão de suficiência computacional obrigatórios.
- **MP4:** eventual aprovação máxima **limitada a avaliação preditiva interna pós-diagnóstico**, mesmo com boas métricas. Sem tutor_id, sem validação por tutor; sem holdout externo, sem comprovação externa.

### 9.4 Decisão MP1

**Recomendação técnica:** aprovar posteriormente **somente C0 e C1, com priors comuns preservadas e prior de forma proposta**, condicionando o início das rotinas de ajuste à aprovação expressa do desenho/custo e ao teste numérico do executor. Essa recomendação não é uma autorização. `aprovacao_MP1.json` continua `aprovado=false` enquanto a pessoa responsável não aprovar expressamente a especificação revisada. Nenhum modelo é automaticamente escolhido e E09 não começa.
