# T04.3 — E06: especificação matemática reconstruída | PNAD Contínua, Estudo I

**Natureza:** `RECONSTRUCAO_EDITORIAL` em 09/11/2026, a partir dos três modelos Stan e do preparador R históricos E07. **Não foi localizado um E06 contemporâneo original na árvore Git examinada.** Este texto não é pré-registro, nova especificação aprovada, evidência de execução MCMC, nem validação científica dos ajustes.  
**Tarefa literal:** “Escrever E06 com justificativa das distribuições, priors, identificação, dependências e alternativas”.  
**Base examinada:** `main` `7e287cc31303e336a13cc65186edb4e6b87dc6c6`; T04.1/PR #37, T04.2/PR #38 e auditorias T03.1–T03.7 já integradas.  
**Objeto:** 13 indicadores substantivos nacionais agregados em cinco eixos (A=2, B=4, C=4, D=2, E=1), **somente período 202609** em E07; `populacao_14mais_mil` é entrada de apoio para quatro contagens, não 14º ajuste. Nada neste E06 modela os dois períodos de origem nem os 26 deltas.

## 1. Estimando, observação e preparação

A pergunta literal do [E00 consolidado](e00_consolidado_T04_1.md) é: **“O desemprego brasileiro caiu para 5,2%. Mas o mercado de trabalho melhorou em todas as dimensões?”** O E07 histórico **não resolve diretamente** a segunda pergunta: ajusta o nível agregado marginal de cada indicador no trimestre móvel terminado em setembro de 2026. O [contrato E01–E05](contratos_e01_e05_T04_2.md) distingue as 13 séries principais da população de apoio, taxas com denominadores distintos, totais expandidos em milhares e rendimento real habitual (R$ da vintage).

Para o indicador `i` em `t=202609`:

- `y_i`: estimativa **agregada** publicada em sua unidade (pontos percentuais, milhares de pessoas ou R$); não é observação individual;
- `c_i=CVPercentual_i>0`: coeficiente de variação declarado para a estimativa, em **porcentagem**, não covariância;
- `s_i=|y_i| c_i/100 > 0`: erro-padrão **aproximado por plug-in**, usado como desvio conhecido na likelihood, não desvio posterior estimado;
- `theta_i`: nível agregado latente do **mesmo indicador e período**, sujeito ao suporte do modelo; `P_i` denota a população nacional 14+ **em milhares**, fixada como entrada apenas nas quatro contagens.

O [R E07](../../scripts/pnadc_estudo_i/estudo_I/e07/R/ajustar_marginais_e07.R) lê `base_analitica_e04_longa.csv` em `estudo_I/e07/dados/`, filtra `Periodo == '202609' & Papel == 'principal'`, ordena por `Eixo, IndicadorId`, calcula `s <- abs(y)*cv/100` e executa um ajuste Stan **separado por linha**. A função [`carregar_e_validar_e04`](../../scripts/pnadc_estudo_i/estudo_I/e07/R/funcoes_e07.R) exige 13 IDs únicos, cinco eixos, valor/CV publicados, finitos e CV positivo; o runner exige `s>0`, taxas observadas entre 0 e 100 e, nas contagens, `P_i>0` com `y_i<=P_i`. A fonte R não transforma `CV` em matriz de covariâncias nem estima a incerteza de `P_i`. A semântica amostral e a vintage E04 completas **não foram verificadas** nesta tarefa; insumos E04 e saídas MCMC E07 não estão no clone.

**Likelihood comum aos três arquivos históricos (aproximação do erro de estimação do agregado):**

```text
y_i | theta_i, s_i  ~  Normal(theta_i, s_i²),     s_i = |y_i| CVPercentual_i / 100 .
L(theta_i; y_i,s_i) ∝ exp{ -(y_i-theta_i)² / (2 s_i²) } .
```

Em Stan, `normal(theta,s)` recebe **desvio-padrão** `s`, não variância. A escolha Normal é uma **aproximação operacional** para estimativas agregadas sob incerteza amostral resumida pelo CV; não demonstra normalidade sob o desenho complexo da PNAD. **Não** é Binomial de respondentes para taxas, Poisson/NB de pessoas para totais nem modelo para distribuição individual de rendimentos. `s` deriva de `y` e é tratado como fixo; ignoram-se incerteza do CV, arredondamento, erros de especificação e, nas contagens, incerteza do denominador.

## 2. Três famílias marginais de E07 e priors *efetivamente codificadas*

| ID E06 | Tipo / fonte E07 | Parâmetro e suporte | Prior histórica, escala e prior induzida em `theta` | Likelihood e `theta` |
| --- | --- | --- | --- | --- |
| M06-T | 8 taxas; [`taxa_marginal.stan`](../../scripts/pnadc_estudo_i/estudo_I/e07/stan/taxa_marginal.stan) | `p_i ∈ [0,1]`; `theta_i=100 p_i` em **p.p.** | `p_i ~ Beta(1,1)` uniforme em [0,1]; implica `theta_i ~ Uniform(0,100)` | `y_i ~ Normal(100 p_i,s_i)` |
| M06-C | 4 totais; [`contagem_marginal.stan`](../../scripts/pnadc_estudo_i/estudo_I/e07/stan/contagem_marginal.stan) | `q_i ∈ [0,1]`; `theta_i=P_i q_i` em **mil pessoas**, com `P_i>0` conhecido no ajuste | `q_i ~ Beta(1,1)`; implica `theta_i | P_i ~ Uniform(0,P_i)` | `y_i ~ Normal(P_i q_i,s_i)` |
| M06-R | 1 rendimento; [`rendimento_marginal.stan`](../../scripts/pnadc_estudo_i/estudo_I/e07/stan/rendimento_marginal.stan) | `mu_i>0`; `theta_i=mu_i` em **R$** | `mu_i ~ Normal(0,10000)` **restrita ao domínio não negativo pelo parâmetro Stan**; densidade induzida de meia-Normal com **desvio 10000**, não variância 10000 | `y_i ~ Normal(mu_i,s_i)` |

**Justificativa e limite de cada prior (interpretação editorial, não motivação histórica comprovada):**

- **Taxas:** Beta(1,1) mantém probabilidade sobre a proporção e induz prior uniforme sobre 0–100 p.p.; não privilegia um valor nem incorpora informação sobre o universo/denominador de cada taxa. “Uniforme” **não significa sem informação** nem garante calibração adequada. Um mesmo intervalo de suporte não torna taxas distintas complementares.
- **Contagens:** Beta(1,1) no fator `q` impõe contagem latente não negativa e limitada por `P14` fixa. Esse `q` é **parametrização auxiliar de escala**, não probabilidade individual estimada de pertencer ao universo de cada indicador, nem compartilhada entre os quatro modelos. O uso de `P14` é um **teto matemático simplificador**, não o denominador substantivo de toda contagem (a força de trabalho, os ocupados e os desalentados têm definições diferentes). Não se propaga a incerteza de `P14`; a prior uniforme em `theta` muda com `P14`.
- **Rendimento:** Normal(0,10000) em parâmetro `real<lower=0> mu` produz distribuição *truncada pela restrição de suporte*; como a média é zero, equivale à meia-Normal com desvio 10000. Stan não implementa uma Normal não truncada com `mu` negativo e tampouco uma LogNormal. A escala pode ser ampla, mas “fraca” é juízo empírico **não demonstrado** sem análise de sensibilidade/prior preditiva e comparação com a vintage de rendimentos.

Os valores **1,1** e **10000** são transcrição das priors existentes, **não** propostas de substituição. Nenhuma prior informativa entre indicadores, hierárquica, temporal, ou prior sobre `rho` consta nos três Stan. A factorização a seguir descreve ajustes separados, não a estrutura de dependência verdadeira do processo amostral.

## 3. Posterior por indicador, suporte e quantidades derivadas

Para `i`, condicionando nas entradas fixas `s_i` e, se aplicável, `P_i`:

```text
pi_i(theta_i | y_i,s_i,P_i) ∝ Normal(y_i | theta_i,s_i²) × pi_i(theta_i | P_i)
  para theta_i no suporte do tipo; densidade normalizada sobre esse suporte.
```

Para taxas o suporte do **nível latente** é `[0,100]`; para contagens `[0,P_i]`; para rendimento, `[0,+∞)`. Isso não restringe a distribuição Normal de `y_i` nem de `y_rep`, que pode gerar valores fora do suporte do parâmetro: risco de PPC e adequação da likelihood, **não erro de leitura do Stan**. A função de massa zero nas fronteiras é detalhe de representação contínua, não garantia de arredondamento oficial.

Os três blocos `generated quantities` definem exatamente:

```text
theta_i = 100*p_i           (taxa)
theta_i = P_i*q_i           (contagem)
theta_i = mu_i              (rendimento)
y_rep_i | theta_i,s_i ~ Normal(theta_i,s_i²)
```

`theta` é transformação determinística do parâmetro de cada **modelo individual**; `y_rep` é réplica preditiva do **único agregado** e não nova observação PNAD nem contraste. O runner solicita `fit$summary(variables='theta')` e grava resumos/arquivos de cadeias por ajuste; **não** grava um objeto com joint draws temporal ou entre indicadores. O PPC histórico E08.1 (se disponível em arquivos externos) não foi reexecutado aqui. A matriz **linha por linha E06→Stan→R**, incluindo IDs/linhas, testes de equivalência e divergências, é escopo da **T04.4**, não aprovação antecipada.

## 4. Identificação: o que os agregados permitem e o que não permitem

**Condicionalmente ao modelo marginal:** com `y_i` finito, `s_i>0` tratado conhecido, suporte fixado e prior própria, a função Normal de localização varia com `theta_i` e define uma posterior própria para o nível `theta_i`. Isso é **identificação paramétrica no modelo de um agregado**, não comprovação da adequação da Normal, da qualidade do CV, da validade da cobertura do desenho ou da precisão de uma única medição. A inferência depende das priors e dos limites quando `s` é grande ou `y` está perto do suporte.

**Não identificação relevante à pergunta E00:**

1. E07 ajusta apenas `202609`; não contém `theta_{202509}` ou `theta_{202606}`; logo não identifica a posterior de `Delta_i=theta_{202609}-theta_{origem}`.
2. Valores/CVs marginais não especificam `Cov(y_{i,t0},y_{i,t1})` nem `Cov(y_{i,t},y_{j,u})`. Independência do código de ajuste **não demonstra** independência amostral, temporal, populacional ou posterior conjunta empiricamente identificada.
3. Uma **prior própria** sobre correlação não cria informação observada acerca dela. Uma distribuição conjunta escolhida por suposição pode existir matematicamente, mas seus contrastes e eventos simultâneos permanecem condicionais à suposição.
4. O apoio `P14` é um agregado estimado, porém fixado na likelihood da contagem; não modelar sua covariância com os totais pode reduzir artificialmente a incerteza. Identidades e denominadores substantivos entre indicadores não estão codificados como restrições de uma posterior conjunta.
5. H01–H03 de E00 são **hipóteses editoriais propostas**, não hipóteses estatísticas exaustivas, mutuamente exclusivas ou previamente registradas; D00-01–D00-06 e `NAO_DECIDIR` continuam abertos.

## 5. Dependência amostral, contrastes e cenários separados de E06/E07

Cada `Periodo` identifica o **mês final de um trimestre móvel**. Nos contrastes do piloto, `202509` (jul–set/2025) → `202609` (jul–set/2026) e `202606` (abr–jun/2026) → `202609` (jul–set/2026), **origem e destino não compartilham meses dentro de cada par**, mas os dois contrastes compartilham **o mesmo destino 202609**. A rotação amostral 1-2(5), pesos, reutilização de domicílios e definições entre indicadores impedem inferir independência apenas da disjunção de meses.

A [T03.2](derivacao_contrastes_gaussianos_T03_2.md) descreve **outro objeto**, a aproximação **E08.2** com marginais operacionais `(y_0,s_0)`, `(y_1,s_1)` e covariância `rho s_0 s_1` **postulada**:

```text
Sigma(rho) = [[s0², rho*s0*s1], [rho*s0*s1, s1²]],
Var_cenario(Delta | rho) = s0²+s1²-2*rho*s0*s1,
Delta_cenario | rho ~ Normal(y1-y0, Var_cenario(Delta | rho)).
```

Os únicos valores codificados para `rho` em [E08.2](../../scripts/pnadc_estudo_i/estudo_I/e08/sensibilidade_e082.py) são `-0.5, 0, 0.5, 0.9`: **hipóteses**, não correlações observadas. `rho=0` em E09 é cenário de independência **postulada**, não identificada. A [T03.3](rastreabilidade_probabilidades_T03_3.md) separa draws marginais E07/E08.1 dos cenários E08.2, propagação E08.3, proxies E08.4 e E09. A [T03.6](rotulos_inferenciais_T03_6.md) impede ler `ProbSentido` ou `IC95Inf/IC95Sup` como posterior conjunta ou intervalo com cobertura oficialmente demonstrada.

A [T03.4](auditoria_suporte_cv_arredondamento_T03_4.md) registra que a Normal operacional não truncada de E08.2 não herda os suportes de `theta` do Stan, que CV de agregados não fornece covariâncias, e que precisão publicada, quatro lacunas `201510/201511`, quebra de conceito em 2015 e início da informalidade em `201512` podem limitar comparações. O [CSV público E09](../../data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv) contém **26 diferenças descritivas**, não CVs, draws, correlações ou uma joint posterior; sua existência não valida 26 probabilidades condicionais.

## 6. Alternativas metodológicas — avaliação, não implementação

| Alternativa | Condições mínimas | Limite e decisão neste E06 |
| --- | --- | --- |
| **A. Preservar E07 marginal + E08/E09 histórico rotulado como cenário** | Usar priors/likelihood originais sem alteração; reportar `s` como plug-in e `rho` como hipótese, com limitações de suporte, arredondamento e desenho | **É a descrição fiel do legado.** Não valida probabilidades ou melhora conjunta. |
| **B. Modelo temporal por indicador** | Covariâncias amostrais entre períodos verificáveis sob desenho, ou dados/replicações autorizados; especificação de likelihood conjunta, priors, denominadores e diagnóstico | **Não implementado.** Duas/três marginais e seus CVs não identificam o parâmetro de correlação; precisaria de novo protocolo e revisão científica. |
| **C. Modelo temporal multivariado entre indicadores/eixos** | Requisitos de B, mais covariâncias cruzadas coerentes e estimando/evento de melhora por eixo pré-definido; sensibilidade, PPC, recuperação e validação | **Não implementado.** Cinco proxies de E08.4 não são probabilidade conjunta identificada para os cinco eixos. |
| **D. Likelihoods/priors alternativas para marginais** | Estudo prévio de suporte e desenho; por exemplo erro t em estimativas, transformações/log-escala ou priors informativas justificadas, com transformação/Jacobiano quando pertinente | **Somente possibilidades de pesquisa**, não opções já validadas. Mudar Beta(1,1), meia-Normal(10000) ou Normal da likelihood requer autorização e comparação científica separadas. |

A decisão da [T03.5](comparacao_alternativas_inferenciais_T03_5.md), no escopo técnico-documental, é manter a alternativa **A** como legado exploratório e condicionar B/C à obtenção de covariâncias e autorização institucional. Nem prior alternativa nem `rho` livre nos agregados atuais constituem identificação automaticamente. Não há mudança no código Stan/R, nas priors, nos dados ou nos resultados.

## 7. Limites de verificação, dependências e aceite

**Verificado neste ciclo documental:** leitura estática de três `.stan`, `ajustar_marginais_e07.R`, `funcoes_e07.R`, E00 T04.1, contratos T04.2 e auditorias T03.1–T03.7; confronto de expressões, unidades, priors e fronteiras inferenciais com esses fontes. Testes T04.3 devem confrontar novamente os códigos versionados e a álgebra **apenas em exemplos sintéticos**, não fabricar dados PNAD.

**Não verificado:** E06 histórico contemporâneo, vintage completa E04, metadados JSON, manifesto, CVs marginais numéricos, matrizes de covariância, saída E07/MCMC, R-hat/ESS/PPC empírico, posterior temporal/multivariada, cobertura de desenho, resultados oficiais da API/precisão do arredondamento, autorização institucional ou licenças. `CI=success` significa somente que as verificações configuradas passaram.

**Governança:** Etapa 01/P0 **ABERTA**; T01.3–T01.7 `Exec. ☐ / QA ☐`, [Issue #14](https://github.com/riscotta/bayes-stan-r/issues/14) e [Issues #15](https://github.com/riscotta/bayes-stan-r/issues/15)/[#16](https://github.com/riscotta/bayes-stan-r/issues/16) **ABERTAS**. T04.3 **Exec. sob análise / QA PENDENTE** até revisão independente em chat distinto; T04.4–T04.7 `Exec. ☐ / QA ☐`. Etapa 04 **ABERTA**, sem merge da T04.3 por este executor. O [formulário T02.4](../../scripts/STUDY_E06_E07_MATRIX_T02_4.md) e a T04.4 exigirão uma matriz completa linha a linha de E06→E07; esta etapa apenas escreve e justifica E06.

**Fontes primárias e testes:** [E00](e00_consolidado_T04_1.md) · [E01–E05](contratos_e01_e05_T04_2.md) · [T03.1](inventario_estimandos_e07_T03_1.md) · [T03.2](derivacao_contrastes_gaussianos_T03_2.md) · [T03.3](rastreabilidade_probabilidades_T03_3.md) · [T03.4](auditoria_suporte_cv_arredondamento_T03_4.md) · [T03.5](comparacao_alternativas_inferenciais_T03_5.md) · [T03.6](rotulos_inferenciais_T03_6.md) · [T03.7](invariantes_sinteticos_T03_7.md) · [Stan taxa](../../scripts/pnadc_estudo_i/estudo_I/e07/stan/taxa_marginal.stan) · [Stan contagem](../../scripts/pnadc_estudo_i/estudo_I/e07/stan/contagem_marginal.stan) · [Stan rendimento](../../scripts/pnadc_estudo_i/estudo_I/e07/stan/rendimento_marginal.stan) · [R ajuste](../../scripts/pnadc_estudo_i/estudo_I/e07/R/ajustar_marginais_e07.R) · [R validação](../../scripts/pnadc_estudo_i/estudo_I/e07/R/funcoes_e07.R).

**Gate T04.3:** revisor distinto deve confrontar **todas** as expressões E06/Stan/R no HEAD exato, verificar truncamento/escala, condicionalidade e validade dos testes, inspecionar o diff, seguir CI do PR, registrar falsos positivos e riscos; não aceitar auto-QA, status `APROVADO` ou merge por CI verde.
