# T05.5 — protocolo pré-registrado: prior predictive, posterior predictive e HMC

Tarefa literal: **Adicionar checks prior/predictive e diagnósticos numéricos com tolerâncias justificadas.**
Data: 2026-11-11. Base congelada: `34c60eb7ac404d6fb5463cc60bcf771e7669e0ec`. Branch: `task/t05-5-prior-predictive-diagnosticos`. Este protocolo antecede qualquer execução científica T05.5; **não modificar limiares depois de observar resultados para obter PASS**.

## Escopo e hipóteses

Cobertura delimitada: os dois programas Stan intocados de T05.4 (taxa marginal PNAD com `p ~ beta(1,1)` e `y ~ normal(100*p,s)`; multinomial com `sum_to_zero_vector[4] alpha ~ normal(0,0.35)`, `p=softmax(alpha)`, `y[g]~multinomial(p)`). A fixture observacional sintética vem de T05.4, seeds Python 5401 e 5402. Zero acesso a dados institucionais.

- **H-P0 (prior independente, fora de Stan):** taxa `E[p]=0.5`, `E[y_prior]=50`. Simular com `random.Random(55101)`, 8.192 draws de `p~U(0,1)`, `y_prior~Normal(100p,0.55)`. Exigir finitude, p em [0,1], desvio `|mean(p)-0.5| <= 0.03`; desvio do resíduo médio `|mean(y_prior-100p)| <= 0.05`. Primeiro limite é mais largo que Hoeffding para N=8192 e erro unilateral 0,001; segundo excede 5 desvios-padrão nominais `0.55/sqrt(8192)`. São checks de uma implementação independente, **não teste de equivalência do RNG Stan**.
- **H-P1 (prior independente, fora de Stan):** gerar 8.192 vetores gaussianos iid `z_k~N(0,0.35²)`, projetar `alpha_k=z_k-mean(z)`, aplicar softmax, e para grupos `n_slots=(750,900)` realizar draws multinomiais independentes. Seed Python 55102. Esta é a distribuição gaussiana induzida no subespaço soma-zero; variância marginal teórica `0.35²*(1-1/4)`. Exigir finitude, soma-zero até `1e-10`, simplex até `1e-12`, inteiros e totais exatos; `max_k |mean(p_k)-0.25| <=0.03`. Limite é conservador diante de Hoeffding com 8.192 desenhos e 4 coordenadas; não reivindica SBC nem calibração.
- **H-PP0:** posterior predictive (Stan nativo) das mesmas fixtures, usando `y_rep` das quatro cadeias; `|mean(y_rep)-100*mean(p)| <=0.15` em unidades da variável observada. Esse é um *check de consistência do gerador condicional*, não teste de adequação científica. Estatística de discrepância observada/replicada registrada, **sem gate sobre posterior predictive p-value**.
- **H-PP1:** para cada grupo e categoria, `|mean(y_rep[g,k]/n_slots[g])-mean(p[k])| <=0.025`. Essa é a consistência de média do gerador multinomial condicional; 0.025 é folga deliberadamente conservadora frente à dispersão binomial e 2.400 desenhos. Registrar discrepâncias para contagens observadas e replicadas sem classificar p-values como calibração.
- **H-HMC:** 4 cadeias independentes com 500 warmup + 600 amostras cada, `adapt delta=0.9`, `max_depth=10` (padrão fixado), seeds 20261121–20261124 para taxa e 20261125–20261128 para multinomial. Usar CmdStan v2.40.0, asset verificado por SHA-256 de T05.4. Inspecionar 2.400 draws por modelo e *cada cadeia individualmente*, sem embaralhar séries.
- **Gate HMC numérico** (somente escalares `p` da taxa, `alpha[1:4]` e `p[1:4]` multinomial): `R_hat < 1.01`, `ESS_bulk >=400`, `ESS_tail >=400` do `stansummary` nativo (CSV com alta precisão). Justificativa: recomendação Stan de R-hat <1.01 e ESS_bulk >= 100 vezes o número de cadeias; aplicar mesmo patamar conservador a ESS_tail. 4 cadeias reduzem riscos de falso diagnóstico vs 2.
- **Gate sampler**: `sum(divergent__)=0` (divergência não tolerada); `treedepth__ <10` em todos os draws (saturação de profundidade não aceita para este smoke e sinaliza ineficiência); `E-BFMI >=0.30` por cadeia, calculado como `mean(diff(energy__)²) / var_pop(energy__)`; rejeitar energia constante. Rejeitar também NaN/Inf, inconsistência de linhas, colunas ou quantidade de cadeias. A tolerância zero para treedepth é conservadora, não um teorema universal.
- **Fronteira diagnóstica**: `y_rep_multinomial[1,1]` apresentou aviso de R-hat >1.01 no T05.4 (2 cadeias × 250); reavaliar estatísticas de generated quantities descritivamente, mas **não usar um R-hat isolado de sorteio posterior preditivo para reprovar o posterior dos parâmetros**. Registrar exceção, não omiti-la.

## Execução e critérios adversariais

Implementar Python 3.12.8 stdlib, manter o workflow R obrigatório intacto; workflow independente de PR com filtro T05.5 + `workflow_dispatch`; `timeout-minutes: 60`, artefatos de outputs, CSVs, resumos, diagnósticos e relatório, retenção 14 dias. Checar em testes estruturais no mínimo: sementes/reprodutibilidade, prior inválido, ausência de parâmetro, R-hat >=1.01, ESS baixo, divergências, profundidade, E-BFMI baixo, preditiva adulterada, CSV truncado e independência de dados externos. Só passar HMC com stansummary nativo real no SHA correto.

Pré-registro não implica que o resultado será PASS. Se FAIL, preservar logs e reportar o achado em vez de relaxar o limite. Não alterar modelos, priors, likelihoods ou outputs históricos. O gating científico **não homologa** PPC bayesiano, SBC, predição fora da amostra, identificabilidade geral, dados reais, licenças, N3/N4, nem comparações LOO. Cobertura 2/20 modelos Stan.

Pendências mantidas: P0 T01.3–T01.7 / Issue #14, Issues #15/#16, T05.2 R1/R2, T05.3 R-QA1–R-QA4, D00-01–D00-06; Etapa 05 ABERTA, T05.6/T05.7 abertas.

Fontes de limiares e interpretação:
- https://mc-stan.org/docs/cmdstan-guide/diagnose_utility.html
- https://mc-stan.org/docs/2_39/cmdstan-guide/stansummary.html
- https://mc-stan.org/docs/reference-manual/transforms.html
