# Raça declarada e capacidade preditiva do comportamento F5 em cães

**Fechamento científico E10 — 21 de outubro de 2026.** Evidência auditada de validação interna, não confirmatória e não causal.

## Pergunta

A raça pura declarada pelo tutor acrescenta capacidade preditiva fora do treino ao escore F5 (*Agonistic Threshold*) além de idade, sexo, porte e esterilização? O F5 é um escore comportamental de questionários do Darwin's Ark e não mede diretamente mordidas ou ataques. Maior escore corresponde ao polo *diffident*. Raça é declarada, não um indicador genético confirmado.

M1 inclui as oito colunas de preditores individuais. M2 acrescenta interceptos hierárquicos por raça declarada, com *pooling* parcial. Estimando principal: diferença RMSE(M1) − RMSE(M2) na previsão fora do treino. São 2.084 cães, 196 raças e cinco folds de tamanhos 417, 417, 417, 417 e 416 (uma repetição). As 14.588 linhas OOF são sete previsões por cão e não observações independentes adicionais.

## Resultados

| Família | RMSE M1 | RMSE M2 | Ganho relativo | IC95% ΔRMSE (bootstrap por cão) |
|---|---:|---:|---:|---:|
| Student-t(5) | 0,9357 | 0,9225 | 1,41% | [0,0044; 0,0218] |
| Normal | 0,9316 | 0,9195 | 1,31% | [0,0042; 0,0200] |
| Skew-normal | 0,9335 | 0,9255 | 0,85% | [0,0018; 0,0142] |

M0 (média do treino) RMSE 0,9379. O M2 foi melhor em quatro de cinco folds e **pior no fold 1** para as três famílias. Bootstrap pareado por cão com 2.000 replicações; seus intervalos não substituem uma avaliação em coorte nova nem resolvem eventual dependência por tutor.

## Distribuições preditivas e calibração

| Família M2 | Abaixo q05 | Acima q95 | Cobertura central 90% |
|---|---:|---:|---:|
| Student-t(5) | 8,16% | 1,78% | 90,07% |
| Normal | 7,44% | 2,50% | 90,07% |
| Skew-normal | 5,71% | 4,94% | 89,35% |

A Normal M2 teve menor RMSE; a skew-normal M2 obteve melhor logscore e interval score. Não houve vencedor universal. O ganho de logscore **da raça** na skew-normal M2 versus M1 foi +0,00557, IC95% [−0,00117; +0,01223], que inclui zero.

Cobertura skew-normal M2 por porte: código 0 = 83,16% (n=95); código 1 = 85,34% (n=573); código 2 = 90,53% (n=676); código 3 = 92,50% (n=627); código 4 = 90,27% (n=113). Raças não vistas no treino: 88,52% (n=61). **CALIBRAÇÃO CONDICIONAL GERAL: NÃO VALIDADA.**

## Inferência computacional e ressalvas

Execução histórica E08_MP: 30 ajustes, 120 cadeias CmdStan, 1.000 warmup + 1.000 draws salvos por cadeia; códigos de retorno zero, sem divergências HMC nem saturações do treedepth, R-hat máximo 1,0082, ESS bulk mínimo 833, ESS tail mínimo 1.432, E-BFMI mínimo 0,6147. Houve avisos *pedantic* e 310 notificações de propostas Metropolis rejeitadas por problemas locais de escala/localização, sem falha terminal. As checagens de quadratura e número de draws foram amostrais.

Os folds E08_MP foram planejados depois de exploração anterior da mesma coorte. Não há holdout genuinamente externo nem identificador de tutor para splits/bootstrap por domicílio. Portanto o resultado demonstra apenas melhora preditiva pequena e interna. Não infere causalidade genética, risco de mordida ou utilidade prática de decisões individuais por raça.

Os códigos R/Stan originais estão em `scripts/caes_raca_f5/original/E08_MP_Entregaveis/`; base E04 individual e 120 CSVs completos do CmdStan não são versionados. O script R padrão recompõe tabelas agregadas, mas não executa Stan. O argumento `--run_stan=1` exige a base E04 externa com hash histórico e um ambiente de compilação/amostragem; não certifica o resultado automaticamente. O ambiente histórico foi R 4.5.1 / CmdStan 2.37.0, enquanto a versão canônica do repositório é 2.40.0.

Fontes: Morrill et al. (2022), [Dryad](https://doi.org/10.5061/dryad.g4f4qrfr0), [Science](https://doi.org/10.1126/science.abk0639). Evidências e script principal: [README local](../../scripts/caes_raca_f5/README.md).

**Conclusão:** raça declarada acrescenta pouca informação ao erro preditivo do F5 no recorte estudado. Calibração condicional e generalização externa permanecem não validadas.