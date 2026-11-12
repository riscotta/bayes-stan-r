# T05.6 — separação da CI rápida e dos testes científicos extensos

Data: 11/11/2026. Base imutável `8fe68217170cd6808d7dcab761f0d5d29b92d715`. Tarefa literal: “Separar jobs de testes rápidos e científicos extensos; controlar tempo/custos e dependências.”

## Escopo e decisão de execução

A CI rápida `.github/workflows/ci.yml` permanece sem `paths` em PR para `main`: o check obrigatório do ruleset é **R 4.5.1 — syntax and tests**. Ele depende obrigatoriamente dos jobs Python public N2 e stanc3 Stan T05.3. O `if: always()` permite executar o job R mesmo se a dependência falhar, mas etapas explícitas encerram com erro se QUALQUER dependência não for `success`; falha, cancelamento ou skip não equivalem a sucesso.

Testes estruturais e adversariais de protocolo T05.4/T05.5 agora executam rapidamente no job de sintaxe Stan, SEM compilar binários nativos CmdStan ou executar sampling. A auditoria de roteamento T05.6 executa no job Python N2, sem dependências de terceiros. `concurrency` cancela apenas execuções anteriores do **mesmo PR**; pushes de main e disparos manuais não são cancelados.

O smoke nativo T05.4 (`.github/workflows/stan-smoke-t05-4.yml`) passa a ser **somente `workflow_dispatch`**. Mantém CmdStan 2.40.0 fixo, 2 modelos, fixtures sintéticas, 2 cadeias por modelo, timeout 55 minutos e artefatos 14 dias. Execuções manuais são serializadas; não há conclusão científica automática por um smoke não executado.

A análise científica T05.5 (`.github/workflows/stan-t05-5.yml`) continua em PRs apenas quando alterados o workflow, o validador, a função auxiliar do smoke, os dois programas Stan de referência, o teste específico T05.5 ou o protocolo em `reports/t05_5/**`. Mantém execução manual opcional, CmdStan 2.40.0 fixo, sampling HMC/PPC, timeout 60 minutos e artefatos de sucesso/falha por 14 dias. Execuções obsoletas do mesmo PR são canceladas; o job científico **não** foi acrescentado ao ruleset obrigatório.

## Matriz de execução e limites

| Workflow/job | PR genérico | PR de fonte/modelo relevante | Manual | Timeout máximo |
| --- | --- | --- | --- | --- |
| CI rápida: Python N2 | sim | sim | sim | 10 min |
| CI rápida: stanc3 + testes de protocolo | sim | sim | sim | 10 min |
| CI rápida: R (obrigatório, após ambos) | sim | sim | sim | 15 min |
| T05.5: CmdStan/HMC sintético | não | sim, pelos paths declarados | sim | 60 min |
| T05.4: smoke nativo histórico | não | não | sim | 55 min |

Os tempos são limites máximos configurados por job, **não** custos realizados nem previsão de duração. A CI rápida permite até 35 runner-minutos de jobs (10+10+15), com caminho crítico teórico até 25 minutos, excluídos inicialização, fila e efeitos de cancelamento. T05.5 e T05.4 podem adicionar, quando acionados, até 60 e 55 runner-minutos respectivamente. Compilação CmdStan não possui cache compartilhado entre esses workflows; nenhum acelerador ou alteração científica foi pressuposto.

## Regressão e testes adversariais

`tests/python/test_ci_t05_6.py` verifica o contrato estático de gatilhos, ausência de filtro por paths na CI obrigatória, dependências e falha fechada, roteamento do workflow científico, limites de job, serialização/cancelamento e retenção dos artefatos. Casos adversariais rejeitam a retirada de `always()`, a perda de uma dependência, dependência fail-open, filtro indevido do check obrigatório, fonte removida do gatilho científico, perda da retenção e habilitação indevida do smoke automático. São testes de **configuração**, não simulação de eventos reais do GitHub Actions. A confirmação operacional exige os checks do PR final e inspeção do run.

## Limites e riscos remanescentes

1. A separação evita custo do T05.4 em PRs e mantém T05.5 direcionado; mudanças apenas fora de seus `paths` não acionam sampling. A regra R protegida continua global.
2. `workflow_dispatch` é execução sob demanda; CI verde na `main` não assegura que T05.4/T05.5 rodaram após o merge. O workflow T05.5 é executado no PR relevante, não automaticamente a cada push em `main`.
3. Timeout/cancelamento podem impedir a etapa `upload-artifact`; retenção de 14 dias limita auditoria tardia. Inspecionar logs/run quando a etapa não ocorrer.
4. Testes de protocolo não substituem compilação/sampling nativos. Cobertura científica T05.5 permanece apenas 2/20 modelos e fixtures sintéticas; prior `alpha` R2, SBC/calibração, dados reais e governança P0 seguem pendentes.
5. Proteção `main`: ruleset #26061035 exige PR e check R 4.5.1, 0 aprovações formais exigidas; QA independente do projeto permanece obrigatório antes de integração, ainda que o ruleset permita merge.

## Gate e evidências a preencher no HEAD final

Conferir: (a) árvore e diff integral sem alteração de programas Stan/dados/prior/likelihood; (b) Python N2, stanc e R `success` no PR; (c) teste adversarial T05.6; (d) T05.5 acionado por alteração do próprio workflow e execução científica conclusiva; (e) T05.4 não disparado automaticamente; (f) SHA-base/HEAD/reviews/pendências anotados. QA somente em chat revisor independente. T05.7 não inicia nesta execução.

Pendências P0: T01.3–T01.7 / Issue #14; Issues #15/#16; T05.2 R1/R2; T05.3 R-QA1–R-QA4; R2 prior `alpha` T05.5; decisões D00-01–D00-06. Não executar release nem publicar dados institucionais.
