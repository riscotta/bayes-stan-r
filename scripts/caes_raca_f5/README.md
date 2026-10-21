# Estudo dos Cães — raça declarada e previsão do escore F5

**Pergunta original:** acrescentar raça pura declarada pelo tutor melhora, fora do treino, a previsão de F5 (*Agonistic Threshold*) além de idade, sexo, porte e esterilização?

**Conclusão:** no subconjunto de 2.084 cães e 196 raças declaradas, M2 (interceptos hierárquicos por raça) reduziu o RMSE interno em **0,85%–1,41%** frente a M1. Pequeno efeito preditivo, condicionado a 5 folds internos após exploração prévia da coorte. Não é evidência causal genética, não representa risco individual de mordida e **não valida calibração condicional nem generalização externa**.

## Execução — padrão do repositório

Na raiz do projeto, com R 4.5.1:

```bash
Rscript scripts/caes_raca_f5/caes_raca_f5_cmdstanr.R
```

O comando **não faz novos ajustes Stan**. Verifica o número de cães, as três famílias, métricas globais, 5 folds por família, direção desfavorável do fold 1, ICs bootstrap pareados e coerência entre caudas/cobertura. Gera `outputs/caes_raca_f5/tables/comparacao_F5_E08_MP.csv` e um registro em `outputs/caes_raca_f5/logs/` (ambos regeneráveis, ignorados por Git).

**Nova amostragem, opcional e custosa:**

```bash
Rscript scripts/caes_raca_f5/caes_raca_f5_cmdstanr.R --run_stan=1
```

Exige que a base analítica E04 **original e íntegra** esteja em `data/raw/caes_raca_f5/base_F5_principal_E04.csv` e corresponda ao SHA-256 abaixo; exige R/cmdstanr, digest, jsonlite, posterior, pbivnorm e CmdStan instalado. O wrapper chama o **executor R v1.5 preservado** em `original/E08_MP_Entregaveis/02_codigo/` com `--stage=full`, criando `outputs/caes_raca_f5/full_local/` e recusando reutilizar uma pasta preenchida. Não modifica a MP1. Não confundir a versão canônica CmdStan 2.40.0 do repositório com o ambiente **histórico CmdStan 2.37.0** da E08_MP; uma reexecução em 2.40.0 deve ser auditada separadamente. A validação independente e o gate não são aprovados automaticamente.

## Modelos e resultados

| Família | RMSE M1 | RMSE M2 | Ganho relativo |
|---|---:|---:|---:|
| Student-t(5) | 0,9357 | 0,9225 | 1,41% |
| Normal | 0,9316 | 0,9195 | 1,31% |
| Skew-normal | 0,9335 | 0,9255 | 0,85% |

M0, baseado na média do treino: RMSE 0,9379. O ganho é direcional em 4/5 folds por família; **M2 piorou no fold 1**. Bootstrap pareado por cão (2.000 replicações) para ΔRMSE: Student-t(5) [0,0044; 0,0218], Normal [0,0042; 0,0200], skew-normal [0,0018; 0,0142]. Não considerar os 14.588 registros de previsão como cães independentes.

A skew-normal M2 alcançou cobertura global de 89,35% no intervalo de 90%, mas obteve **83,16% no porte 0** (n=95) e **85,34% no porte 1** (n=573). A cobertura em raças inéditas no treino foi 88,52% (n=61). Normal M2 teve melhor RMSE; skew-normal M2, melhor logscore e interval score entre as famílias. Sem vencedor universal. **Calibração condicional geral: NÃO VALIDADA.**

F5 é um escore de questionário na orientação publicada (polo mais alto: *diffident*); não mede diretamente mordidas ou ataques. Raça é **declarada**, não genótipo. Não há `tutor_id`, holdout externo, análise causal ou validação para novas populações; os folds E08_MP vieram depois de exploração histórica dos mesmos cães.

## Arquivos

- `caes_raca_f5_cmdstanr.R`: comando principal auditável.
- `original/E08_MP_Entregaveis/`: código R e PowerShell **original**, modelos Stan históricos M1/M2, candidatos skew-normal, configuração MP1 e autorização. Os fontes R/Stan e hashes estão preservados, sem revisão de priors e famílias.
- `data/raw/caes_raca_f5/resultados_auditados/`: tabelas **agregadas** para verificação offline (métricas, folds, contrastes e bootstrap).
- `data/raw/caes_raca_f5/README.md`: proveniência, restrições e localização do insumo E04 não redistribuído.
- `tests/testthat/test-caes-raca-f5-structure.R`: contrato de arquivos, integridade e limitação do script principal.

Hashes SHA-256 históricos: `executar_E08_MP.R` v1.5 `f67060377d18babe76ec3f92b32301d2598e976bcceeee52e9f5b8b2e886aea1`; PowerShell v1.3 `8bd700fd1c52658329582159029cb621fc234b6ed481fb60887e88ef7ffb0c7b`; base derivada E04, **não versionada**, `9db17e9d53edd88b7603591a89fae521f93d1a83b82a851a4156527f3408bad9`.

## Fonte e interpretação

Fonte: Morrill et al. (2022), Darwin's Ark, dataset Dryad [10.5061/dryad.g4f4qrfr0](https://doi.org/10.5061/dryad.g4f4qrfr0). Artigo: [Science 10.1126/science.abk0639](https://doi.org/10.1126/science.abk0639). A origem/termos da fonte não transferem automaticamente a licença MIT para a base derivada. O estudo completo E01–E10 foi encerrado **com ressalvas científicas** em 21/10/2026; os artefatos íntegros de cadeia/fits permanecem no arquivo histórico fora do GitHub. Publicar o código não implica revalidar estatisticamente sua utilização em contextos decisórios.
