# T05.4 — protocolo predefinido de smoke tests Stan (sem resultados presumidos)

Tarefa: Criar smoke tests com dados sinteticos pequenos para recuperacao de parametros e dimensoes.
Main de partida: 1db300e9e473ae777d146554b76ebbf5a4233782.
Branch: task/t05-4-smoke-sintetico-parametros-dimensoes.
Dependencia de escopo: Issue #15; T05.3 R-QA1–R-QA4 e T05.2 R1/R2 permanecem registradas.

## Seleção e material analisado

- PNAD Estudo I: scripts/pnadc_estudo_i/estudo_I/e07/stan/taxa_marginal.stan. Parametro p escalar em [0,1], generated quantities theta e y_rep. Uma observacao gaussiana sintetica de precisao conhecida; verifica o caminho basico de taxa, o intervalo e a identidade theta=100*p.
- Mega-Sena: scripts/mega_sena_dezenas/modelo_multinomial_global.stan. Parametro alpha (sum-to-zero) com D=4, simplex p de comprimento 4; y e y_rep_multinomial com G=2, D=4; log_lik com G=2. Verifica arrays, simplex, sum-to-zero do tipo e dimensoes em runtime.
- Cobertura intencional: 2 de 20 arquivos Stan em duas unidades; nao extrapolar a outros modelos, datasets ou inferencias. Nao se le nenhuma fonte de dados reais; somente copia dos dois arquivos .stan originais para uma pasta temporaria de compilacao.

## Fixtures sinteticas, sementes, tolerancias predefinidas

- Taxa: p verdadeiro = 0.68, s = 0.55, y = 100*p + ruido normal gerado por random.Random(5401); 2 cadeias com seeds 20261111 e 20261112; |media posterior(p) - 0.68| <= 0.05. Sao dados de brinquedo calibrados para bom sinal; essa tolerancia nao e um teste de cobertura nem calibracao bayesiana.
- Multinomial: D=4, G=2, p verdadeiro=(0.18,0.23,0.27,0.32), n_slots=(750,900); contagens geradas por multinomial sintetica com random.Random(5402), em ordem fixa; seeds CmdStan 20261113 e 20261114; max_k |media posterior(p[k]) - p_verdadeiro[k]| <= 0.08.
- Sampling: 2 cadeias independentes, 250 warmup + 250 draws por cadeia, delta=0.9. Exigir 500 linhas pos-warmup por modelo, todos valores examinados finitos.
- Taxa: p escalar no intervalo [0,1], colunas p/theta/y_rep, e |theta - 100*p| <= 0.005 por draw.
- Multinomial: exatamente 4 colunas p[k], 4 alpha[k], 8 y_rep_multinomial[g,k], 2 log_lik[g]. Cada p pertence a [0,1], soma p == 1 +/- 0.00002. Cada y_rep[g,k] inteiro nao negativo e soma sobre k igual a n_slots[g].
- Negativo de runtime: para G=2, D=4, fornecer intencionalmente y 2x3 e exigir rejeicao nao-zero pelo executavel nativo; salvar stderr/stdout e codigo de retorno. Falhas de dimensao e recuperacao tornam o workflow vermelho, nao apenas alerta.
- Registrar em artefatos JSON com SHA-256 das fixtures/fontes, logs de compilacao C++ nativa, CSVs de sampling, stansummary (R-hat e ESS), diagnose (divergencias, treedepth, E-BFMI) e tempo de compilacao/sampling. Esses diagnosticos sao apenas DESCRITIVOS neste ciclo; validacoes preditivas e gates numericos pertencem a T05.5.

## Ferramenta, CI, custos e requisitos

- CmdStan release oficial v2.40.0, asset cmdstan-2.40.0.tar.gz, SHA-256: 26d21c9737b35728e926f9b7a5d7362b38dc23e2e7d5e166c890e1fa74dac90c; Ubuntu 24.04 x86_64, Python 3.12.8 da stdlib. O checksum foi informado pela API oficial da release em 11/11/2026.
- Novo workflow independente e opcional: dispara em PRs que alterem arquivos da T05.4 e por workflow_dispatch. Nao se torna requisito da CI rapida nem reescreve o check R protegido. Job limitado a 55 minutos, make -j2, dois compilados e 4 cadeias curtas sequenciais.
- Distincoes: unittest local = protocolo/fixture/erros simulados; stanc = geracao C++; make = compilacao C++ nativa; CmdStan sample = amostragem real; checagem de recuperacao = propriedade de duas simulacoes favoraveis; N3/N4, PPC, SBC e validade cientifica NAO homologados.
- O workflow grava artefatos por 14 dias. Logs de diagnostico e CSVs contem exclusivamente draws com dados sinteticos; nao versionar amostras geradas nem publicar dados institucionais.

## Condicoes do QA independente

Comparar o diff completo e os dois arquivos Stan originais; confirmar identificabilidade apenas no contexto destas fixtures; reproduzir os seeds e validar dimensionalidade/invariantes, versao + checksum, compilacao, sample real, status de CI no SHA exato, resultados e logs de diagnose/stansummary. Investigar falso PASS (poucos draws, amostra excessivamente facil, tolerancias permissivas e representatividade restrita). Nao interpretar smoke PASS como calibracao, estabilidade geral ou autorizacao de dados. Executor nao faz merge nem registra sua propria revisao como QA.

Pendencias intocadas: R-QA1–R-QA4, T05.2 R1/R2, T01.3–T01.7 / Issue #14, Issues #15/#16, decisoes D00-01–D00-06; T05.5–T05.7 e Etapa 05 abertas.
