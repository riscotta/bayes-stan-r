# T03.1 — Inventário dos estimandos e das saídas efetivas da E07 (PNAD Contínua, Estudo I)

**Natureza:** auditoria documental do código versionado, não execução de E07, validação dos dados ou homologação científica.  
**Linha de base inspecionada:** `main` `709309fd4bcc2b73ab93ff09fbb1e313aa13e260` (04/11/2026).  
**Objeto:** distinguir o parâmetro marginal para cada indicador em 202609 dos contrastes temporais criados posteriormente. Este inventário não resolve T03.2–T03.7.

## 1. Contrato realmente executado pelo código E07

- [`ajustar_marginais_e07.R`](../../scripts/pnadc_estudo_i/estudo_I/e07/R/ajustar_marginais_e07.R) seleciona `Periodo == '202609' & Papel == 'principal'`, ordena por `Eixo, IndicadorId` e ajusta **um modelo independente por indicador**. O modo `completo` seleciona os 13 indicadores, e `smoke` apenas `desocupacao_pct`. Não há loop sobre 202509/202606 nem construção de diferença temporal nesse arquivo.
- [`funcoes_e07.R`](../../scripts/pnadc_estudo_i/estudo_I/e07/R/funcoes_e07.R) exige uma base E04 local `estudo_I/e07/dados/base_analitica_e04_longa.csv`, 13 indicadores principais distintos em 202609, cinco eixos A–E, valores e CV positivos/publicados. A presença e a integridade dessa base **não foram comprovadas** por esta auditoria do GitHub.
- Para cada indicador, o código calcula `s = abs(y) * CVPercentual / 100` e usa `y | theta,s ~ Normal(theta,s)`. `y` é estimativa agregada publicada; `theta` é o estimando latente daquele **indicador naquele período**, condicionado a `s`, suporte e prior. O `s` obtido do CV não é uma matriz de covariâncias.
- Os modelos [`taxa_marginal.stan`](../../scripts/pnadc_estudo_i/estudo_I/e07/stan/taxa_marginal.stan), [`contagem_marginal.stan`](../../scripts/pnadc_estudo_i/estudo_I/e07/stan/contagem_marginal.stan) e [`rendimento_marginal.stan`](../../scripts/pnadc_estudo_i/estudo_I/e07/stan/rendimento_marginal.stan) têm, respectivamente: `p ~ Beta(1,1)`, `theta=100*p` com `0<=theta<=100`; `q ~ Beta(1,1)`, `theta=P14_fixa*q` com `0<=theta<=P14_fixa`; e `mu ~ Normal(0,10000)` com `mu>0`, `theta=mu`. O denominador `P14_fixa` é entrada condicionada, não posterior estimada para população.
- Nos três arquivos Stan, `theta` e `y_rep = normal_rng(theta,s)` são `generated quantities`. `y_rep` é réplica preditiva de um único agregado, **não** diferença temporal, prova de aderência ou estimando adicional de eixo.
- O código R solicita `fit$summary(variables = 'theta')`: resumo posterior **marginal** do estimando em 202609. Não calcula `theta_202609 - theta_202509`, `theta_202609 - theta_202606`, nem `P(todos os eixos melhorarem | dados)`.

## 2. Inventário por indicador (13 parâmetros theta marginais em 202609)

Cada linha representa **um ajuste condicional separado**, não um componente de modelo multivariado conjunto. A nomenclatura abaixo vem da [projeção descritiva E09 versionada](../../data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv) e do seletor por sufixo do R; não foi inspecionada a base E04 local.

| Eixo | IndicadorId | Tipo E07 | Estimando E07 (202609) | Classe da saída |
| --- | --- | --- | --- | --- |
| A | `desocupacao_pct` | taxa | `theta=100*p` | resumo marginal |
| A | `desocupados_mil` | contagem | `theta=P14_fixa*q` | resumo marginal |
| B | `forca_trabalho_mil` | contagem | `theta=P14_fixa*q` | resumo marginal |
| B | `nivel_ocupacao_pct` | taxa | `theta=100*p` | resumo marginal |
| B | `ocupados_mil` | contagem | `theta=P14_fixa*q` | resumo marginal |
| B | `participacao_pct` | taxa | `theta=100*p` | resumo marginal |
| C | `desalentados_mil` | contagem | `theta=P14_fixa*q` | resumo marginal |
| C | `desalento_pct` | taxa | `theta=100*p` | resumo marginal |
| C | `subocupacao_horas_pct` | taxa | `theta=100*p` | resumo marginal |
| C | `subutilizacao_pct` | taxa | `theta=100*p` | resumo marginal |
| D | `contrib_previdencia_pct` | taxa | `theta=100*p` | resumo marginal |
| D | `informalidade_pct` | taxa | `theta=100*p` | resumo marginal |
| E | `rendimento_real_habitual_rs` | rendimento | `theta=mu>0` | resumo marginal |

**Conferência estrutural:** 13 IDs únicos, A=2, B=4, C=4, D=2, E=1; oito taxas, quatro contagens e um rendimento. O indicador de apoio `populacao_14mais_mil` **não** é 14º estimando: atua como denominador condicional das quatro contagens.

## 3. Saídas identificáveis no código versus evidência efetivamente disponível

| Artefato | Onde o código o produz/consome | Qual o alcance da evidência nesta revisão? |
| --- | --- | --- |
| `entrada.csv` | `resultados/<modo_data_pid>/<eixo_id_periodo>/` | **Especificado no código**, não encontrado/versionado no diretório E07 do GitHub |
| `chains_<id>*.csv` | `cmdstanr::sample(..., output_dir=dir_fit)` | **Especificado no código**; 52 cadeias históricas são relatadas no README, mas os CSVs não estão neste repositório |
| `fit.rds` | `fit$save_object(...)` | **Especificado no código**, não verificado como saída executada neste ciclo |
| `resumo_theta.csv` | `fit$summary(variables='theta')` por ajuste | **Especificado no código**, não disponível para comparação de valores neste clone |
| `resumo_marginais.csv` | Consolidação de resumos `theta` após o loop completo | **Especificado no código**, sem tabela numérica versionada que comprove 13 médias/DPs posteriores |
| `execucao.txt` | Versões, seed, cadeias, modelo e limites por ajuste | **Especificado no código**, não recuperado nesta auditoria |
| `y_rep` | `generated quantities` dos três Stan e cadeias CmdStan | **Definido**, mas não solicitado no resumo `theta`; sem PPC reexecutado/verificado |
| `contrastes_descritos_e09.csv` | `data/raw/pnadc_estudo_i/resultados_auditados/` | **Disponível/versionado**: 26 linhas descritivas de valores publicados e deltas; **não** contém draws de E07 nem probabilidades posteriores de contrastes |

O [`reprocessar_resumos_e07.R`](../../scripts/pnadc_estudo_i/estudo_I/e07/R/reprocessar_resumos_e07.R) pode reconstruir resumos **se** CSVs de cadeias estiverem presentes; sua existência não comprova reprocessamento nesta revisão. O README histórico menciona 13 modelos/52 cadeias, mas **não se verificaram os draws, ESS, R-hat ou PPC nesta tarefa**. A ausência de saídas versionadas não demonstra que nunca tenham existido fora do GitHub.

## 4. Cadeia de atribuição das diferenças e probabilidades temporais

| Etapa | Unidade/objeto | Classe metodológica | O que não se pode atribuir |
| --- | --- | --- | --- |
| E07 | `theta_i,202609` para cada um dos 13 indicadores | Posterior **marginal** condicional à estimativa e ao CV de 202609 | Posterior para `Delta_i` ou conjunta entre períodos/indicadores |
| E05 / projeção E09 pública | `Delta_i = y_i,202609 - y_i,202509` (anual) ou `y_i,202609 - y_i,202606` (recente) | **Diferença descritiva** de agregados arredondados; 13 × 2 = 26 | Contraste de draws `theta` E07 |
| [E08.2](../../scripts/pnadc_estudo_i/estudo_I/e08/sensibilidade_e082.py) | `Delta_i`, `s_0`, `s_1`, `rho` = -0,5; 0; 0,5; 0,9 | **Cenário gaussiano condicional**; usa resumo E07 202609 apenas para checagem de consistência marginal, não como amostras de diferenças | `rho` observado/estimado ou posterior temporal conjunta identificada |
| [E08.3](../../scripts/pnadc_estudo_i/estudo_I/e08/sensibilidade_e083.py) e [E08.4](../../scripts/pnadc_estudo_i/estudo_I/e08/avaliacao_e084.py) | Alternativas de prior, arredondamento, checagens e proxies de eixos | Sensibilidades/limites sob hipóteses explícitas | Prova de posterior conjunta dos cinco eixos |
| [E09](../../scripts/pnadc_estudo_i/estudo_I/e09/consolidar_e09.py) | Consolidação de 26 cenários/contrastes | Reúso de E08, **sem nova inferência ou MCMC** | 26 novos ajustes Stan |

Em E08.2, `s_Delta(rho) = sqrt(s_0^2 + s_1^2 - 2*rho*s_0*s_1)`. **A derivação formal, os pressupostos de gaussianidade e a validade de rho são escopo da T03.2**, não homologados neste inventário. A [nota científica](nota_cientifica.md) já ressalva a não identificabilidade do evento conjunto.

## 5. Pendências explícitas e próximos gates

- **T03.2:** derivar formalmente a variância dos contrastes, interpretar `rho` e suporte da aproximação.
- **T03.3:** rastrear cada probabilidade divulgada em E08/E09 à fonte computacional (draws ou cenário) com rastreabilidade de coluna/linha.
- **T03.4–T03.7:** auditar suporte/CV, sobreposição dos trimestres e covariâncias faltantes; comparar métodos, revisar rótulos e testar invariantes. Nada disso está aprovado por T03.1.
- **Governança P0:** T01.3–T01.7 e Issue #14 continuam abertas; nenhum dataset restrito foi aberto, alterado ou publicado neste ciclo.
- **Critério de QA desta alteração:** revisor distinto deve conferir o diff e os 13 IDs, seletores e modelos Stan, classes de saídas, separação de E07 e E08/E09, teste novo, CI no SHA final, inexistência de mudança científica e cobertura negativa (limites). **CI verde isolada não aprova o inventário.**

**Resultado deste inventário:** as únicas quantidades posteriores cuja computação é **programada diretamente** em E07 são 13 `theta` marginais de 202609 (e réplicas `y_rep` em cadeia); **nenhuma saída posterior E07 foi reexecutada ou auditada numericamente neste ciclo**. Os 26 deltas públicos são descrições de agregados, não 26 contrastes de amostras posteriores.
