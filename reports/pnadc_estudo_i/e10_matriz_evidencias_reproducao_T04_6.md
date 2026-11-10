# E10 | Matriz de evidências e níveis de reprodução — PNAD Contínua, Estudo I (T04.6)

**Natureza:** `RECONSTRUCAO_EDITORIAL` de 10/11/2026. Não é o relatório E10 contemporâneo do pacote local, pré-registro, protocolo institucional, reprodução de modelos ou homologação científica.  
**Tarefa literal:** “Construir E10 com matriz de evidências e instruções honestas dos níveis de reprodução”.  
**Base inspecionada:** `main` `f2de3e8070cc6b097fcbbc394ee1845be663c753` (PR #41 integrado; CI push/main #42807044017 `completed/success`). Os hashes *blob* abaixo identificam fontes nesta base; não são hashes de execuções da análise.  
**Pergunta:** “O desemprego brasileiro caiu para 5,2%. Mas o mercado de trabalho melhorou em todas as dimensões?” ([E00](e00_consolidado_T04_1.md); [nota científica](nota_cientifica.md)).

**Resultado documental admissível:** no Git público é possível auditar a estrutura e a **projeção descritiva de 26 linhas** de E09. Não é possível, apenas com os arquivos versionados, reproduzir a coleta histórica, E04–E09 integrais, as 52 cadeias relatadas, diagnósticos individuais, as 104 probabilidades condicionais de E08.2, a inferência temporal nem a melhoria conjunta. `SituacaoOficialIBGE` é rótulo descritivo/oficial, não probabilidade bayesiana. `NAO_REPRODUZIDO` não significa `REPROVADO`; `CI_VERDE` não significa `VALIDADO_CIENTIFICAMENTE`.

## 1. Quatro níveis distintos: critérios e estado no clone

| Nível | O que comprovaria | Instrução efetivamente possível e estado | O que NÃO autoriza |
| --- | --- | --- | --- |
| **N1 — evidência estrutural** | Existência, paths, conteúdo e identidade de fontes; contratos, correspondência matemática–código e oráculos sintéticos de testes | `git rev-parse HEAD`, `git hash-object CAMINHO`, inspeção de fontes; `Rscript tests/run_tests.R` **se** R/testthat estiverem instalados. **Disponível no clone**; CI do PR deve ser verificada no seu HEAD, não presumida. | Alegar execução E04–E09, compilação Stan, validade de priors ou adequação amostral |
| **N2 — reprodução descritiva** | Revalidar o SHA-256 da projeção publicada, suas 26 chaves, janelas, e a identidade aritmética `Delta = ValorDestino - ValorOrigem` | `python scripts/pnadc_estudo_i/auditar_e10.py` na raiz, com Python 3; depende somente do CSV público imutável. **Reexecutável**, com sucesso a registrar separadamente de testes editoriais. | Substituir a base E04, reestimar CV, provar significância, obter `ProbSentido` ou PPC |
| **N3 — reprodução inferencial** | Reexecutar com versões fixadas E04/E05, 13 ajustes marginais Stan E07, E08.1, E08.2 (quatro `rho` por par), E08.3/E08.4 e E09 integral, com logs/diagnósticos, checks de suporte e sensibilidades por afirmação | **NÃO DISPONÍVEL NO CLONE:** faltam entradas E04, cadeias/resumos E07, `diagnosticos_e081.csv`, saídas E08.2/E08.3/E08.4 e consolidação integral E09. Código existe, mas executá-lo exige insumos autorizados e ambiente controlado. | Concluir que as probabilidades foram reproduzidas/validadas ou que correlação temporal foi identificada |
| **N4 — reprodução integral** | Refazer E01–E03 e extração SIDRA documentada em vintage fixada; conferir manifesto dos 19 CSV locais, transformações, modelos, saídas, hashes, comparabilidade e licenças; revisar cientificamente todas as conclusões | **NÃO DISPONÍVEL:** coletor/API histórico exato, snapshot e metadados, manifesto e registros institucionais não estão integralmente no Git; P0 aberta. Não há comando end-to-end comprovado para um clone público. | Alegar homologação institucional, privacidade, aderência integral ao desenho da PNAD, causalidade ou probabilidade conjunta dos cinco eixos |

**Escada de evidência não é escada automática de aprovação:** N1+N2 não implicam N3 ou N4. Mesmo um N3 computacional bem-sucedido exigiria exame inferencial específico de suporte, desenho amostral, dependência e identificação. Aprovação editorial do presente E10 jamais transforma as 26 linhas descritivas em probabilidades.

## 2. Matriz de rastreabilidade E00–E09 (fontes, lacunas e interpretação)

Em `Etapa`, E08.1/2/3/4 discriminam operações diferentes. “Fonte” indica arquivo presente (não execução demonstrada). “Falta” indica evidência necessária para subir de nível. Localizadores adicionais, equações, diagnósticos e ressalvas constam dos documentos vinculados.

| Etapa | Artefato/versionamento presente | Evidência atual no clone | Falta para reprodução | Nível e leitura permitida |
| --- | --- | --- | --- | --- |
| E00 | [E00 consolidado](e00_consolidado_T04_1.md), blob `77141c81a2df955b57b8af5970fc75b2c8f4ba04` | Pergunta, cinco eixos, 13 indicadores, janelas anual/recente explicitadas editorialmente | E00 original contemporâneo, pré-registro e responsável científico comprovado | N1; `RECONSTRUCAO_EDITORIAL` |
| E01 | [Contratos T04.2](contratos_e01_e05_T04_2.md) | Origem SIDRA e objeto nacional referidos documentalmente | Plano/coletor original da aquisição e consulta parametrizada | N1 editorial; `NAO_DISPONIVEL_NO_GIT` |
| E02 | [Contratos T04.2](contratos_e01_e05_T04_2.md) | Tabelas/variáveis/CVs declaradas em E04 | Metadados JSON e snapshot da extração original | N1 editorial; `NAO_DISPONIVEL_NO_GIT` |
| E03 | [Contratos T04.2](contratos_e01_e05_T04_2.md) | Código histórico menciona manifesto de transporte | `MANIFESTO_SHA256.csv` original, trilha íntegra dos 19 CSV locais | N1 editorial; `NAO_DISPONIVEL_NO_GIT` |
| E04 | [construir_base_e04.py](../../scripts/pnadc_estudo_i/estudo_I/e04/construir_base_e04.py), [contratos](contratos_e01_e05_T04_2.md) | Algoritmo e asserts 22.130 entradas, 2.405 linhas/31 colunas esperadas | CSV SIDRA e metadados do pacote; saídas longa/larga e auditoria | N1; contrato estático, E04 **não reexecutada** |
| E05 | [explorar_e05.py](../../scripts/pnadc_estudo_i/estudo_I/e05/explorar_e05.py), [contratos](contratos_e01_e05_T04_2.md) | Algoritmos, 14 contrastes incluindo apoio, 16 séries exploratórias esperadas | Saídas E04, manifesto, CSV E05, figuras e auditoria | N1; E05 **não reexecutada** |
| E06 | [Especificação editorial](e06_especificacao_modelos_T04_3.md), blob `772b94db51d72d1c8412749c5339569f92a4233c` | Priors, likelihood, parâmetros e limites mapeados a Stan | E06 original; evidência de decisão contemporânea e adequação | N1 matemático/editorial, não modelo temporal conjunto |
| E07 | [Ponte matemática–Stan–R](ponte_e06_e07_T04_4.md), blob `59865b476b987d29d7dd0f746c275d9f27db0726`; [3 Stan](../../scripts/pnadc_estudo_i/estudo_I/e07/stan/) e R | Três famílias, 13 ajustes **marginais em 202609**, `theta` e `y_rep` | E04 de entrada, draws/cadeias, fit, PPC e logs para reexecução | N1; identidades de fonte, **não** 52 cadeias verificadas |
| E08.1 | [diagnostico_e081.py](../../scripts/pnadc_estudo_i/estudo_I/e08/diagnostico_e081.py), blob `f6f5f43c98cfc79ade9ab1809d21d00e89587c02` | Algoritmo PPC, R-hat, ESS, MCSE, divergências e E-BFMI | Quatro chains por indicador e `diagnosticos_e081.csv` | N1; relato agregado da [nota](nota_cientifica.md) não é validação individual |
| E08.2 | [sensibilidade_e082.py](../../scripts/pnadc_estudo_i/estudo_I/e08/sensibilidade_e082.py), blob `a0a1051a6216d0d6e4e3cb5200e6d71584f9a088` | Fórmulas gaussianas e 4 cenários `rho=-0.5,0,0.5,0.9` por 26 pares | E04/E05, `diagnosticos_e081.csv` e 104 saídas numéricas | N1; `rho` postulado, **não observado**; sem gate PPC/ESS/R-hat |
| E08.3 | [sensibilidade_e083.py](../../scripts/pnadc_estudo_i/estudo_I/e08/sensibilidade_e083.py), blob `f24680029178b9d6f462167f179378a2355a79fe` | Código para priors e 16 vértices de arredondamento | Saídas e insumos; verificação numérica de sensibilidade | N1; análise lateral, não entrada direta em E09 nem limites globais |
| E08.4 | [avaliacao_e084.py](../../scripts/pnadc_estudo_i/estudo_I/e08/avaliacao_e084.py), blob `8087ad94edf9afece517b7e60914cc436352cba7` | Código de confronto oficial e limites Fréchet de **cinco proxies** | Saídas computadas e metadados de confronto | N1; não identifica melhora simultânea dos cinco eixos substantivos |
| E09 | [consolidar_e09.py](../../scripts/pnadc_estudo_i/estudo_I/e09/consolidar_e09.py), blob `449870bc7e173577022c03589dc9c399e9af41dc`; [projeção pública](../../data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv), blob `096b9e2d42c8fe1231ac675dc48cfc61dfa294d2` | **26 deltas e rótulos oficiais auditáveis**; seleciona `rho=0` no código histórico | `contrastes_e09_consolidado.csv` integral, saídas E08.2/E08.4 e diagnósticos; nenhum campo probabilístico no CSV público | N1+N2; **DESCRITIVO_APENAS**; probabilidades `NAO_VERIFICADAS` |

A dependência efetiva do pipeline foi examinada em [T04.5](ponte_e08_e09_T04_5.md), blob `9c412d11551246412fc83fa09d52e0d5ef6eeea9`: `E07 → E08.1 → E08.2 → E09`, com `E04/E05 → E08.2`, `E08.4 → E09` e E08.3 lateral. E08.2 **lê obrigatoriamente** `diagnosticos_e081.csv`, compara `folder`, `y/s` e informações marginais `theta_mean/theta_sd`; isso é dependência/consistência de entradas, **não veto por PPC, R-hat, ESS ou divergências**. E09 não lê diretamente E08.1. A ausência dos artefatos intermediários impede reproduzir os resultados inferenciais.

## 3. Matriz das 26 afirmações: reprodução **exclusivamente descritiva**

Fonte única dos números desta tabela: [`contrastes_descritos_e09.csv`](../../data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv), SHA-256 fixado pelo [`auditar_e10.py`](../../scripts/pnadc_estudo_i/auditar_e10.py): `8bd38ce75917d981431f4c2a9c09ba73648ff1151fd6ac717068637b682fe120`. `Delta` nas unidades originais do CSV (p.p.; mil pessoas; R$). `SituacaoOficialIBGE` é transcrita como publicada no recorte, **não deriva de `Delta` por limiar arbitrário**. A leitura de cada linha é **DESCRITIVO_APENAS / SEM_PROBABILIDADE_VALIDADA**.

| ID | Eixo | IndicadorId | Contraste | Delta | SituacaoOficialIBGE |
| --- | --- | --- | --- | ---: | --- |
| T46-01 | A | `desocupacao_pct` | anual | -0.4 | caiu |
| T46-02 | A | `desocupacao_pct` | recente | -0.2 | caiu |
| T46-03 | A | `desocupados_mil` | anual | -417 | caiu |
| T46-04 | A | `desocupados_mil` | recente | -233 | caiu |
| T46-05 | B | `forca_trabalho_mil` | anual | 788 | cresceu |
| T46-06 | B | `forca_trabalho_mil` | recente | 347 | cresceu |
| T46-07 | B | `nivel_ocupacao_pct` | anual | 0.3 | estavel |
| T46-08 | B | `nivel_ocupacao_pct` | recente | 0.2 | cresceu |
| T46-09 | B | `ocupados_mil` | anual | 1205 | cresceu |
| T46-10 | B | `ocupados_mil` | recente | 581 | cresceu |
| T46-11 | B | `participacao_pct` | anual | 0 | estavel |
| T46-12 | B | `participacao_pct` | recente | 0.1 | estavel |
| T46-13 | C | `desalentados_mil` | anual | -178 | caiu |
| T46-14 | C | `desalentados_mil` | recente | 171 | cresceu |
| T46-15 | C | `desalento_pct` | anual | -0.2 | caiu |
| T46-16 | C | `desalento_pct` | recente | 0.1 | cresceu |
| T46-17 | C | `subocupacao_horas_pct` | anual | -0.3 | caiu |
| T46-18 | C | `subocupacao_horas_pct` | recente | 0.1 | cresceu |
| T46-19 | C | `subutilizacao_pct` | anual | -0.9 | caiu |
| T46-20 | C | `subutilizacao_pct` | recente | 0.1 | estavel |
| T46-21 | D | `contrib_previdencia_pct` | anual | 0.3 | Não consta no Quadro Sintético |
| T46-22 | D | `contrib_previdencia_pct` | recente | -0.1 | Não consta no Quadro Sintético |
| T46-23 | D | `informalidade_pct` | anual | -0.3 | Não consta no Quadro Sintético |
| T46-24 | D | `informalidade_pct` | recente | 0.1 | Não consta no Quadro Sintético |
| T46-25 | E | `rendimento_real_habitual_rs` | anual | 130 | cresceu |
| T46-26 | E | `rendimento_real_habitual_rs` | recente | 37 | estavel |

**Janela anual:** `202509→202609`. **Janela recente:** `202606→202609`. Ambos os contrastes compartilham destino `202609`, mas não é legítimo presumir independência entre eles nem ignorar o desenho rotativo 1-2(5). A classificação “estavel” não pode ser deduzida de um `Delta` arredondado: é rótulo do recorte oficial e depende do contexto publicado. Os quatro registros “Não consta no Quadro Sintético” **não equivalem** a inexistência de dados SIDRA ou a resultado de teste de hipótese.

## 4. Procedimento operacional verificável e gates de parada

**Pré-condição geral:** trabalhar em clone próprio, sem editar fontes históricos; fixar o commit usado e registrar sistema, versões, comandos, hash do CSV, logs e eventuais falhas. Para qualquer dado externo, confirmar direitos, privacidade e autorização institucional antes de copiar, processar ou publicar. Não executar pipeline remoto de forma automática em CI nem expor insumos protegidos.

### N1 — estrutura e contratos

```bash
git rev-parse HEAD
git hash-object scripts/pnadc_estudo_i/auditar_e10.py
git hash-object data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv
Rscript tests/run_tests.R
```

O comando R depende de instalação de R/testthat e valida **apenas o que cada teste implementa**, inclusive asserts estáticos e oráculos sintéticos. O hash Git SHA-1 identifica bytes versionados; não atesta dados institucionais, execução MCMC ou validade do modelo. Se faltar R/testthat, registrar `NAO_EXECUTADO_AMBIENTE`, não declarar teste aprovado. Os status/checks no GitHub precisam ser lidos no SHA exato do PR.

### N2 — descrição congelada

```bash
python scripts/pnadc_estudo_i/auditar_e10.py
```

O script existente usa somente Python 3 padrão e lê o CSV relativo à raiz. **Saída esperada se passar:** mensagem “AUDITORIA DE SÍNTESE: APROVADA”, 13 indicadores, cinco eixos, 26 contrastes; confere SHA-256 `8bd38ce75917d981431f4c2a9c09ba73648ff1151fd6ac717068637b682fe120`, cardinalidade, janelas, e `Delta` aritmético. Erro de SHA ou assert **interrompe** N2; não ajustar o CSV nem o hash para fazer o teste passar. O próprio script declara não reproduzir SIDRA, E04–E08, MCMC ou posterior conjunta.

### N3 — pré-requisitos antes de qualquer reexecução inferencial

Em **ambiente autorizado e separado**, recuperar entrada E04 (CSV e metadados com vintage), sua trilha SHA-256, 13 pares de quatro cadeias E07 e resumos, `diagnosticos_e081.csv`, tabelas E08.2/E08.3/E08.4 e E09 integral. Fixar versões Python/R/Stan/CmdStan e parâmetros de amostragem, confirmar caminhos legados e comparar **todos os hashes** e chaves antes de propor reamostrar. O [`run.ps1`](../../scripts/pnadc_estudo_i/estudo_I/e07/run.ps1) contém modos `preflight`, `smoke`, `completo` e `resumo`; `-Modo completo` **faria amostragem nova**, portanto não é uma auditoria inocente de histórico e não deve ser invocado automaticamente. Com entradas ausentes, **parar** em `N3_BLOQUEADO_INSUMOS`. Se forem fornecidas, a eventual execução deve ter protocolo próprio e gate científico independente: suporte, CV, arredondamento, correlação, PPC e diagnósticos associados a cada estimando. Não usar sucesso do Python como aceite inferencial automático.

### N4 — reprodução integral e publicação

Exigir reconstituição rastreável da consulta SIDRA original, versionamento temporal da API, 19 CSV locais e manifesto, transformações E01–E10, decisão da pessoa/instituição responsável sobre licenças e privacidade, comparação numérica de resultados e revisão independente. A coleta original não está inteiramente versionada; **não fornecer comando fictício** para execução integral. Sem resolver P0 e sem protocolo aprovado, registrar `N4_BLOQUEADO_ORIGEM_GOVERNANCA`. A E09 integral também não forneceria, por si, posterior temporal conjunta identificada.

## 5. Interpretação científica, incertezas e critérios negativos

- **Níveis marginais versus mudanças:** E07 contém `theta`/`y_rep` no destino `202609`; E08.2 calcula `ProbSentido` usando diferenças de agregados, `s=|y|CV/100` e `Var(Delta|rho)=s0²+s1²-2rho*s0*s1`. Esses cenários não provêm de draws conjuntos Stan. O cenário `rho=0` consolidado em E09 é hipótese, não correlação observada ou independência demonstrada.
- **Diagnóstico não é gate:** E08.1 calcula PPC e convergência marginais; E08.2 precisa de seu CSV como insumo, mas o código não veta cenários por PPC, R-hat, ESS, E-BFMI ou divergências. A nota relata historicamente 52 cadeias, 0 divergências, E-BFMI mínimo 0,96646 e R-hat máximo 1,01051; **não foram recalculados neste repositório**.
- **Riscos ainda não resolvidos:** suporte Normal de `y_rep` fora do suporte latente, CV e arredondamentos, períodos de divulgação, denominadores diferentes, rotação amostral, ausência de covariâncias temporais/entre indicadores. A E08.3 usa normal aos momentos e 16 vértices; E08.4 aplica Fréchet a cinco proxies. Nenhuma fornece `P(todos os cinco eixos melhorarem|dados)` identificada.
- **Governança não homologada:** T01.3–T01.7/Issue #14 permanecem P0, Issues #15/#16 seguem abertas, D00-01–D00-06 não se encerram por documentação. **Preservadas** as ressalvas T04.4 R1–R4 e T04.5 R1–R3. A CI verde, auditoria de hash e QA documental não certificam originalidade da coleta, licenças, privacidade, posterior, causalidade ou divulgação.
- **Controle de etapa:** T04.6 somente poderá ter `Exec. ☒` após branch, diff, testes e CI conclusivos; `QA ☐` até avaliação técnico-científica independente no HEAD final. **T04.7 não executada; Etapa 04 aberta.** Não autoaprovar ou realizar merge neste ciclo.

## 6. Índice de evidências e pedido de revisão

Documentos de base: [T04.1/E00](e00_consolidado_T04_1.md); [T04.2/E01–E05](contratos_e01_e05_T04_2.md), blob `3e87c809d8e454f212d5f401a52ccdb2f64d9b62`; [T04.3/E06](e06_especificacao_modelos_T04_3.md); [T04.4/E06→E07](ponte_e06_e07_T04_4.md); [T04.5/E08→E09](ponte_e08_e09_T04_5.md); [nota científica](nota_cientifica.md), blob `7f1dea32b2742d1f38b029bf2e92a9e7e060ef1f`; [auditoria descritiva](../../scripts/pnadc_estudo_i/auditar_e10.py), blob `a8aa628b0cad4321811acd283819203c69b493f7`; [CSV público E09](../../data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv). Localizadores de linhas por fonte estão detalhados nos documentos T04.4/T04.5.

**Revisor independente T04.6:** examinar a matriz etapa a etapa e as 26 chaves/`Delta`/`SituacaoOficialIBGE` contra o CSV imutável; verificar hashes, comandos, distinções N1/N2/N3/N4, ausências, testes, CI do HEAD, riscos de falsa equivalência entre estrutura e inferência, pendências P0/D00 e ressalvas anteriores. Emitir parecer `APROVADO`/`APROVADO_COM_RESSALVAS`/`REPROVADO`/`INCONCLUSIVO` em chat distinto. Não promover dados descritivos a inferência probabilística nem interpretar PR aberto como autorização de merge.
