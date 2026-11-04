# T02.7 — Piloto documental de aplicação dos contratos: mortalidade SP 0–74

**Unidade histórica:** `scripts/mortalidade_sp_0_74/` · **Classe T02.1:** Estudo aplicado.
**Registro:** `RECONSTRUCAO_EDITORIAL` de 04/11/2026, não E00/E06/E08 original nem nova versão científica.
**Linha de base de leitura:** `main` `e9e9ce1bcad4ed1fd7e412e790e2b5505874452a`.
**Operação realizada:** leitura documental de README, código E07/E08, caderno e matriz de evidências versionados. **Não houve** inspeção de linhas de dados, execução R/Stan, reestimação, revisão estatística independente, homologação de licença/privacidade ou decisão científica. `PENDENTE` indica estado **da etapa no piloto**, não a ausência de qualquer documento.

**Contratos aplicados:** [T02.1 taxonomia](STUDY_TAXONOMY_T02_1.md) · [T02.2 oito campos](STUDY_METADATA_CONTRACT_T02_2.md) · [T02.3 E00](STUDY_E00_TEMPLATE_T02_3.md) · [T02.4 E06→E07](STUDY_E06_E07_MATRIX_T02_4.md) · [T02.5 E08→E09](STUDY_E08_E09_MATRIX_T02_5.md) · [T02.6 estados](STUDY_STAGE_STATUS_T02_6.md).
A aplicação verifica a **usabilidade editorial** dos contratos, não comprova preenchimento histórico integral, validade inferencial nem autorização para redistribuir fontes.

## 1. Metadados obrigatórios — T02.2

| Campo | Preenchimento editorial e fonte/limite |
| --- | --- |
| Pergunta | Como a mortalidade registrada de pessoas de 0–74 anos mudou de 2006 a 2023, distinguindo composição por idade/sexo, taxas específicas e qualidade das previsões? Formulação recuperada do [caderno E01–E10](../reports/mortalidade_sp_0_74/caderno_E01_E10.md); **não** é E00 original pré-especificado. |
| Objeto | Células distrito × ano × sexo × grupo quinquenal, São Paulo, 96 distritos, 2006–2023; base territorial provisória. Fonte: [README do estudo](mortalidade_sp_0_74/README.md). |
| Dados | Origem descrita APVP/PRO-AIM × SEADE; agregados E09 publicados e base celular/insumos primários ausentes no clone. A documentação de direitos e limites territoriais não está homologada; [README](mortalidade_sp_0_74/README.md) e [matriz](../reports/mortalidade_sp_0_74/matriz_evidencias.md). Conteúdo individual **não inspecionado**. |
| Método | Taxas brutas/padronizadas, decomposição simétrica e modelos Poisson/NB2 em **R**, sem modelo Stan; [caderno](../reports/mortalidade_sp_0_74/caderno_E01_E10.md), [código E07](mortalidade_sp_0_74/01_E07_ajustar_modelos.R). Equivalência matemática E06→E07 completa **não auditada**. |
| Estado | Documentação de fontes acessíveis: **inspecionada nesta tarefa**; teste documental do piloto: **versionado nesta T02.7, com execução conferível nos logs do PR #29**. Execução R científica: **não efetuada neste ciclo**; QA independente e validade científica: `PENDENTE_DE_VERIFICACAO`. Estado E00–E10 discriminado na seção 5. |
| Resultados | Tabelas agregadas e interpretações **históricas** disponíveis em [relatório](../reports/mortalidade_sp_0_74/relatorio_cientifico.md); não foram recalculadas nem homologadas nesta tarefa. |
| Limites | Base territorial provisória, insumos primários não distribuídos, falta de licença original homologada, limites da cobertura preditiva e ausência de identificação causal. [Matriz de evidências](../reports/mortalidade_sp_0_74/matriz_evidencias.md). |
| Responsável | Responsável científico/decisor autorizado e parecer independente específico deste piloto: `PENDENTE_DE_VERIFICACAO`. A titularidade da conta GitHub não comprova esses papéis. |

## 2. E00 — exercício de aplicação sem pré-especificação retroativa (T02.3)

**Pergunta e estimandos recuperados:** taxas de mortalidade por 100 mil pessoas estimadas, brutas e padronizadas; decomposição da mudança das taxas específicas e da composição populacional. Fonte documental: [caderno, capítulo E09](../reports/mortalidade_sp_0_74/caderno_E01_E10.md). **Não se afirma ter localizado o protocolo E00 original.**

| ID | Explicação candidata para uma variação da taxa bruta | Predição discriminante / fonte | Condição de revisão |
| --- | --- | --- | --- |
| H01 | Variação predominantemente de **composição** idade/sexo, mantidas as taxas específicas | Parcela de composição comparável em magnitude à diferença bruta; decomposição descrita no caderno E09 | Revisar se a parcela das taxas específicas não for desprezível |
| H02 | Variação predominantemente de **taxas específicas** sob composição aproximadamente constante | Parcela das taxas específicas comparável à diferença bruta | Revisar se a parcela de composição for material |
| H03 | Variação relevante dos **dois componentes** | Ambas as parcelas não desprezíveis na mesma comparação e estratificação | Revisar por mudança de pesos, território ou denominadores |

H01–H03 são **hipóteses editoriais ilustrativas formuladas após a existência dos resultados**, não hypotheses pré-registradas, mutuamente excludentes sob todos os limiares, nem modelos probabilísticos comparados. **Não atribuir** probabilidades posteriores, fatores de Bayes ou poder discriminante já demonstrado. Critério quantitativo de “predominância”, conjunto exaustivo de alternativas e autoridade decisória: `PENDENTE_DE_VERIFICACAO`.

| Decisão E00 | Estado local | Critério de fechamento ainda necessário |
| --- | --- | --- |
| D00-01 Fixar estimandos e recortes | `ABERTA` | Evidência do planejamento original, definição das comparações e validação da autoridade |
| D00-02 Fixar hipóteses/limiares de escolha | `ABERTA` | Critérios justificáveis, discussão de não identificação e decisão antes de nova análise |
| D00-03 Autorizar inferência ou publicação adicional | `ABERTA` | Homologação da proveniência/privacidade, revisão científica e responsável comprovado |

**Riscos:** junção territorial provisória, denominadores aproximados, interpretação causal indevida e critérios pós-hoc. **Regra de não decisão:** sem prova dos gates, manter decisões abertas; nenhum resultado histórico muda.

## 3. Ponte E06 → E07 — amostra rastreável, NÃO matriz completa (T02.4)

A [síntese E06 do caderno](../reports/mortalidade_sp_0_74/caderno_E01_E10.md) informa que o documento matemático E06 histórico **não está versionado**; logo não se pode alegar equivalência integral entre equação E06 e implementação E07. As linhas abaixo são exemplos de **localização no código**, não correspondência validada.

| ID | Classe | Evidência E07 em R no SHA-base | Correspondência matemática E06 e estado local |
| --- | --- | --- | --- |
| M06-P01 | LIKELIHOOD | [`01_E07_ajustar_modelos.R`, linhas 41–43](mortalidade_sp_0_74/01_E07_ajustar_modelos.R#L41-L43): `glm(..., family=poisson)` e `MASS::glm.nb` | E06 original não localizado no Git; `ABERTA` / `PENDENTE_DE_VERIFICACAO`, sem equivalência certificada |
| M06-P02 | TRANSFORMACAO_RESTRICAO | [`01_E07_ajustar_modelos.R`, linha 24](mortalidade_sp_0_74/01_E07_ajustar_modelos.R#L24): `offset(log(populacao))` | Restrições de exposição e correspondência matemática de E06 não auditadas; `ABERTA` |
| M06-P03 | GENERATED_QUANTITY/SAIDA | [`01_E07_ajustar_modelos.R`, linhas 73–78](mortalidade_sp_0_74/01_E07_ajustar_modelos.R#L73-L78): arquivos CSV de validação e sessionInfo | Saídas documentadas no código; execução neste ciclo e mapeamento de E06 `PENDENTE_DE_VERIFICACAO` |

**Cobertura faltante:** equações E06, dados e índices completos, parâmetros, priors (quando aplicáveis), suporte, unidades, demais transformações e linhas E07 devem ser inventariados em auditoria científica posterior. Stan **não é** empregado neste estudo conforme README; isto **não** dispensa examinar a implementação R. A taxonomia local `ABERTA` não equivale a status E06 ou E07 aprovado.

## 4. Ponte E08 → E09 — uma afirmação de demonstração, NÃO inventário completo (T02.5)

| ID | Afirmação E09 e fonte | Evidência/diagnóstico E08 | Sensibilidade e limite | Situação local / diligência |
| --- | --- | --- | --- | --- |
| A08-P01 | A NB2 previu 42.279,93 óbitos em 2021, ante 61.166 observados; [relatório E10](../reports/mortalidade_sp_0_74/relatorio_cientifico.md), seção Resultados | [Código E08](mortalidade_sp_0_74/02_E08_diagnosticar.R#L10-L20) e [matriz](../reports/mortalidade_sp_0_74/matriz_evidencias.md) documentam validação histórica, **não execução nesta tarefa**; ligação numérica original E08→E09 não recalculada | Contraste preditivo **não é excesso causal**; falta de fontes E04, conferência de E07 e avaliação científica específica da cobertura | `ABERTA` / D4: diagnóstico e cadeia completa ainda não verificados nesta tarefa; conferir E07→E08→E09, versões, arredondamentos e sensibilidade |

Nenhuma posterior conjunta, probabilidade causal, versão de MCMC ou robustez foi inferida. O contrato exige uma linha por **cada** afirmação E09; **A08-P01 é apenas a demonstração**, e todas as outras afirmações continuam por rastrear. O [caderno](../reports/mortalidade_sp_0_74/caderno_E01_E10.md) distingue o intervalo do bootstrap da cobertura celular de um intervalo para o total de mortes.

## 5. Estados E00–E10 (T02.6): adoção conservadora por etapa

**Objeto do estado:** documentação e evidências **suficientes para cada etapa**, não mera existência de arquivo. `APROVADO` requer QA independente da etapa e cumprimento de seus critérios específicos. O piloto documental não produziu tal QA; nenhuma etapa é promovida a `APROVADO` ou `RESSALVA` por conta da CI. Não atribuir `INDISPONIVEL_NO_HISTORICO` sem busca histórica registrada; o caderno já registra ausências pontuais, mas não há diligência integral nesta tarefa.

| Etapa | Estado | Fonte consultada e motivo de pendência | Próxima diligência |
| --- | --- | --- | --- |
| E00 | `PENDENTE` | Questão reconstruída do E09; E00 original e decisões não verificados | Identificar autoridade e protocolo original, se disponível |
| E01 | `PENDENTE` | [Caderno](../reports/mortalidade_sp_0_74/caderno_E01_E10.md) recupera delimitação, não relatório original | Buscar origem e revisar recorte |
| E02 | `PENDENTE` | Esquema celular documentado, sem caderno autônomo verificado | Inspecionar contrato original sem dados restritos |
| E03 | `PENDENTE` | Qualidade/território documentados parcialmente | Homologar equivalência territorial e proveniência |
| E04 | `PENDENTE` | Base derivada e insumos não disponíveis para verificação nesta tarefa | Conferir entradas em ambiente autorizado |
| E05 | `PENDENTE` | Exploração recuperada de relatórios posteriores | Rastrear origem E05 |
| E06 | `PENDENTE` | Modelo resumido; documento E06 original não versionado | Obter E06 original, comparar com código |
| E07 | `PENDENTE` | Código Poisson/NB2 no Git, sem reajuste do ciclo | Conferir especificações, execução/diagnósticos |
| E08 | `PENDENTE` | Script e referência histórica, sem dados E04/E07 completos nesta tarefa | Validar E08, sensibilidade, cobertura e fontes |
| E09 | `PENDENTE` | Relatório e tabelas publicados; sem reconciliação exaustiva de afirmações | Auditar matriz E08→E09 por afirmação |
| E10 | `PENDENTE` | Caderno/manifesto e auditoria agregada descritos; sem nova reprodução completa | Verificar níveis de reprodutibilidade e relatório |

## 6. Resultado do piloto, defeitos de adoção e próximos gates

**Critérios editoriais verificáveis neste PR:** existem os oito metadados; há exemplos E00 de alternativas e decisões abertas; localizadores reais E07; ao menos uma afirmação E09 com fonte, diagnóstico condicionado, sensibilidade e limite; onze estados explícitos com fontes e ações; links relativos íntegros; teste estrutural específico. Isto testa **a aplicação documental** dos seis contratos, não a suficiência científica das evidências.

**Lacunas detectadas pelo uso:** (1) E00 e E06 originais não versionados; (2) correspondência matemática E06→E07 não completa; (3) a matriz E08→E09 está demonstrada por apenas uma afirmação; (4) documentação E01–E05 é retrospectiva e incompleta; (5) direitos, correspondência territorial e dados externos não homologados; (6) owner/QA científico não comprovados; (7) Rscript indisponível no ambiente executor, devendo o PR depender de CI explícita. São **bloqueios da aprovação de etapas científicas**, não impedimentos para adotar a política documental no CONTRIBUTING.

**Decisão T02.7:** executar apenas a formalização editorial e demonstrar uso limitado do contrato com rastreabilidade e pendências. **O QA da T02.7 depende de parecer técnico separado, registrado no PR #29; a homologação das etapas científicas continua pendente.** Issue [#14](https://github.com/riscotta/bayes-stan-r/issues/14), T01.3–T01.7 e T03.* continuam abertas. Não republicar dados nem modificar saídas históricas.
