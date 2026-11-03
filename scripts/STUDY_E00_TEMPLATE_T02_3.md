# T02.3 — Modelo editável de E00: decisões e hipóteses concorrentes

**Escopo:** preparar um E00 por unidade de estudo, com pergunta, hipóteses alternativas, decisões fecháveis, critérios, riscos e pendências justificadas. **Não** é E00 executado de nenhum estudo e não reconstitui documentação histórica. A validação editorial e adoção em um piloto pertencem à T02.7.

**Contrato relacionado:** [oito metadados T02.2](STUDY_METADATA_CONTRACT_T02_2.md). **Categoria:** [taxonomia T02.1](STUDY_TAXONOMY_T02_1.md). **Governança de dados:** [Issue #14](https://github.com/riscotta/bayes-stan-r/issues/14). O preenchimento não altera estado de licença, privacidade, QA, inferência ou publicação.

## Instruções de uso

1. Copiar este Markdown para o local do **novo** planejamento E00, preservando o identificador histórico da unidade; não criar nem preencher fichas para os 33 diretórios no âmbito da T02.3.
2. Registrar fonte verificável para cada afirmação e distinguir `DOCUMENTACAO_ATUAL` de `RECONSTRUCAO_EDITORIAL` (esta última não deve ser atribuída à época original). Indicar data e autor do preenchimento **somente se comprovados**.
3. Para campo ou decisão sem evidência, usar `PENDENTE_DE_VERIFICACAO` com motivo, fonte consultada, responsável por diligência se confirmado e condição objetiva para fechamento; usar `INDISPONIVEL_NO_HISTORICO` quando as fontes históricas examinadas não contêm o registro. `NAO_APLICAVEL` exige justificativa, nunca significa informação desconhecida.
4. Fechar uma decisão apenas se houver critério previamente declarado, evidência localizável, autoridade identificada e registro explícito do motivo. Sem esses quatro itens, o estado da **decisão** permanece `ABERTA`. `FECHADA` e `REABERTA` são rótulos locais de decisão, **não** taxonomia do estado E00–E10 (T02.6).
5. Não atribuir probabilidades, desempenho, aprovações, dados, owners, licenças ou resultados que não tenham sido comprovados. Não inserir conteúdos institucionais restritos no GitHub.

## 1. Identificação e contexto dos oito metadados (T02.2)

- **Pasta/identificador histórico:** `scripts/<identificador>/` — `PENDENTE_DE_VERIFICACAO`.
- **Categoria editorial:** `Laboratório de modelos` ou `Estudo aplicado` — `PENDENTE_DE_VERIFICACAO`; fonte: taxonomia T02.1.
- **Natureza do registro:** `DOCUMENTACAO_ATUAL` ou `RECONSTRUCAO_EDITORIAL` — `PENDENTE_DE_VERIFICACAO`; data/fonte: `PENDENTE_DE_VERIFICACAO`.

| Campo obrigatório T02.2 | Conteúdo específico da unidade | Evidência ou razão da lacuna |
| --- | --- | --- |
| **Pergunta** | `PENDENTE_DE_VERIFICACAO` | Fonte e diligência: `PENDENTE_DE_VERIFICACAO` |
| **Objeto** | `PENDENTE_DE_VERIFICACAO` | Estimando/unidade/recorte e fonte: `PENDENTE_DE_VERIFICACAO` |
| **Dados** | `PENDENTE_DE_VERIFICACAO` | Tipo, origem, versão, acesso, restrições: `PENDENTE_DE_VERIFICACAO` |
| **Método** | `PENDENTE_DE_VERIFICACAO` | Família e pressupostos declarados: `PENDENTE_DE_VERIFICACAO` |
| **Estado** | `PENDENTE_DE_VERIFICACAO` | Documentação, execução, QA e validação separadamente: `PENDENTE_DE_VERIFICACAO` |
| **Resultados** | `PENDENTE_DE_VERIFICACAO` | Artefato existente ou ausência justificada: `PENDENTE_DE_VERIFICACAO` |
| **Limites** | `PENDENTE_DE_VERIFICACAO` | Acesso, identificação, generalização: `PENDENTE_DE_VERIFICACAO` |
| **Responsável** | `PENDENTE_DE_VERIFICACAO` | Papel, nome e comprovação da atribuição: `PENDENTE_DE_VERIFICACAO` |

## 2. Pergunta, estimando e escopo decidível

- **Pergunta literal e fonte:** `PENDENTE_DE_VERIFICACAO`.
- **Unidade de análise, população, universo ou processo gerador:** `PENDENTE_DE_VERIFICACAO`.
- **Estimando(s), unidade de medida, período e condicionantes:** `PENDENTE_DE_VERIFICACAO`.
- **Decisão a informar e destinatário:** `PENDENTE_DE_VERIFICACAO` (não inferir o destinatário pelo autor do GitHub).
- **Fronteira do que o E00 não pretende afirmar:** `PENDENTE_DE_VERIFICACAO`.

Para **Laboratório de modelos**, pergunta e estimando podem ser metodológicos, e dados podem ser sintéticos; explicitar gerador, condições e limites da demonstração. Para **Estudo aplicado**, especificar fenômeno, população/período e implicações da amostragem e da proveniência quando conhecidos. Os dois tipos exigem evidência e não pressupõem um estudo completo.

## 3. Hipóteses concorrentes e espaço de modelos

Preencher ao menos duas explicações **substantivamente diferentes**, incluindo sua consequência observável e a hipótese de trabalho associada. Diferenciar hipóteses mutuamente excludentes de hipóteses sobrepostas; não afirmar exaustividade sem argumento. Os rótulos abaixo são **lugares de preenchimento**, não hipóteses reais nem modelos ajustados.

| ID | Hipótese/explicação ou modelo candidato | Predição ou implicação discriminante | Pressupostos, identificação e evidência inicial | Condição de revisão |
| --- | --- | --- | --- | --- |
| H01 | `PENDENTE_DE_VERIFICACAO` — explicação candidata 1 | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` |
| H02 | `PENDENTE_DE_VERIFICACAO` — explicação candidata 2 | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` |
| H03 | `PENDENTE_DE_VERIFICACAO` — outra explicação/plano de inadequação, se pertinente | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` |

- **Relação entre H01/H02/H03 (exclusivas, sobrepostas, não exaustivas) e justificativa:** `PENDENTE_DE_VERIFICACAO`.
- **Como pesos prévios de hipóteses/modelos e priors seriam justificados, se aplicáveis:** `PENDENTE_DE_VERIFICACAO`. Não inserir probabilidades arbitrárias só para completar o modelo.
- **Quais observações/diagnósticos distinguiriam as hipóteses e quais não distinguiriam:** `PENDENTE_DE_VERIFICACAO`.
- **Risco de não identificação, equivalência observacional ou dependência de prior:** `PENDENTE_DE_VERIFICACAO`.
- **Nenhum resultado de comparação, probabilidade posterior ou ajuste deve ser antecipado no E00.** Especificações matemáticas detalhadas pertencem ao trabalho científico subsequente, não a esta tarefa documental.

## 4. Critérios de escolha, revisão e não decisão

- **Regra de comparação proposta:** `PENDENTE_DE_VERIFICACAO` — explicitar objeto da comparação, medida pertinente (preditiva, evidência de modelo ou decisão com perdas) e suas limitações.
- **Critério de escolha e limiar, quando justificável:** `PENDENTE_DE_VERIFICACAO` — unidade, direção, justificativa e análise de sensibilidade; não inventar números.
- **Critério de rejeição, descarte ou revisão:** `PENDENTE_DE_VERIFICACAO` — teste de adequação, falha de identificação, extrapolação, sensibilidade a priors, ou evidência conflitante; distinguir rejeitar uma formulação de provar sua alternativa.
- **Condição de `NAO_DECIDIR`:** `PENDENTE_DE_VERIFICACAO` — evidência insuficiente, hipótese não distinguível, risco de dados, diagnóstico inadequado ou ausência de responsável autorizado.
- **Como a decisão seria reaberta diante de nova evidência:** `PENDENTE_DE_VERIFICACAO`.

Uma probabilidade sobre `H_i` só pode ser reportada se `H_i`, o conjunto concorrente, os pressupostos e o cálculo correspondente estiverem definidos e verificáveis. Ajuste posterior de limiares deve ser registrado como revisão, não como critério pré-especificado.

## 5. Registro de decisões fecháveis

Cada decisão exige **critério**, **evidência**, **responsável autorizado** e **motivação**. Uma decisão é `FECHADA` apenas depois de preencher esses quatro elementos e a data do fechamento. `ABERTA` não significa falha: impede inventar aprovação.

| ID | Decisão específica e alternativas | Critério verificável de fechamento | Evidência (link/caminho/versão) | Responsável e prova de atribuição | Estado da decisão, data e justificativa |
| --- | --- | --- | --- | --- | --- |
| D00-01 | Delimitar pergunta, estimando e recorte — `PENDENTE_DE_VERIFICACAO` | Fonte e escopo coerentes com a pergunta | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `ABERTA`; motivo: falta evidência |
| D00-02 | Fixar conjunto e relação das hipóteses — `PENDENTE_DE_VERIFICACAO` | Hipóteses distinguíveis ou impossibilidade explicitada | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `ABERTA`; motivo: formulação não comprovada |
| D00-03 | Definir regra de comparação, perdas/limiares e condição de não decisão — `PENDENTE_DE_VERIFICACAO` | Critérios documentados antes da análise e justificáveis | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `ABERTA`; motivo: critérios ainda não fundamentados |
| D00-04 | Aprovar escopo, riscos e dependências para avançar — `PENDENTE_DE_VERIFICACAO` | Riscos e gates aplicáveis classificados, sem inferir autorização | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `ABERTA`; motivo: autoridade e provas não verificadas |

**Histórico de mudança de decisão:** `PENDENTE_DE_VERIFICACAO` — ID, versão anterior, versão nova, motivação, fonte e autoridade. Nunca substituir silenciosamente decisões históricas.

## 6. Riscos e medidas condicionantes

| ID | Risco ou viés a avaliar | Consequência para decisão | Mitigação e evidência exigida | Situação justificada |
| --- | --- | --- | --- | --- |
| R00-01 | Proveniência, licença, privacidade e acesso | Impede uso ou publicação sem gates | Fonte, autorização e controle institucional, quando aplicáveis | `PENDENTE_DE_VERIFICACAO` |
| R00-02 | Identificação, prior e especificação | Hipóteses podem não ser discrimináveis | Diagnósticos, cenários e sensibilidade planejados | `PENDENTE_DE_VERIFICACAO` |
| R00-03 | Amostra, unidade, desenho e extrapolação | Decisão pode não valer para população pretendida | Critérios de qualidade, recortes e limites declarados | `PENDENTE_DE_VERIFICACAO` |

**Gatilho de suspensão:** se acesso ou redistribuição de dados não estiverem homologados, interromper a ação dependente; o E00 em si não concede permissão.

## 7. Dependências, pré-condições e responsáveis

| Dependência | Artefato/terceiro necessário | Evidência de disponibilidade | Quem pode remover o bloqueio (se comprovado) | Estado e próximo ato |
| --- | --- | --- | --- | --- |
| Definição da pergunta e do estimando | Formulação original/estudo | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` |
| Insumos e direitos de uso, se houver | Fonte e gate institucional | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` |
| Estratégia de discriminação de hipóteses | Protocolo metodológico futuro | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` |

## 8. Índice de evidências auditáveis

| ID | Afirmação/decisão apoiada | Localizador relativo, URL ou comando | Versão, data e limites da fonte | Comprovação da leitura/execução |
| --- | --- | --- | --- | --- |
| EV00-01 | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` |

**Observação:** um README prova apenas o que ele documenta. Links existentes não equivalem a execução de Stan/MCMC, inspeção de dados, licença ou revisão científica. Não colar dados sensíveis, laudos ou credenciais nas evidências públicas.

## 9. Registro de pendências justificadas

| ID | Lacuna e sua relevância para E00 | Motivo e fontes já consultadas | Estado da informação | Ação possível, responsável comprovado e critério de encerramento |
| --- | --- | --- | --- | --- |
| P00-01 | Identidade do responsável por decisão | `PENDENTE_DE_VERIFICACAO` — não presumir autoria a partir do GitHub | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` |
| P00-02 | Comprovação de hipóteses/estimandos no histórico | `PENDENTE_DE_VERIFICACAO` — especificar arquivos consultados | `INDISPONIVEL_NO_HISTORICO` **somente após busca documentada**, senão `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` |
| P00-03 | Dependência institucional, se pertinente | `PENDENTE_DE_VERIFICACAO` — não expor documentos | `NAO_APLICAVEL` **apenas com justificativa**, senão `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` |

## 10. Critério de saída e transferência de E00

- [ ] Pergunta, objeto, recorte e estimandos documentados ou lacunas explicitadas com fonte e diligência.
- [ ] Pelo menos duas hipóteses concorrentes definidas, com consequências observáveis, condições de distinção e limitações.
- [ ] Critério de escolha, revisão e `NAO_DECIDIR` justificado **antes** da análise, ou impossibilidade documentada.
- [ ] Cada decisão `FECHADA` tem critério atendido, evidência, autoridade comprovada, motivação e data; demais decisões `ABERTA`.
- [ ] Riscos, dependências e pendências com condição objetiva de resolução; nenhuma lacuna ocultada.
- [ ] Fontes linkáveis sem exposição de informações restritas; status de execução, QA e validade científica não presumidos.

**Resultado deste arquivo T02.3:** um **modelo vazio e verificável**, não um E00 preenchido, aprovado ou cientificamente validado. Matrizes E06→E07 / E08→E09, formalização de estados E00–E10 e política no `CONTRIBUTING.md` pertencem, respectivamente, às tarefas T02.4, T02.5, T02.6 e T02.7.
