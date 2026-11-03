# T02.6 — Contrato de estados por etapa E00–E10

**Objetivo:** especificar um estado verificável para cada etapa de cada unidade de estudo, sem atribuir estados a estudos históricos sem inspeção. **Natureza:** formulário editorial vazio, não auditoria, homologação científica, aprovação de dados ou reconstrução de registros originais. A aplicação em estudo piloto e a norma no `CONTRIBUTING.md` pertencem à **T02.7**.

**Vínculos:** [taxonomia T02.1](STUDY_TAXONOMY_T02_1.md) · [oito metadados T02.2](STUDY_METADATA_CONTRACT_T02_2.md) · [E00 T02.3](STUDY_E00_TEMPLATE_T02_3.md) · [E06→E07 T02.4](STUDY_E06_E07_MATRIX_T02_4.md) · [E08→E09 T02.5](STUDY_E08_E09_MATRIX_T02_5.md) · [catálogo](README.md) · [Issue #14 — direitos e privacidade](https://github.com/riscotta/bayes-stan-r/issues/14).

## 1. Unidade, escopo e separação dos controles

- **Unidade:** uma pasta histórica `scripts/<identificador>/`, classe `Laboratório de modelos` ou `Estudo aplicado`, variante/versão e SHA específicos. A classificação editorial da T02.1 não aprova nenhuma etapa.
- **Granularidade:** exatamente um registro de estado para cada E00–E10 na versão avaliada. Registrar a ausência de evidência sem completar etapas retroativamente. Não inventar relatórios contemporâneos.
- **Três níveis distintos:** (a) estado **da etapa de um estudo** definido aqui; (b) estados **locais de linhas ou decisões** dos modelos T02.3–T02.5 (`ABERTA`, `CONFORME_CODIGO`, `CONFORME_EXECUCAO`, `DIVERGENCIA` etc.); (c) caixas **Exec./QA da tarefa do Plano**. Nenhum é convertido automaticamente em outro.
- **Dimensões adicionais separadas:** existência documental, execução/reprodução, parecer técnico, validade científica e governança de dados/licença. O estado de etapa nunca substitui esses registros. A CI verde, por si só, não dá `APROVADO`.
- **Base de preenchimento:** caminhos relativos, SHA/hash, versão, linhas, data, comandos e resultados observados; distinguir `DOCUMENTACAO_ATUAL` de `RECONSTRUCAO_EDITORIAL`. Identificação do revisor, sua independência e alcance precisam de evidência; não presumir aprovação formal de terceiro.
- **Regra de segurança:** não transcrever registros individuais, credenciais nem insumos restritos para obter evidências públicas. A Issue #14 e T01.3–T01.7 permanecem bloqueios separados quando pertinentes.

## 2. Dicionário fechado de cinco estados

| Código canônico | Condição necessária | Evidência mínima e decisão | O que NÃO significa |
| --- | --- | --- | --- |
| `APROVADO` | Entregáveis e verificações relevantes da etapa integralmente atendidos no escopo declarado; nenhuma ressalva material aberta. | Fontes versionadas; checklist aplicável comprovado; execução/diagnósticos quando a etapa os exige; parecer independente favorável identificável, data e SHA exato. | Nem licença homologada nem validade universal do modelo; não decorre da CI. |
| `RESSALVA` | Conteúdo central verificado, mas existe limitação específica aceita expressamente pelo revisor para um uso delimitado. | Mesmo inventário de fontes e parecer independente com ressalva, impacto, uso autorizado, condição de saneamento e responsável. Ressalva que inviabiliza o objetivo central exige `PENDENTE`, não este estado. | Não é aprovação plena, autorização tácita de dados ou ocultação de falha inferencial. |
| `PENDENTE` | Evidência, execução, diagnóstico, autorização pertinente ou revisão necessária ausente, incompleta, contraditória ou não examinada. **Estado inicial de toda etapa ainda não auditada.** | Registrar precisamente o que falta, fontes consultadas, impacto, próxima diligência e responsável comprovado se conhecido. Sem parecer favorável, não promover. | Não pressupõe que artefato inexista; não é falha provada nem aprovação. |
| `NAO_APLICAVEL` | A atividade da etapa é materialmente inaplicável à natureza/escopo da unidade, e a exclusão é justificável. | Razão técnica específica, escopo/variante e fonte, decisão revisada e identificada. Em estudo aplicado completo, a ausência de documento ou de execução não é inaplicabilidade. | Não serve para omitir etapa difícil ou dados indisponíveis; não conta como `APROVADO`. |
| `INDISPONIVEL_NO_HISTORICO` | Artefato contemporâneo necessário não foi localizado **após busca documentada** em fontes plausíveis, ou acesso é comprovadamente indisponível/restrito. | Inventário de fontes consultadas e tentativas, datas, hashes quando houver, motivo da indisponibilidade e impacto. Revisor verifica a diligência, não certifica conteúdo ausente. | Não prova que nunca existiu, nem autoriza reconstruir execução/resultado ou marcar `APROVADO`. |

**Precedência conservadora:** evidência contraditória, bloqueio legal ou falha crítica => `PENDENTE`; um arquivo não encontrado sem busca => `PENDENTE`; restrição de acesso sem documentação => `PENDENTE`. Uma etapa só é `NAO_APLICAVEL` para a unidade/variante explicitada, nunca porque a documentação falta. A ausência histórica comprovada restringe a alegação sobre a etapa, sem apagar artefatos que existam.

## 3. Critérios de evidência específicos por etapa

| Etapa | Evidência/entregável a conferir para decidir o estado | Controle ou falha que impede promoção automática |
| --- | --- | --- |
| E00 | Pergunta, população/unidade, estimandos, hipóteses concorrentes, decisões, riscos e critérios de fechamento, com origem. | Hipótese ou decisão editorial não verificada; reconstrução de planejamento apresentada como original. |
| E01 | Fonte, proveniência, permissão de acesso/uso e riscos de privacidade dos insumos. | Direitos, autoridade ou segurança não demonstrados (Issue #14 / T01.*); acesso não equivale a redistribuição permitida. |
| E02 | Inventário/dicionário, versões, estrutura, cobertura, unidades e regras de inclusão/exclusão. | Fonte ou recorte não identificável; campos preenchidos por conjectura. |
| E03 | Preparação, filtros, transformações, dados faltantes e rastreabilidade entrada→base analítica. | Entrada inacessível, transformação não registrada ou reconstituição não verificável. |
| E04 | Exploração e verificações iniciais com código, insumos, saídas e limitações. | Gráfico declarado sem artefato, execução ou origem localizável. |
| E05 | Contratos de análise e verificações de qualidade de dados, suposições e recortes com evidência. | Invariantes ou população efetiva desconhecidos; inconsistências não resolvidas. |
| E06 | Formulação matemática, identificação, suporte, priors, likelihood, alternativas e vínculo com E00. | Equação/prior apenas sugerida, sem evidência da variante usada. |
| E07 | Rastreabilidade E06→Stan/R, comandos, versões, diagnósticos e saídas verificadas se houver inferência. | `CONFORME_CODIGO` não prova `CONFORME_EXECUCAO`; CI não prova posterior reproduzida. |
| E08 | Diagnósticos de inferência e sensibilidade; PPC/R-hat/ESS quando aplicáveis; distinguir cenários aproximados. | Diagnóstico ausente, correlação hipotética tratada como observada ou posterior conjunta não identificada. |
| E09 | Afirmação por afirmação ligada a estimando, evidência E08, hipótese, diagnóstico, sensibilidade e validade. | Matriz E08→E09 vazia, extrapolação ou cenário condicional apresentado como posterior conjunta. |
| E10 | Síntese, níveis de reprodução, manifesto, fontes, conclusões/limitações e decisões de publicação. | Relato sem execução identificável ou sem fronteiras de reprodutibilidade; pendência de licença. |

**Aplicabilidade:** a classificação Laboratório/Estudo aplicado informa a avaliação, mas não torna automaticamente E01–E10 `NAO_APLICAVEL`. Se a etapa não integra legitimamente um exemplo didático, justificar individualmente por que a exclusão não prejudica o objetivo declarado. Não impor dez relatórios históricos onde não existiam.

## 4. Registro de evidências, revisão e transições

Cada alteração de estado exige: identificador do estudo e variante; etapa; estado anterior/novo; artefatos consultados (arquivo, URL/linha, hash/SHA); natureza histórica/editorial; execução (comando, ambiente, seed, resultado) **ou ausência explícita**; achados, restrições e impacto; parecer/revisor e independência verificados; responsável, data, decisão e próxima diligência. Evidência protegida deve permanecer no ambiente autorizado, com referência pública não sensível quando permitido.

**Transições permitidas mediante nova evidência e decisão registrada:** `PENDENTE` → `APROVADO`/`RESSALVA` somente após confronto e parecer independente favorável; `PENDENTE` → `NAO_APLICAVEL` somente com justificativa técnica revisada; `PENDENTE` → `INDISPONIVEL_NO_HISTORICO` somente após busca registrada. `RESSALVA` → `APROVADO` após sanar/aceitar formalmente o risco no escopo; `APROVADO`/`RESSALVA` → `PENDENTE` quando surgirem evidências contraditórias, alteração de escopo, versão ou revogação de autorização. `INDISPONIVEL_NO_HISTORICO`/`NAO_APLICAVEL` → `PENDENTE` quando aparecer nova fonte ou mudar a unidade avaliada. **Nunca editar silenciosamente estado antigo:** manter log imutável de decisões e adicionar registro sucessor.

**Regras de agregação:** não inferir aprovação do estudo pela soma de etapas; apresentar sempre vetor E00–E10 e bloqueios de dados/ciência/reprodução à parte. Uma única etapa `APROVADO` não sana outra `PENDENTE`; `RESSALVA` preserva sua condição de uso. Para cada nova versão, reavaliar o impacto em estados anteriores.

## 5. Modelo copiável — todos os estados iniciais são pendentes

Identificador histórico: `PENDENTE_DE_VERIFICACAO`; classe editorial: `PENDENTE_DE_VERIFICACAO`; variante/escopo, commit e data-base: `PENDENTE_DE_VERIFICACAO`. Tipo de registro: `DOCUMENTACAO_ATUAL` ou `RECONSTRUCAO_EDITORIAL` (escolher e justificar ao preencher). **Linhas abaixo são vazias, sem avaliação de qualquer estudo existente.**

| Etapa | Artefato/fonte e SHA | Execução/diagnóstico verificado | Parecer independente (ID, data, SHA) | Estado | Justificativa, lacuna e impacto | Responsável comprovado e diligência |
| --- | --- | --- | --- | --- | --- | --- |
| E00 | PENDENTE_DE_VERIFICACAO | PENDENTE_DE_VERIFICACAO | PENDENTE_DE_VERIFICACAO | `PENDENTE` | Não auditado | PENDENTE_DE_VERIFICACAO |
| E01 | PENDENTE_DE_VERIFICACAO | PENDENTE_DE_VERIFICACAO | PENDENTE_DE_VERIFICACAO | `PENDENTE` | Não auditado | PENDENTE_DE_VERIFICACAO |
| E02 | PENDENTE_DE_VERIFICACAO | PENDENTE_DE_VERIFICACAO | PENDENTE_DE_VERIFICACAO | `PENDENTE` | Não auditado | PENDENTE_DE_VERIFICACAO |
| E03 | PENDENTE_DE_VERIFICACAO | PENDENTE_DE_VERIFICACAO | PENDENTE_DE_VERIFICACAO | `PENDENTE` | Não auditado | PENDENTE_DE_VERIFICACAO |
| E04 | PENDENTE_DE_VERIFICACAO | PENDENTE_DE_VERIFICACAO | PENDENTE_DE_VERIFICACAO | `PENDENTE` | Não auditado | PENDENTE_DE_VERIFICACAO |
| E05 | PENDENTE_DE_VERIFICACAO | PENDENTE_DE_VERIFICACAO | PENDENTE_DE_VERIFICACAO | `PENDENTE` | Não auditado | PENDENTE_DE_VERIFICACAO |
| E06 | PENDENTE_DE_VERIFICACAO | PENDENTE_DE_VERIFICACAO | PENDENTE_DE_VERIFICACAO | `PENDENTE` | Não auditado | PENDENTE_DE_VERIFICACAO |
| E07 | PENDENTE_DE_VERIFICACAO | PENDENTE_DE_VERIFICACAO | PENDENTE_DE_VERIFICACAO | `PENDENTE` | Não auditado | PENDENTE_DE_VERIFICACAO |
| E08 | PENDENTE_DE_VERIFICACAO | PENDENTE_DE_VERIFICACAO | PENDENTE_DE_VERIFICACAO | `PENDENTE` | Não auditado | PENDENTE_DE_VERIFICACAO |
| E09 | PENDENTE_DE_VERIFICACAO | PENDENTE_DE_VERIFICACAO | PENDENTE_DE_VERIFICACAO | `PENDENTE` | Não auditado | PENDENTE_DE_VERIFICACAO |
| E10 | PENDENTE_DE_VERIFICACAO | PENDENTE_DE_VERIFICACAO | PENDENTE_DE_VERIFICACAO | `PENDENTE` | Não auditado | PENDENTE_DE_VERIFICACAO |

**Log de decisões a adicionar por versão:**

| Etapa / ID | Estado anterior → novo | Fonte, hash, busca, execução e limite | Revisor, parecer, data e independência comprovada | Risco, próximo passo e decisão |
| --- | --- | --- | --- | --- |
| PENDENTE_DE_VERIFICACAO | `PENDENTE` → `PENDENTE` | PENDENTE_DE_VERIFICACAO | PENDENTE_DE_VERIFICACAO | Sem decisão; linha ilustrativa |

## 6. Tratamento de legado e exemplos condicionais (não avaliados)

- **Cenário hipotético A:** se um relatório E08 de estudo histórico não for localizado após consultar árvore/commits, catálogo, saídas e responsáveis acessíveis, registrar fontes/tentativas e considerar `INDISPONIVEL_NO_HISTORICO` para a etapa/versão específica. Não produzir uma falsa execução para ocupar a lacuna.
- **Cenário hipotético B:** se um Laboratório de modelos consistir declaradamente apenas em uma demonstração matemática sem análise empírica, o responsável pode **propor** `NAO_APLICAVEL` para uma etapa determinada, desde que justifique sua irrelevância, apresente fonte e obtenha revisão; a classificação de laboratório não basta.
- **Cenário hipotético C:** se o código E07 existir, mas nenhuma execução de inferência for verificável, `PENDENTE` prevalece para exigências de execução, ainda que as linhas locais de T02.4 documentem conformidade de código. Nunca concluir `APROVADO` pela presença de scripts.

O formulário não foi instanciado em nenhum estudo nesta T02.6. Fontes ausentes, hipóteses não identificadas, estatísticas não calculadas e responsabilidades não confirmadas devem continuar explicitamente pendentes. Informações de privacidade e redistribuição requerem homologação independente do estado editorial.

## 7. Aceitação documental da T02.6 e fronteira do próximo ciclo

- [ ] Cinco estados únicos com semântica, evidência mínima, precedência e restrições inequívocas.
- [ ] Onze etapas E00–E10 com critérios próprios e onze linhas copiáveis, todas inicialmente `PENDENTE`.
- [ ] Regras de transição/reabertura preservam histórico, revisão independente e SHA da versão.
- [ ] Distinção explícita de estados locais T02.3–T02.5, Exec./QA do plano, licenças e validade científica.
- [ ] Links relativos conferidos; teste estrutural R e check obrigatório da CI aprovados no HEAD revisado.
- [ ] Parecer de QA independente sobre este contrato antes de integrar o PR.

**Gate T02.6:** aprovar apenas a especificação do contrato, **não** a aplicação a 33 unidades ou a qualquer estudo. Adotar em piloto e formalizar em `CONTRIBUTING.md` somente na T02.7. O controle de dados da Issue #14 e o exame PNAD T03.* permanecem pendentes e independentes da CI.
