# T02.4 — Matriz obrigatória de rastreabilidade E06 → E07

**Finalidade:** documentar, por unidade de estudo e por variante de modelo, a correspondência auditável entre formulação matemática em E06 e implementação efetiva em E07 (Stan, R ou outra interface explicitada). O formulário abaixo é **um contrato vazio**, não uma inspeção de modelos existentes, nem uma validação inferencial. A aplicação a um estudo piloto e sua incorporação normativa ao CONTRIBUTING pertencem à T02.7.

**Vínculos:** [contrato mínimo T02.2](STUDY_METADATA_CONTRACT_T02_2.md) · [modelo de decisões E00 T02.3](STUDY_E00_TEMPLATE_T02_3.md) · [taxonomia T02.1](STUDY_TAXONOMY_T02_1.md) · [Issue #14 — dados](https://github.com/riscotta/bayes-stan-r/issues/14). Não confundir esta matriz com a ligação de evidências E08 → E09 (T02.5) nem com estados por etapa E00–E10 (T02.6).

## 1. Identificação, fontes e critério de cobertura

- **Unidade/identificador histórico e classe editorial:** `PENDENTE_DE_VERIFICACAO`; fonte e SHA: `PENDENTE_DE_VERIFICACAO`.
- **Modelo e variante (identificador não ambíguo):** `PENDENTE_DE_VERIFICACAO`; versão, data e responsável comprovado: `PENDENTE_DE_VERIFICACAO`.
- **Natureza da matriz:** `DOCUMENTACAO_ATUAL` ou `RECONSTRUCAO_EDITORIAL`; justificar e datar. Não apresentar reconstrução como registro original.
- **Fonte E06 (caminho, seção/linha, versão, expressão):** `PENDENTE_DE_VERIFICACAO`.
- **Fonte E07 (arquivo(s) Stan/R, caminho, bloco/linha, SHA):** `PENDENTE_DE_VERIFICACAO`.
- **Entrada e comando efetivamente utilizados, se houve execução:** `PENDENTE_DE_VERIFICACAO`; ambiente, seed, versão de Stan/R e evidências: `PENDENTE_DE_VERIFICACAO`.
- **Abrangência:** cada expressão E06 e cada bloco inferencial relevante E07 devem aparecer na matriz, com IDs estáveis, inclusive itens sem correspondência, alterações de parametrização e quantidades derivadas. A documentação não deve inventar equações E06 ausentes.
- **Ausências:** `PENDENTE_DE_VERIFICACAO` se falta diligência; `INDISPONIVEL_NO_HISTORICO` somente após registrar busca e fontes; `NAO_APLICAVEL` somente com razão matemática/operacional verificável. Nenhum dos três constitui aprovação.

A cobertura mínima inclui **equações**, **dados e índices**, **parâmetros**, **priors**, **likelihood**, **transformações e restrições** e **generated quantities**. Para cada classe, registrar toda ocorrência pertinente ou uma linha `NAO_APLICAVEL` justificada após inspeção; não basta uma linha genérica por categoria. Modelos somente em R devem explicitar a ausência de Stan e mapear as operações computacionais equivalentes.

## 2. Inventário de fontes e versões do modelo

| ID | Tipo de artefato | Caminho/URL, símbolo e linhas | Commit/hash, data e natureza histórica/editorial | Inspeção efetiva e limite |
| --- | --- | --- | --- | --- |
| F06-01 | Formulação matemática E06 | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` |
| F07-01 | Stan (arquivo .stan ou string compilada em R) | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` |
| F07-02 | R: preparação, chamadas e pós-processamento | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` |
| FE-01 | Execução, diagnóstico ou teste, se disponível | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` |

Arquivos e linhas devem apontar para o **mesmo SHA** de código auditado. Se o arquivo for produzido dinamicamente, identificar função geradora, versão, argumento e materialização verificável. Um link para a pasta ou README não prova correspondência matemática.

## 3. Matriz principal — uma linha por correspondência verificável

**Regra:** duplicar linhas para *cada* equação, parâmetro, prior, termo de likelihood, transformação, bloco de dados ou quantity. Uma relação um-para-muitos usa IDs relacionados; jamais condensar termos matematicamente diferentes em “modelo conforme”. Os IDs M06 abaixo são exemplos de linhas vazias, não a cardinalidade exigida.

| ID | Classe E06 | Expressão E06, símbolo, suporte e unidades | E07: arquivo, bloco, linhas, símbolo/operador | R: origem, transformação, indexação e dimensão | Parametrização, restrições e mapeamento E06→E07 | Evidência e versão | Situação local e divergência |
| --- | --- | --- | --- | --- | --- | --- | --- |
| M06-01 | EQUACAO | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `ABERTA` — motivo: não auditado |
| M06-02 | DADO_INDICE | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `ABERTA` — motivo: não auditado |
| M06-03 | PARAMETRO | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `ABERTA` — motivo: não auditado |
| M06-04 | PRIOR | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `ABERTA` — motivo: não auditado |
| M06-05 | LIKELIHOOD | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `ABERTA` — motivo: não auditado |
| M06-06 | TRANSFORMACAO_RESTRICAO | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `ABERTA` — motivo: não auditado |
| M06-07 | GENERATED_QUANTITY | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `ABERTA` — motivo: não auditado |

**Campos obrigatórios por linha:** ID único; classe; localizador e expressão E06; localizador e símbolo E07 ou ausência justificada; transformação/preparação R quando aplicável; dimensões, unidade, suporte e indexação; correspondência declarada; versão/hash; evidência de inspeção; estado local e justificativa. Um símbolo idêntico não comprova igualdade de parametrização.

## 4. Verificações especializadas por classe

| Classe | Perguntas de confronto obrigatório | Não declarar equivalência se faltar |
| --- | --- | --- |
| EQUACAO | Definições de variáveis, condicionantes, estrutura hierárquica, independências, termos determinísticos e normalizações coincidem? | Derivação/localizador de cada igualdade ou mudança de escala |
| DADO_INDICE | Origem e versão da entrada, filtros, unidade analítica, N/K, tipos, índices 1-based, missingness, pesos, offsets e agregação estão reconciliados? | Forma e ordenação das matrizes/vetores e trilha R→Stan |
| PARAMETRO | Nome, dimensão, suporte, interpretação e restrição de identificação coincidem? | Mapa de nomes, escala, hiperparâmetros e parâmetros identificáveis |
| PRIOR | Distribuição, parâmetros de escala (desvio/variância/taxa), truncamento, suporte, dependência e transformação estão equivalentes? | Checagem de prior induzida/Jacobiano quando pertinente |
| LIKELIHOOD | Família, link, parametrização, constante, censura/truncamento, observações repetidas, dependência e ponderação coincidem? | Derivação da densidade/massa e do target +=/amostragem correspondente |
| TRANSFORMACAO_RESTRICAO | Centramento/não centramento, padronização, inversão, ordenação e jacobiano preservam modelo e escala? | Transformação direta/inversa e medida probabilística |
| GENERATED_QUANTITY | Formulações de log_lik, y_rep, predições, contrastes, quantidades agregadas e uso downstream concordam com E06? | Suporte, dimensão, unidade, condicionamento e proveniência dos sorteios |

Conferir também blocos Stan `data`, `transformed data`, `parameters`, `transformed parameters`, `model` e `generated quantities`, se existentes, bem como o R responsável pela preparação e pelos nomes de saída. Ausência de `generated quantities` é `NAO_APLICAVEL` apenas com justificativa e sem alegação de PPC ou log_lik não calculado. A matriz distingue **prior preditiva**, **posterior preditiva** e **simples transformação determinística**, sem tratá-las como intercambiáveis.

## 5. Critérios de divergência e impacto

| Código | Critério objetivo | Consequência / ação |
| --- | --- | --- |
| D0 — ABERTA | Evidência, fonte, versão ou correspondência ainda não inspecionada. | `ABERTA`; registrar diligência e não afirmar equivalência. |
| D1 — DOCUMENTAL | Notação ou nome diferente com transformação bijetiva demonstrada e mesma distribuição. | Documentar prova e referência; pode ser `CONFORME_CODIGO` após inspeção completa. |
| D2 — SUBSTANTIVA | Prior, likelihood, suporte, dimensão, escala, filtragem, indexação, transformação/Jacobiano ou generated quantity diferentes sem equivalência demonstrada. | `DIVERGENCIA`; bloquear alegação de correspondência e registrar impacto inferencial. |
| D3 — EXECUCAO | Código inspecionado, mas comando, artefato, hash, diagnóstico ou saída contradiz a alegação de execução. | Não classificar como `CONFORME_EXECUCAO`; abrir diligência e preservar histórico. |
| D4 — INDISPONIBILIDADE | E06 ou E07 original ausente após busca documentada, ou acesso legitimamente restrito. | `INDISPONIVEL_NO_HISTORICO` / `PENDENTE_DE_VERIFICACAO`; especificar impossibilidade e fronteira do parecer. |

**Estados locais da linha (não são os estados de etapa T02.6):** `ABERTA`, `CONFORME_CODIGO`, `CONFORME_EXECUCAO`, `DIVERGENCIA` ou `NAO_APLICAVEL` (motivo verificável). `CONFORME_CODIGO` requer confronto matemático com o arquivo efetivamente lido no SHA indicado. `CONFORME_EXECUCAO` exige adicionalmente execução identificável, entradas/versões acessíveis e teste pertinente, mas **não implica** inferência válida ou MCMC convergido. Divergência não se resolve mudando arquivos científicos neste ciclo documental.

## 6. Evidências, decisões e transferência para E07

| ID | Linha(s) M06 / decisão | Fonte/artefato/comando, SHA e linhas | Tipo e resultado efetivamente verificado | Limite, responsável comprovado e próxima diligência |
| --- | --- | --- | --- | --- |
| EV06-01 | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` |
| DEC06-01 | Autorizar ou suspender afirmação de equivalência | `PENDENTE_DE_VERIFICACAO` | `ABERTA` — sem parecer; não é aprovação | `PENDENTE_DE_VERIFICACAO` |

**Registro de lacunas:** `PENDENTE_DE_VERIFICACAO` — ID da linha, motivo, fontes consultadas, impacto, responsável comprovado (se houver), critério de resolução e data. Não registrar nomes, datas de execução ou resultados por inferência. Evidência pública não pode conter registros institucionais, credenciais ou dados sensíveis; respeitar a Issue #14 e os gates T01.3–T01.7.

## 7. Checklist de aceite por estudo e fronteira da T02.4

- [ ] Inventário E06/E07 identifica variante e SHA, com origem contemporânea ou reconstrução editorial.
- [ ] Todas as expressões e classes relevantes estão ligadas por IDs únicos ou ausência justificada, sem omitir itens sem equivalente.
- [ ] Cada linha identifica E06 e Stan/R por caminhos/linhas, além de formas, dimensões, indexação, suporte, parametrização e transformações.
- [ ] Priors, likelihood e generated quantities foram confrontadas com sua expressão E06; diferenças registradas com tipo e impacto.
- [ ] Evidência distingue leitura de código, execução, diagnósticos e revisão científica; links não substituem inspeção.
- [ ] Divergências substanciais permanecem abertas até decisão científica formal; nenhuma reescrita de histórico, dados ou resultados foi feita para passar no contrato.
- [ ] Parecer independente identifica SHA e limitações antes de qualquer alegação de QA por estudo.

**Aceitação documental deste artefato T02.4:** o modelo existe, pode ser copiado, tem todos os campos/classes e critérios de divergência, está vinculado em [scripts/README.md](README.md) e passa pelos testes estruturais aplicáveis na CI. **Não comprova** que qualquer estudo já foi mapeado ou executado; a validação do piloto pertence à T02.7. T02.5, T02.6 e T02.7 permanecem não executadas.
