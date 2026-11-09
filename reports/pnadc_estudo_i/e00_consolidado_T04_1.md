# T04.1 — E00 consolidado: PNAD Contínua, Estudo I

**Natureza do registro:** `RECONSTRUCAO_EDITORIAL` em 09/11/2026, a partir de fontes versionadas; **não é** E00 contemporâneo, protocolo pré-registrado, confirmação da intenção original nem aprovação científica.  
**Tarefa literal:** “Consolidar E00 com pergunta literal, população, janelas, hipóteses, estimandos e decisões ainda abertas”.  
**Base documental:** `main` no SHA `c35c2d8e5a69116ad931de00a9805276068fcb97`; os caminhos abaixo identificam a fonte verificável de cada afirmação.  
**Tipo editorial:** `Estudo aplicado`, dentro da taxonomia T02.1; a distinção entre elaboração documental atual e execução histórica é obrigatória.

Este documento aplica ao caso PNAD o [modelo de E00](../../scripts/STUDY_E00_TEMPLATE_T02_3.md) e o [contrato de oito metadados](../../scripts/STUDY_METADATA_CONTRACT_T02_2.md). A ausência do E00 completo original entre os arquivos versionados **não autoriza** reconstruir decisões antigas como se tivessem sido pré-especificadas. O registro descreve o que a documentação permite afirmar **hoje** e identifica o que continua em aberto.

## 1. Pergunta literal, oito metadados e fontes

> “O desemprego brasileiro caiu para 5,2%. Mas o mercado de trabalho melhorou em todas as dimensões?”

A frase está reproduzida no [README do estudo](../../scripts/pnadc_estudo_i/README.md) e na [nota científica](nota_cientifica.md), onde é identificada como pergunta original. O documento original de E00, sua data e eventual sequência de decisões **não foram localizados no material versionado** examinado; a descrição abaixo é reconstrução editorial.

| Campo T02.2 | Consolidação verificável e status |
| --- | --- |
| **Pergunta** | Formulação literal acima, tal como documentada no README/nota científica; estatuto de formulação original referido pelas fontes, sem comprovação de pré-registro. |
| **Objeto** | Mercado de trabalho brasileiro em cinco eixos A–E, 13 indicadores substantivos; agregados oficiais, não indivíduos acompanhados longitudinalmente. |
| **Dados** | PNAD Contínua agregada, série nacional IBGE/SIDRA; projeção descritiva pública de E09 com 26 linhas em `data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv`. Snapshot integral, CVs de entrada, artefatos E04–E08 e cadeias históricas não estão no clone. Licença da vintage e governança P0 não homologadas. |
| **Método** | Descrição de contrastes agregados; E07 ajusta 13 modelos Stan marginais no período 202609; E08.2/E09 calculam cenários gaussianos condicionais a `rho` temporal hipotético. Não há modelo conjunto identificado. |
| **Estado** | E00 consolidado **documentalmente**, pendente de QA T04.1. QA/integração T03.1–T03.7 concluídos no próprio escopo; nenhum resultado histórico reestimado nesta T04.1. Validação científica e homologação de dados não inferidas. |
| **Resultados** | Síntese pública descritiva dos 26 contrastes; demais resultados/probabilidades históricas não reconstruídos a partir do clone. O quadro IBGE e a projeção devem ser distinguidos. |
| **Limites** | Desenho complexo, trimestres móveis, CVs sem covariâncias de desenho, suportes, arredondamento, restrições de acesso, mudanças conceituais e ambiguidade da expressão “todas as dimensões”. Não inferir causalidade nem chance conjunta de melhora. |
| **Responsável** | `PENDENTE_DE_VERIFICACAO` para a autoridade científica que teria aprovado o E00 original. Autoria da conta GitHub e manutenção dos arquivos não provam autoridade sobre hipóteses nem autorização de dados. |

**Fontes primárias versionadas:** [README](../../scripts/pnadc_estudo_i/README.md), [nota científica](nota_cientifica.md), [inventário T03.1](inventario_estimandos_e07_T03_1.md), [auditoria T03.4](auditoria_suporte_cv_arredondamento_T03_4.md), [alternativas T03.5](comparacao_alternativas_inferenciais_T03_5.md), [rótulos T03.6](rotulos_inferenciais_T03_6.md), [dados descritivos E09](../../data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv).

## 2. População, unidade, alcance e janelas

**Escopo geográfico:** Brasil. **Unidade publicada e comparada:** estatística agregada por indicador e período de referência, não pessoa, domicílio, município ou trajetória individual. A PNAD Contínua é amostral; as contagens são totais estimados expandidos, não entrevistas individuais. A população de **14 anos ou mais** é referência central/denominador de apoio no E04, mas alguns indicadores empregam universos e denominadores próprios (ocupados, ocupados com rendimento, força ampliada, força de trabalho + desalentados). Não tratar todos os 13 estimandos como taxas sobre a mesma população.

**Duas comparações realmente documentadas**, com identificação `AAAAMM`:

| Janela no E09 | Origem | Destino | Interpretação documental |
| --- | --- | --- | --- |
| `anual` | `202509` | `202609` | Indicadores referentes a julho–setembro de 2025 versus julho–setembro de 2026. |
| `recente` | `202606` | `202609` | Indicadores referentes a abril–junho versus julho–setembro de 2026. |

O sufixo mensal identifica o **último mês do trimestre móvel**, não uma observação somente daquele mês. As duas comparações não compartilham meses de referência; isso **não prova independência estatística**, devido ao desenho de rotação da amostra PNAD. A projeção pública tem exatamente 13 × 2 = **26** contrastes descritivos, não 26 modelos Stan.

**Fora do alcance:** resultados individuais, qualidade contratual total, salários de cada pessoa, diferenças causais, emprego formal em todos os grupos, comparações regionais e trajetórias individuais. A expressão “todas as dimensões” é uma pergunta substantiva aberta: cinco eixos de indicadores selecionados **não são uma definição exaustiva de bem-estar laboral**.

## 3. Eixos, indicadores e estimandos documentados

A [especificação E04](../../scripts/pnadc_estudo_i/estudo_I/e04/construir_base_e04.py) define os IDs; [T03.1](inventario_estimandos_e07_T03_1.md) verifica 13 ajustes E07 independentes para `202609`. A direção substantiva favorável, quando sugerida, **não é por si uma regra científica homologada de melhora simultânea**.

| Eixo | IDs dos indicadores principais | Escala / sentido típico a examinar |
| --- | --- | --- |
| A — Desocupação | `desocupacao_pct`, `desocupados_mil` | Taxa (%) e total (mil); queda usualmente favorável. |
| B — Ocupação/participação | `nivel_ocupacao_pct`, `participacao_pct`, `ocupados_mil`, `forca_trabalho_mil` | Taxas (%) e totais (mil); crescimento não equivale automaticamente a melhora de qualidade. |
| C — Subutilização/desalento | `subutilizacao_pct`, `subocupacao_horas_pct`, `desalento_pct`, `desalentados_mil` | Taxas (%) e total (mil); queda usualmente favorável. |
| D — Informalidade/previdência | `informalidade_pct`, `contrib_previdencia_pct` | Taxas (%); menor informalidade e maior contribuição são indicadores distintos, não complementos algébricos. |
| E — Rendimento | `rendimento_real_habitual_rs` | Rendimento médio real habitual (R$); aumento requer considerar comparabilidade e classificação oficial. |

`populacao_14mais_mil` é indicador auxiliar **não substantivo**; não é 14º modelo marginal E07. Em taxas, a diferença entre percentuais é expressa em **pontos percentuais**; os demais contrastes mantêm mil pessoas ou R$. Para os 13 indicadores `i` e cada janela `w=(t0,t1)`, o **estimando descritivo calculável na projeção** é:

`Delta_descritivo(i,w) = y_publicado(i,t1) - y_publicado(i,t0)`.

Essa diferença entre números arredondados **não** é um estimando latente estimado conjuntamente por Stan. Na E07, o parâmetro programado é `theta_i,202609`, posterior **marginal** para o nível de um indicador naquele período; `y_rep` é réplica preditiva marginal. Não se ajustam `theta_i,202509` e `theta_i,202606` como pares conjuntos em E07.

Os campos `ProbSentido` e `IC95Inf/Sup` da E08.2/E09 são **funções de cenários Normais sob `rho` hipotético** e não posteriores de diferenças de Stan. O evento `P(todos os cinco eixos melhorarem | dados)` **não está identificado** pelos insumos disponíveis: falta definição operacional consensual do evento de melhora em cada eixo e dependência entre indicadores/tempos. Ver [T03.2](derivacao_contrastes_gaussianos_T03_2.md) e [T03.6](rotulos_inferenciais_T03_6.md).

## 4. Hipóteses concorrentes: propostas editoriais, não pré-registro

As hipóteses abaixo são **RECONSTRUCAO_EDITORIAL** para organizar uma decisão E00 futura; não representam hipóteses supostamente predefinidas no estudo histórico, modelos já ajustados, partição exaustiva, nem probabilidades de modelo.

| ID | Hipótese/explicação concorrente proposta | Implicação observável ou discriminante | Estado de identificação |
| --- | --- | --- | --- |
| H01 | **Melhora não uniforme:** queda da desocupação convive com estabilidade ou piora de parte dos indicadores no recorte recente. | Comparar sinais, unidades e `SituacaoOficialIBGE` dos 13 indicadores nas duas janelas; contrastar eixos, não resumir tudo por uma taxa. | Descrição pública permite avaliar discrepâncias; `P(todos os eixos)` continua não identificado. |
| H02 | **Melhora amplamente disseminada no recorte selecionado:** vários indicadores variam no sentido usualmente favorável, ressalvadas mudanças oficialmente classificadas estáveis. | Verificar se o padrão de direção e classificação oficial se sustenta nos 13 indicadores/janelas e em definições alternativas do que é “melhorar”. | Hipótese descritiva **não excludente** de H01 quando alguns indicadores divergem; critérios de agregação pendentes. |
| H03 | **Indeterminação inferencial de mudança latente:** observações arredondadas e margens isoladas admitem cenários de covariância diferentes, impossibilitando probabilidade posterior temporal/conjunta identificada. | Comparar como `ProbSentido` muda com `rho`, suporte e arredondamento; exigir covariâncias amostrais e desenho antes de alegar identificação. | Risco metodológico comprovado documentalmente, mas não constitui posterior de hipótese/modelo. |

**Relação:** H01 e H02 podem ocorrer parcialmente juntas e não são modelos mutuamente exclusivos; H03 trata de identificabilidade e pode coexistir com ambas. Não é válido calcular pesos posteriores `P(H01|dados)` etc. com esta tabela. Uma comparação bayesiana de hipóteses exigiria formalizar espaço de modelos, priors, likelihood conjunta, dados e regra de decisão **em novo ciclo científico**.

## 5. Observações já disponíveis e distinção entre evidências

O [CSV descritivo](../../data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv) registra, no recorte **recente**, `desocupacao_pct` 5,4 → 5,2 (queda), `desalentados_mil` 2.288 → 2.459 (aumento) e `rendimento_real_habitual_rs` 3.753 → 3.790 (classificação oficial: `estavel`). Essas são **leituras dos agregados e do campo de classificação do CSV**, não resultados recalculados de E07–E09, nem prova independente de significância/causalidade. A [nota científica](nota_cientifica.md) descreve ressalvas e outros exemplos.

| Camada | Evidência disponível no clone | Interpretação permitida |
| --- | --- | --- |
| Série/contraste publicado | Projeção pública E09 de 26 registros e códigos E04/E05 | Diferenças descritivas entre períodos, condicionadas a fonte e arredondamento. |
| Classificação IBGE | `SituacaoOficialIBGE` para parte dos indicadores | Campo transcrito da síntese; algumas células do eixo D não constam no Quadro Sintético. |
| E07 Stan | Três modelos e scripts R versionados, mas sem draws históricos | Especificações dos 13 níveis marginais em 202609, não inferência temporal reexecutada. |
| E08.1–E08.4 / E09 | Scripts históricos e relatórios T03.1–T03.7 | PPC marginal, cenários gaussianos e limites para proxies com hipóteses não observadas; sem nova reprodução. |
| Licença/proveniência | Inventário de dados e [Issue #14](https://github.com/riscotta/bayes-stan-r/issues/14) | P0 aberto; **nenhuma** autorização de redistribuição homologada por este E00. |

## 6. Decisões ainda abertas e condições verificáveis

Nenhuma decisão científica/institucional abaixo é marcada `FECHADA`. O registro de alternativas, por si só, não prova autoridade nem critério pré-especificado.

| ID | Decisão em aberto | Requisito objetivo antes de fechar | Estado |
| --- | --- | --- | --- |
| D00-01 | Formalizar qual evento substantivo significa “melhoria” em cada eixo e se bastam 13 indicadores selecionados. | Definições, direção e agregação justificadas; avaliação da estabilidade oficial, destinatário e autoridade científica identificados. | `ABERTA` |
| D00-02 | Escolher entre relatório somente descritivo/condicional e novo modelo temporal ou multivariado. | Justificar estimando, identificar covariâncias de desenho e plano de coleta/licenciamento; obter revisão científica formal para mudança de modelo. | `ABERTA` |
| D00-03 | Pré-especificar hipóteses realmente comparáveis, perdas, critérios e `NAO_DECIDIR`. | Hipóteses distinguíveis, priors/likelihood justificáveis, métrica de comparação e registro anterior a eventual nova execução. | `ABERTA` |
| D00-04 | Homologar proveniência, permissão e privacidade dos insumos sujeitos a P0. | Gates T01.3–T01.7 e Issue #14 com autorização e inspeção em ambiente institucional apropriado. | `ABERTA` |
| D00-05 | Atribuir formalmente responsabilidade pela aprovação científica do E00 e pelo uso dos dados. | Evidência da função, autorização explícita e decisão com data; autoria GitHub não basta. | `ABERTA` |
| D00-06 | Determinar nível de reprodução e acesso dos insumos ausentes. | Manifestos, scripts de origem, CVs, saídas E07/E08, licenças e ensaios de reprodução documentados; tarefa posterior T04.2/T04.6/T06. | `ABERTA` |

**Regra de `NAO_DECIDIR`:** não declarar melhora conjunta nem posterior temporal quando evento, dados, identificação de covariâncias ou diagnóstico não forem suficientes; não concluir autorização de redistribuição pela presença de arquivo em repositório público. **Reabertura:** cada decisão exige nova evidência localizável, responsável com atribuição comprovada, data e justificativa registrada, sem reescrever o E00 histórico.

## 7. Riscos, evidências faltantes e dependências

- **P0 dados:** Issue #14 e T01.3–T01.7 abertas; não transcrever insumos institucionais nem publicar novas bases como meio de testar E00.
- **Dependência entre observações:** trimestres móveis, esquema amostral rotativo e covariâncias de desenho ausentes; `rho=-0,5; 0; 0,5; 0,9` são cenários postulados, não estimativas.
- **Mudanças conceituais e denominadores:** E04 registra rupturas de 2015 na subocupação/subutilização e períodos distintos de cobertura; nenhum ajuste de quebra ou harmonização foi feito aqui.
- **Reprodução:** falta snapshot de 22.130 linhas, séries completas, CVs do processo E04/E07, saídas de E08/E09 e draws E07; ausência no Git não prova ausência histórica.
- **Separação de responsabilidades:** `PENDENTE_DE_VERIFICACAO` a autoridade responsável pelo E00 histórico; sem atribuição inventada.
- **Validade:** CI e testes de estrutura comprovam contratos de arquivos, não identificação, cobertura amostral, causalidade nem validade bayesiana dos cenários.

## 8. Critério de aceitação T04.1 e pacote de continuidade

A entrega T04.1 será verificável se o novo E00 contiver pergunta literal, recorte Brasil, janelas exatas, os 13 indicadores e seus cinco eixos, distinção do apoio populacional, hipóteses rotuladas como reconstrução editorial, decisões abertas com gates, referência cruzada dos estimandos e identificação das lacunas. O [teste estrutural T04.1](../../tests/testthat/test-pnadc-e00-consolidacao-t04-1.R) deve validar presença, unicidade, janelas e vínculos mínimos contra o CSV público **sem reexecutar E04–E09**.

**Não é o gate de encerramento da Etapa 04:** T04.2–T04.7 permanecem pendentes. **Não há QA independente neste documento** nem autorização automática de merge pela simples criação do arquivo. O próximo chat deve revisar o PR e o SHA final, conferir as fontes e a CI, procurar reclassificações indevidas e emitir parecer independente.

**Índice de fontes no SHA-base:** [modelo E00 T02.3](../../scripts/STUDY_E00_TEMPLATE_T02_3.md) · [contrato T02.2](../../scripts/STUDY_METADATA_CONTRACT_T02_2.md) · [PNAD README](../../scripts/pnadc_estudo_i/README.md) · [E04](../../scripts/pnadc_estudo_i/estudo_I/e04/construir_base_e04.py) · [E05](../../scripts/pnadc_estudo_i/estudo_I/e05/explorar_e05.py) · [E07](inventario_estimandos_e07_T03_1.md) · [T03.5](comparacao_alternativas_inferenciais_T03_5.md) · [T03.6](rotulos_inferenciais_T03_6.md) · [nota científica](nota_cientifica.md) · [projeção E09](../../data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv).