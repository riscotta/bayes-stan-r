# T02.5 — Matriz obrigatória de evidência e conclusão E08 → E09

**Finalidade:** permitir que um terceiro rastreie, por estudo, **cada afirmação de E09** à evidência efetivamente disponível em E08, ao diagnóstico pertinente, às hipóteses de sensibilidade e ao limite de validade. Este é um **contrato documental vazio**, não uma auditoria científica, execução de E08/E09 ou homologação de resultados. Não preencher com probabilidades ou conclusões presumidas. O piloto e a formalização no `CONTRIBUTING.md` pertencem à T02.7.

**Vínculos:** [taxonomia T02.1](STUDY_TAXONOMY_T02_1.md) · [oito metadados T02.2](STUDY_METADATA_CONTRACT_T02_2.md) · [planejamento E00 T02.3](STUDY_E00_TEMPLATE_T02_3.md) · [ponte matemática–código E06→E07 T02.4](STUDY_E06_E07_MATRIX_T02_4.md) · [índice dos scripts](README.md) · [Issue #14 — governança de dados](https://github.com/riscotta/bayes-stan-r/issues/14). Os estados abaixo são **locais à afirmação**, não a política de estados por etapa T02.6.

## 1. Escopo da unidade e cadeia de custódia

- **Estudo, pasta/identificador histórico e classe editorial:** `PENDENTE_DE_VERIFICACAO`; fonte: `PENDENTE_DE_VERIFICACAO`.
- **Natureza da ficha:** `DOCUMENTACAO_ATUAL` ou `RECONSTRUCAO_EDITORIAL`; registrar data e autoria apenas se verificadas. Reconstrução editorial não é registro contemporâneo.
- **Versão E08:** arquivo, seção/linha, SHA, versão do método, data e variantes: `PENDENTE_DE_VERIFICACAO`.
- **Versão E09:** nota, tabela, figura, frase/seção/linha, SHA e público-alvo declarado: `PENDENTE_DE_VERIFICACAO`.
- **Fontes ascendentes:** E00 (hipóteses e estimando), E06/E07 (modelo, parametrização e saídas), dados de entrada e, se existirem, artefatos de execução: `PENDENTE_DE_VERIFICACAO`.
- **Comando, ambiente, seed e artefatos da execução E08/E09, quando efetivamente verificados:** `PENDENTE_DE_VERIFICACAO`. Identificar códigos e hashes distintos de outputs; a existência de arquivos não prova reexecução.
- **Cobertura:** inventariar **todas** as afirmações materiais de E09 (texto, gráficos, legendas, tabelas, conclusões e negativas) e todas as evidências E08 invocadas. Preservar afirmações sem fonte como pendência; não omiti-las.
- **Dados protegidos:** apenas identificadores de artefatos e metadados permitidos. Não transcrever registros, credenciais nem dados institucionais em PR, logs ou chat; respeitar T01.3–T01.7 e Issue #14.

**Fronteira inferencial:** evidências de **posterior marginal por modelo**, **contraste derivado de amostras conjuntas identificadas**, **aproximação condicional** e **cenário de sensibilidade com parâmetros hipotéticos** são coisas distintas. É vedado apresentar um cenário condicionado a ρ não observado como probabilidade posterior conjunta empírica. Uma CI verde não demonstra PPC, convergência, calibração, replicação ou validade da conclusão.

## 2. Fontes verificáveis — separar origem, cálculo e texto publicado

| ID | Etapa/tipo | Caminho/URL e localizador preciso | SHA/versão/hash e origem histórica/editorial | Verificação realizada, status e acesso |
| --- | --- | --- | --- | --- |
| F08-01 | E08 — análise/sensibilidade/diagnóstico | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `NAO_INSPECIONADO` — diligência pendente |
| F08-02 | E08 — saída ou amostras efetivamente utilizadas | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `NAO_INSPECIONADO` — diligência pendente |
| F09-01 | E09 — afirmações, tabelas e figuras | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `NAO_INSPECIONADO` — diligência pendente |
| F07-01 | E07/E06 e dados — origem da estimativa | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `NAO_INSPECIONADO` — diligência pendente |

Para um resultado ausente, usar `PENDENTE_DE_VERIFICACAO` (ainda não apurado); após diligência descrita e comprovável, `INDISPONIVEL_NO_HISTORICO`. `NAO_APLICAVEL` exige justificativa contextual e **não** significa desconhecido. Links devem resolver para a versão auditada e apontar seção, linha, tabela, variável ou célula; URLs de diretório não bastam.

## 3. Matriz principal E08 → E09 — uma linha por afirmação e recorte

**Instrução:** duplicar linhas para cada frase material, número, contraste, eixo, população, janela, variável, figura ou generalização de E09. Não condensar conclusões distintas em “resultados confirmados”. Quando uma afirmação depender de várias fontes/diagnósticos, atribuir IDs de evidência relacionados e detalhar o vínculo na seção 4. IDs abaixo identificam **cinco exemplos vazios**, não uma lista exaustiva de resultados.

| ID | Classe e afirmação E09 literal | Estimando, recorte, escala e probabilidade-alvo | Evidência E08: cálculo/saída/linha e ID | Modelo, condicionantes, correlações e fonte | Diagnóstico aplicável, resultado e fonte | Sensibilidade: parâmetro, cenário, faixa e efeito | Limite de validade e extrapolação proibida | Localizador E09, SHA e reconciliação numérica | Estado local, divergência e próxima ação |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| A08-01 | DESCRITIVA — `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `ABERTA` — não auditada |
| A08-02 | POSTERIOR_MARGINAL — `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `ABERTA` — não auditada |
| A08-03 | CONTRASTE_CONJUNTO — `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `ABERTA` — não auditada |
| A08-04 | CENARIO_CONDICIONAL — `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `ABERTA` — não auditada |
| A08-05 | INTERPRETACAO_OU_AUSENCIA — `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `ABERTA` — não auditada |

**Dez campos obrigatórios por linha (correspondentes às dez colunas):** (1) ID estável e único; (2) classe e afirmação E09 literal; (3) estimando, unidade, população, tempo, evento e escala; (4) evidência E08, algoritmo e localizador; (5) modelo/variante, hipóteses condicionantes e proveniência; (6) diagnóstico pertinente **com resultado, critério e fonte ou ausência justificada**; (7) sensibilidade, cenários, variação da conclusão e fonte; (8) limites de validade (identificação, desenho, cobertura e acesso); (9) localizador e versão E09, com reconciliação numérica e arredondamento; (10) estado local, código de divergência, motivo e próxima ação. **Nenhuma coluna vazia equivale a PASS.**

## 4. Índice de evidências, diagnósticos e sensibilidade

| ID | A08 relacionado | Tipo (estimativa/diagnóstico/sensibilidade) e hipótese | Artefato, comando, SHA, localizador e resultado verificado | Limite do teste e diligência |
| --- | --- | --- | --- | --- |
| EV08-01 | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` |
| DG08-01 | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` |
| SN08-01 | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` |

**Diagnóstico por afirmação, conforme a fonte do cálculo:** inspecionar R-hat, ESS, divergências, BFMI, treedepth e mistura quando a afirmação de fato depender de MCMC; registrar PPC e checks de plausibilidade/calibração quando existentes; para aproximações, verificar hipóteses, suporte, positividade de variância/covariância, simulação numérica, estabilidade e erro de aproximação. **A ausência de PPC/MCMC é um limite explícito, não uma falha automaticamente sanável por outra estatística.** Separar diagnóstico de amostragem de adequação substantiva e de robustez da hipótese causal.

**Sensibilidades candidatas, somente quando pertinentes:** priors/hiperparâmetros; desenho e pesos amostrais; missingness, filtros e vintage; correlação e dependência temporal; arredondamento; escolha de likelihood; comparação de modelos; alteração de janela, métrica ou população. Se ρ for apenas hipótese de cenário, registrar domínio admissível, valores varridos, efeito por afirmação e **não** chamá-lo de correlação estimada. Distinguir cálculo realmente executado de exercício proposto.

## 5. Regras de reconciliação E08 → E09

1. **Correspondência literal:** citar o trecho exato de E09 (incluindo legenda/célula), a versão e todas as saídas E08 que sustentam a afirmação. Comparar unidades, direção do contraste, janela, denominador, condicionamento, escala e transformação (probabilidade, log-odds, taxa). Confrontar número arredondado com origem de precisão suficiente.
2. **Natureza da incerteza:** identificar se intervalo é credível posterior, preditivo, cenário condicional ou faixa de sensibilidade. Não converter quantis marginais em distribuição conjunta; não pressupor independência, nem multiplicar probabilidades entre eixos sem modelo conjunto identificado.
3. **Hipótese explícita:** registrar modelo/variante, estimando, dados, correlação/sobreposição, prior e limitações de identificação; hipóteses não observadas devem constar do enunciado E09 sempre que mudarem a interpretação.
4. **Diagnóstico observável:** registrar fonte, comando, valor, limiar previamente definido ou justificativa técnica e consequência para **a afirmação específica**. CI, parse e presença de PDF não comprovam diagnóstico inferencial.
5. **Sensibilidade local:** separar variação explorada de robustez alegada; declarar pontos onde sinal, ordem, magnitude, conclusão ou decisão mudam. Não proclamar robustez por uma única configuração favorável.
6. **Validade e comunicação:** restringir interpretação ao universo amostral, janela, vintage e pressupostos efetivamente documentados; proibir extrapolação para causalidade, população ou posterior conjunta sem evidência.
7. **Negativas e lacunas:** “não há evidência”, “não significativo”, “não aplicável” e “não executado” exigem fundamentos diferentes. Registrar pesquisa e fontes examinadas antes de `INDISPONIVEL_NO_HISTORICO`. Ausência de diagnóstico impede afirmar que ele passou.

## 6. Códigos de divergência e estados locais

| Código | Situação verificável | Estado/consequência |
| --- | --- | --- |
| D0 — SEM_EVIDENCIA | Afirmação sem fonte E08, SHA, cálculo ou localização suficientes; material não inspecionado. | `ABERTA`; não promover a validação. |
| D1 — RECONCILIADA | Mesma afirmação/estimando, versão e número recalculados ou confrontados com arredondamento documentado; pressupostos e diagnósticos adequadamente declarados. | `CONFORME_DOCUMENTO` ou, se houve execução identificável, `CONFORME_EXECUCAO`; **não** atesta validade científica. |
| D2 — INFERENCIAL | E09 extrapola posterior marginal, cenário hipotético, diagnóstico ou sensibilidade para probabilidade conjunta/causalidade/generalização não demonstrada. | `DIVERGENCIA`; bloquear alegação e encaminhar revisão científica. |
| D3 — NUMERICA_OU_VERSAO | Saída, direção, unidade, escala, arredondamento, variante ou janela diferem e não existe reconciliação demonstrada. | `DIVERGENCIA`; preservar fonte e documentar impacto. |
| D4 — DIAGNOSTICO_OU_SENSIBILIDADE | Afirmação requer diagnóstico, hipótese ou análise de sensibilidade ausente, contraditória ou não examinada. | `ABERTA` ou `DIVERGENCIA` (se houver contradição); declaração de robustez bloqueada. |
| D5 — RESTRICAO | Dado, licença, execução ou histórico indisponível após diligência documentada, ou acesso legitimamente restrito. | `PENDENTE_DE_VERIFICACAO` ou `INDISPONIVEL_NO_HISTORICO`; limitar parecer, sem divulgar dado restrito. |

**Estados locais permitidos (não correspondem a T02.6):** `ABERTA`, `CONFORME_DOCUMENTO`, `CONFORME_EXECUCAO`, `DIVERGENCIA` e `NAO_APLICAVEL`. `CONFORME_DOCUMENTO` exige confronto verificável da afirmação com E08, incluindo indicação dos limites; `CONFORME_EXECUCAO` exige adicionalmente execução com artefatos e ambiente identificáveis. Nenhum deles significa “inferência válida”, “dados autorizados” ou “QA independente aprovado”.

## 7. Decisões, pendências e condição de aceite por estudo

| ID | A08/afirmação e problema | Fontes consultadas, divergência e impacto inferencial | Responsável comprovado, decisão/autoridade e condição verificável de fechamento | Estado e próxima diligência |
| --- | --- | --- | --- | --- |
| P08-01 | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `ABERTA` — sem diligência |
| DEC08-01 | Autorizar, corrigir, qualificar ou suspender afirmação E09 | `PENDENTE_DE_VERIFICACAO` | `PENDENTE_DE_VERIFICACAO` | `ABERTA` — nenhuma decisão presumida |

- [ ] Inventário E08/E09 completo por SHA, fonte contemporânea versus reconstrução, texto E09 e IDs A08 estáveis.
- [ ] Toda afirmação material E09 tem trilha de origem E08 ou lacuna justificada; tabelas, gráficos e negativas incluídos.
- [ ] Estimando, recorte, unidade, escala, método, hipótese de dependência e natureza da probabilidade foram reconciliados.
- [ ] Diagnósticos e sensibilidades pertinentes possuem fontes, resultados e consequências; ausências estão explícitas.
- [ ] Limites inferenciais, privacidade/licenças, amostra, cobertura e extrapolações indevidas foram examinados.
- [ ] Divergências permaneceram abertas até decisão autorizada; revisão científica distinta do PASS estrutural foi registrada.
- [ ] Revisão técnica independente com SHA e fontes foi realizada antes de alegar QA por estudo.

**Aceitação da tarefa T02.5:** modelo copiável e coerente, contendo obrigatoriamente afirmação, evidência, diagnóstico, sensibilidade e limite de validade, vinculado ao catálogo e com testes estruturais. **Não** prova aplicação em estudo real, MCMC, PPC ou interpretação científica correta. Estados por etapa e integração normativa permanecem T02.6 e T02.7; auditoria de PNAD E08/E09 pertence à T03.*. Não editar E08, E09, saídas históricas, modelos ou dados neste ciclo.
