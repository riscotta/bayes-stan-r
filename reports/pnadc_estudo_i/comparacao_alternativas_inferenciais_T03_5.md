# T03.5 — Alternativas inferenciais e identificabilidade (PNAD Contínua, Estudo I)

**Natureza:** comparação metodológica e proposta de decisão para o estudo existente; auditoria estática dos fontes E07–E09, sem ajuste de modelo, reexecução, validação empírica, inspeção de dados institucionais ou alteração das 26 saídas históricas. **Base do ciclo:** main `0ee9cb91594bd6532684cd859ce3e847c2430c8d` (09/11/2026), após T03.1–T03.4. **Estado:** proposta submetida a QA independente; não equivale a aprovação científica.

## 1. Pergunta que a decisão deve responder

Os 13 indicadores substantivos, distribuídos em cinco eixos, possuem dois contrastes publicados por indicador: 202509→202609 e 202606→202609, totalizando 26 pares. Uma diferença descritiva entre agregados não é automaticamente um contraste posterior. A pergunta científica deve distinguir:

1. **Direção descritiva:** diferença `y_1-y_0` dos números publicados, observada sob suas regras de arredondamento.
2. **Cenário probabilístico por indicador:** probabilidade de sinal sob uma **distribuição gaussiana postulada**, CVs percentuais e `rho` temporal fixado pelo analista.
3. **Inferência para contraste latente:** distribuição de `theta_1-theta_0` derivada de uma likelihood, priors, suportes e covariância amostral justificadas.
4. **Evento conjunto entre indicadores/eixos:** requer dependência **entre indicadores e entre períodos**, além de definição prévia e defensável do evento de melhoria por eixo. Não é obtido multiplicando probabilidades marginais nem usando o mesmo `rho` temporal para todos.

As saídas de E07 são resumos de 13 modelos **marginais de 202609**. E08.2 computa cenários de diferenças a partir de estimativas/CVs E04; E08.3 usa quadratura sob priors e `rho` postulados e aproxima a cauda por Normal dos momentos; E09 seleciona `rho=0` e extremos de uma grade. Nenhum desses passos identifica uma distribuição temporal conjunta. Ver os inventários T03.1–T03.3 e a auditoria T03.4.

## 2. Matriz comparativa de alternativas

| Alternativa | Insumos indispensáveis | Quantidade que pode sustentar | Identificabilidade e riscos | Decisão proposta |
| --- | --- | --- | --- | --- |
| **A — Preservar cenários gaussianos condicionais de E08.2/E09** | Estimativas agregadas publicadas, CVs com metadados, direção e quatro valores hipotéticos de `rho = -0,5; 0; 0,5; 0,9` | `P_cenario(k Delta > 0 \| rho, y_0,y_1,s_0,s_1)`, como **sensibilidade exploratória**; intervalos gaussianos condicionais | `rho` e distribuição conjunta não estimados; CVs não são covariâncias; normalidade não truncada ignora suportes; cobertura de desenho e calibração não demonstradas; arredondamento pode alterar direção | **Manter o histórico imutável**, com ressalvas explícitas e correção editorial de rótulos na T03.6. Não apresentar como probabilidade posterior da PNAD |
| **B — Modelo conjunto temporal por indicador (pares ou três períodos)** | Série consistente de estimativas, variâncias **e covariâncias de amostragem entre períodos** estimadas por desenho, ou microdados/replicações autorizados que as permitam estimar; metadados, suportes, priors e likelihood | Posterior de `Delta_i=theta_{i,1}-theta_{i,0}` para cada indicador, **condicional a um modelo novo validado** | Duas ou três médias e SEs **não identificam** as correlações desconhecidas; posterior para `rho` pode ser quase inteiramente determinada pela prior; restrições, versões, pesos, rotação, unidades e incerteza do denominador devem ser modeladas | **Não implementar agora**; especificar protocolo e fonte de covariâncias antes de criar novo estudo/ajuste |
| **C — Modelo multivariado temporal dos cinco eixos** | Requisitos de B **mais** covariâncias entre indicadores, compatibilidade das amostras e definições, estimandos por eixo e regra pré-definida de melhora simultânea | Posterior de evento conjunto definido, se o modelo e sua identificação forem sustentados e avaliados | Matriz de dependências de dimensão elevada, agregados de definições diferentes, vínculos contábeis, restrições e incerteza de desenho; proxy de um indicador por eixo não equivale ao eixo | **Não declarar identificada**; tratar como novo programa de pesquisa apenas após viabilidade institucional e protocolo científico |

**Observação:** B não exige necessariamente microdados se for fornecida uma estimativa válida e identificável da matriz de covariância de desenho. Na ausência de covariância empírica e de hipótese verificável que a substitua, impor `rho=0` ou uma prior para `rho` produz resultado **condicional à hipótese**, não identificação pelos agregados. Para C, uma matriz estimada precisa ainda ser coerente (simétrica, semidefinida positiva e compatível com escalas/suportes); estabilização numérica isolada não justifica sua validade científica.

## 3. Demonstração de não identificação por marginais

Na aproximação bivariada de um indicador, com `s_0,s_1>0`, a família de matrizes

```text
Sigma(rho) = [[ s0², rho*s0*s1 ],
              [ rho*s0*s1, s1² ]]       (-1 < rho < 1)
Var(Delta | rho) = s0² + s1² - 2*rho*s0*s1
P_cenario(k*Delta > 0 | rho) = Phi(k*(y1-y0)/sqrt(Var(Delta | rho))).
```

Todos esses cenários têm **as mesmas marginais univariadas**. Os CVs de cada período, mesmo exatos, não distinguem as covariâncias fora da diagonal. Logo não se identifica a distribuição do contraste **a partir das marginais somente**. Exemplo **sintético, não PNAD**: `y0=10, y1=13, s0=2, s1=1`; para `rho=-0,5;0;0,5;0,9`, as variâncias são `7;5;3;1,4` e as probabilidades condicionais de `Delta>0` diferem, embora as marginais sejam idênticas. A demonstração não prova ausência absoluta de informação: covariâncias adicionais, desenho ou hipóteses estruturais justificadas poderiam informar a dependência.

Para cinco eventos com probabilidades marginais `p_1,...,p_5` sob um mesmo cenário, os limites de Fréchet `max(0,sum(p)-4) <= P(interseção) <= min(p)` permitem **muitas** probabilidades conjuntas. Com cinco `p_i=0,9` sintéticos, a faixa é `[0,5;0,9]`; `0,9^5` seria apenas a **hipótese de independência entre eventos**. Os cinco eventos proxy de E08.4 **não** definem os cinco eixos substantivos. Nem os limites de Fréchet identificam um valor pontual de probabilidade de melhoria simultânea.

## 4. Critérios de aceitação para qualquer futura reformulação

| Gate científico | Evidência mínima a produzir | Situação neste ciclo |
| --- | --- | --- |
| Definir alvo inferencial e direção | População, períodos, variáveis, regras para cinco eixos, unidades e decisão E00 | **PARCIAL**: 13 indicadores/26 contrastes documentados; definição de evento conjunto pendente |
| Proveniência e permissão | Fontes/vintages, acesso autorizado, privacidade e licença, Issue #14/T01.3–T01.7 | **PENDENTE**: nenhum conteúdo institucional examinado |
| Incerteza amostral temporal | SE e covariâncias de desenho por indicador/par, estimadas ou justificadas com dados autorizados | **INDISPONÍVEL NO REPOSITÓRIO** |
| Dependência entre indicadores | Matriz coerente entre indicadores/períodos ou desenho/microdados que a identifiquem | **INDISPONÍVEL NO REPOSITÓRIO** |
| Especificação e suporte | Likelihood de estimativas agregadas com covariância, priors e suportes de taxa [0,100], contagem [0,P14] e renda >= 0; incerteza de denominador | **NÃO ELABORADO**; modelos existentes E07 são marginais |
| Diagnóstico e robustez | PPC, R-hat/ESS, sensibilidade a priors, covariâncias, suportes e arredondamento; testes sintéticos de recuperação, cobertura e estabilidade | **NÃO EXECUTADO** para modelo conjunto |
| Reprodução e validação externa | Insumos e manifesto autorizados, código novo, execução independente e avaliação científica | **NÃO EXECUTADO**; E04–E09 históricos preservados |

Um modelo temporal conjunto com `rho` livre e apenas as médias e CVs públicos pode produzir uma posterior **matematicamente definida por suas priors**, mas isso não demonstra que a correlação foi identificada pelos dados. Requer análise de sensibilidade e, idealmente, informação adicional de desenho.

## 5. Recomendação científica limitada e encaminhamento

**Proposta para QA T03.5:** manter a alternativa **A** como artefato histórico de **cenários condicionais exploratórios**, mantendo os quatro `rho` como hipóteses sem estimativa empírica. A E09 em `rho=0` deve ser rotulada como **cenário de independência postulada no par**, nunca hipótese demonstrada ou posterior temporal conjunta. Evitar interpretações de melhora simultânea, probabilidades oficiais do IBGE ou cobertura credível/frequentista validada. É aceitável reportar **diferenças descritivas** de números publicados com alerta de arredondamento e critérios do IBGE, sem inflar a evidência probabilística.

**T03.6:** revisar nota científica e rótulos de `ProbSentido`, `ProbMin/MaxCenariosRho`, `IC95Inf/Sup` e títulos interpretativos, preferencialmente em documentação/camada de apresentação; preservar nomes e arquivos históricos, com depreciação/alias explícitos se necessário. **T03.7:** testar invariantes e fronteiras, sem assumir que testes sintéticos validem resultados reais.

**Gatilho para B/C:** demanda científica explícita por inferência de mudanças latentes ou melhora conjunta, aprovação institucional dos dados, possibilidade demonstrada de estimar/justificar as covariâncias relevantes, E00 reformulada, revisão do modelo proposto e plano de reprodução. Isso constituirá **novo ciclo inferencial**; não reclassificar retroativamente os cenários originais como posteriors.

**Riscos residuais:** P0/Issue #14 aberto; ausência de 39 registros completos E04, resumos/draws E07, covariâncias de desenho e E09 integral neste clone; suporte/CV/arredondamento e rotação auditados estruturalmente, **não quantificados** nas 26 comparações. Nenhum dado, Stan, Python inferencial, saída histórica ou documentação institucional restrita foi criado ou modificado por esta tarefa.

## 6. Gate de QA independente

O revisor em chat distinto deve inspecionar **diff no HEAD exato**, verificar a matriz A/B/C frente a T03.1–T03.4 e aos fontes E07/E08/E09; conferir a prova de não identificação e a validade/limite dos limites de Fréchet; executar testes estruturais e sintéticos, examinar falsos positivos, confirmar CI e ausência de alteração histórica; registrar parecer **APROVADO / REPROVADO / INCONCLUSIVO** e riscos. **Execução, CI verde e autoauditoria não autorizam marcar QA ou mesclar.** A decisão nesta página é uma **proposta** enquanto faltar esse gate.

**Referências internas:** [T03.1](inventario_estimandos_e07_T03_1.md) · [T03.2](derivacao_contrastes_gaussianos_T03_2.md) · [T03.3](rastreabilidade_probabilidades_T03_3.md) · [T03.4](auditoria_suporte_cv_arredondamento_T03_4.md) · [nota científica](nota_cientifica.md). Fontes: `scripts/pnadc_estudo_i/estudo_I/e07/`, `e08/sensibilidade_e082.py`, `e08/sensibilidade_e083.py`, `e08/avaliacao_e084.py`, `e09/consolidar_e09.py`.
