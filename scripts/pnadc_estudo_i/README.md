# PNAD Contínua — Estudo I (E00–E10)

> **Pergunta literal:** “O desemprego brasileiro caiu para 5,2%. Mas o mercado de trabalho melhorou em todas as dimensões?”

Brasil; PNAD Contínua **agregada**; contrastes 202509→202609 (anual) e 202606→202609 (recente). Este é um estudo distinto de [pnadc_desocupacao](../pnadc_desocupacao/README.md).

| Eixo | Indicadores substantivos |
| --- | --- |
| A — Desocupação | taxa e desocupados |
| B — Ocupação/participação | nível de ocupação, taxa de participação, ocupados, força de trabalho |
| C — Subutilização/desalento | subutilização, subocupação por horas, taxa e quantidade de desalentados |
| D — Informalidade/previdência | informalidade, contribuição à previdência |
| E — Rendimento | rendimento médio real habitual |

**13 indicadores**; população de 14 anos ou mais é apoio/denominador, não um indicador substantivo adicional.


## Consolidação de E00 — T04.1

A [reconstrução editorial de E00](../../reports/pnadc_estudo_i/e00_consolidado_T04_1.md) registra a pergunta literal, o universo de agregados, duas janelas, os 13 indicadores, hipóteses concorrentes **propostas** e decisões ainda **ABERTAS**. Não constitui o E00 contemporâneo original, pré-registro, aprovação científica nem homologação de dados; a revisão independente, a CI e a integração da T04.1 ocorreram no PR #37; isso não equivale a homologação científica ou dos dados. As tarefas T04.2–T04.7 requerem ciclos próprios.

## Proveniência e contratos documentais E01–E05 (T04.2)

O [inventário T04.2](../../reports/pnadc_estudo_i/contratos_e01_e05_T04_2.md) relaciona as lacunas E01–E03, o dicionário dos **13 indicadores principais + 1 apoio**, os IDs SIDRA e CV declarados, os universos/denominadores, as janelas e os contratos E04/E05. É **RECONSTRUCAO_EDITORIAL**, não documentação contemporânea recuperada. O snapshot SIDRA de 22.130 linhas, os metadados JSON locais, o manifesto histórico e as saídas E04/E05 **não estão disponíveis no clone**; os testes são estruturais e verificam apenas o código e a E09 pública. A T04.2 aguarda **QA independente**, CI do HEAD final e eventual integração; isso não autoriza reexecução, redistribuição nem aprovação institucional. T01.3–T01.7/Issue #14 permanecem pendentes.

## Especificação matemática E06 — T04.3

A [reconstrução editorial E06](../../reports/pnadc_estudo_i/e06_especificacao_modelos_T04_3.md) documenta os **três modelos Stan marginais de E07** (8 taxas, 4 totais e 1 rendimento), a aproximação Normal para agregados com `s=|y|×CV/100`, as priors históricas, restrições de suporte, limitações de identificação e alternativas **não implementadas**. Não é E06 contemporâneo, não reestima dados/modelos, não identifica covariâncias nem posterior temporal ou conjunta. A T04.2 foi integrada no PR #38 após QA independente com ressalvas; sua menção a QA pendente acima retrata o estado pré-merge de T04.2. **T04.3 ainda requer QA independente** no PR próprio; T04.4–T04.7 e a Etapa 04 permanecem abertas.

## Rastreabilidade dos estimandos E07 (T03.1)

O [inventário documental da E07](../../reports/pnadc_estudo_i/inventario_estimandos_e07_T03_1.md) relaciona os 13 estimandos `theta` **marginais de 202609**, os arquivos de saída previstos no R/Stan e as diferenças **descritivas** e **cenários condicionais** produzidos somente em E05/E08/E09. Os resumos MCMC E07 históricos não estão versionados neste repositório; a T03.1 não os reexecuta nem valida uma posterior temporal conjunta. T03.1–T03.7 foram integradas após revisão técnico-documental independente em seus respectivos ciclos, com as ressalvas registradas no plano; isso não representa validação científica integral.

## Derivação matemática dos contrastes (T03.2)

A [derivação gaussiana condicional de E08.2](../../reports/pnadc_estudo_i/derivacao_contrastes_gaussianos_T03_2.md) explicita a matriz de covariância **hipotética** de dois períodos, a identidade `Var(Delta | rho) = s_0² + s_1² - 2rho s_0 s_1`, a probabilidade de sinal e um exemplo sintético. O parâmetro `rho` não foi estimado, e esses cálculos não identificam posterior temporal conjunta nem probabilidades de melhoria simultânea. O documento não altera os resultados históricos; T03.3–T03.7 tiveram revisões específicas em seus ciclos; a homologação dos dados ainda exige os gates da Etapa 01/P0.


## Rastreabilidade das probabilidades E08/E09 (T03.3)

A [matriz de origem dos campos probabilísticos](../../reports/pnadc_estudo_i/rastreabilidade_probabilidades_T03_3.md) distingue PPC e resumos de draws **marginais** E07/E08.1, cenários gaussianos condicionais E08.2, quadratura/propagação aproximada E08.3, limites de proxies E08.4 e consolidação E09. Nenhum cálculo temporal E08/E09 usa draws conjuntos de diferenças E07. A T03.3 foi integrada após revisão técnico-documental com ressalvas (ver histórico do plano); o inventário não reexecuta os arquivos históricos nem estima covariâncias. T03.4–T03.5 foram integradas em seus escopos técnico-documentais, sem homologação das probabilidades; T03.6–T03.7 foram integradas no escopo técnico-documental, sem reprodução dos cálculos históricos.

## Auditoria de suporte e dependência (T03.4)

A [auditoria T03.4](../../reports/pnadc_estudo_i/auditoria_suporte_cv_arredondamento_T03_4.md) compara os suportes Stan de taxas, contagens e rendimento com a aproximação gaussiana não truncada de E08.2; examina a escala do CV, hipóteses de arredondamento de E08.3, janelas dos contrastes e falta de covariâncias de desenho. **Ambos os pares não compartilham meses de referência, mas o esquema amostral rotativo 1-2(5) impede deduzir independência dos estimadores.** Os valores de ρ permanecem cenários hipotéticos. Testes sintéticos verificam riscos e relações matemáticas, não massas de cauda reais nem validade das probabilidades E08/E09. T03.4 foi integrada após QA técnico-documental; T03.5 também foi integrada, sem homologação probabilística. T03.6–T03.7 foram integradas; a governança P0 permanece aberta.

## Alternativas inferenciais e identificabilidade (T03.5)

A [matriz de decisão T03.5](../../reports/pnadc_estudo_i/comparacao_alternativas_inferenciais_T03_5.md) compara manter os cenários gaussianos condicionais da E08/E09, desenvolver modelo temporal por indicador condicionado a covariâncias de desenho verificáveis, ou especificar um modelo multivariado dos cinco eixos. **Decisão metodológica após QA técnico-documental T03.5:** preservar os resultados históricos apenas como cenários exploratórios; dados marginais/CVs não identificam correlações temporais ou entre indicadores, nem melhoria simultânea. Não houve novo Stan/MCMC nem alteração de dados, scripts históricos ou resultados. A T03.5 foi integrada no PR #34 após revisão técnico-documental com ressalvas (sem aprovação formal por terceira conta); T03.6–T03.7 foram integradas; a governança P0 permanece pendente.

## Rótulos interpretativos e nota científica (T03.6)

A [matriz de rótulos T03.6](../../reports/pnadc_estudo_i/rotulos_inferenciais_T03_6.md) estabelece a leitura admissível das colunas históricas sem mudar seu contrato físico: `ProbSentido` é uma **probabilidade de direção sob cenário gaussiano condicional**; `IC95Inf`/`IC95Sup` são os **extremos de faixa normal condicional nominal de 95%**, não IC de cobertura do desenho demonstrada; `ProbMinCenariosRho`/`ProbMaxCenariosRho` são **extremos dos quatro valores hipotéticos de rho**, não intervalo posterior; E08.1 contém PPC marginal, E08.3 usa aproximação aos momentos e E08.4 contém limites dos **cinco proxies**, não probabilidade conjunta identificada de melhora de todos os eixos.

Na apresentação, associar cada número a **indicador, unidade, janela, direção favorável, rho postulado e método**. `rho=0` em E09 não é correlação empírica nem independência demonstrada. Ver a [nota científica atualizada](../../reports/pnadc_estudo_i/nota_cientifica.md). A mudança T03.6 foi **somente documental**, sem alteração dos 15 fontes históricos, dados ou resultados; recebeu QA independente com ressalvas e CI no SHA do PR #35. A T03.7 também foi integrada, enquanto T01.3–T01.7/Issue #14 continuam abertas.

## Invariantes matemáticas e casos sintéticos (T03.7)

O [relatório T03.7](../../reports/pnadc_estudo_i/invariantes_sinteticos_T03_7.md) descreve os ensaios executáveis em R de variância, sinal e limites dos contrastes gaussianos, suporte de `theta`, escala do CV, arredondamento por 16 vértices, limites de Fréchet para **cinco proxies** e integridade descritiva da projeção E09. A suíte `tests/testthat/test-pnadc-invariantes-sinteticos-t03-7.R` usa **casos sintéticos**, não reexecuta os scripts Python E08/E09 nem Stan, não inspeciona insumos institucionais e **não valida inferências históricas**. A T03.6 foi integrada no PR #35 e a T03.7 no PR #36, cada uma após QA técnico-documental independente com ressalvas e CI no respectivo SHA. A Etapa 01/P0 (Issue #14) continua aberta.

## Código e resultados aqui disponíveis

- `auditar_e10.py` e [síntese histórica E09](../../data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv): auditoria de SHA-256, cardinalidade, períodos e identidades dos **26 contrastes** (projeção descritiva de 10 colunas da E09 original).
- `estudo_I/e04/construir_base_e04.py` e `estudo_I/e05/explorar_e05.py`: fontes originais de construção e exploração; exigem a extração SIDRA local e o manifesto de transporte.
- `estudo_I/e07/stan/`: **três modelos Stan originais** usados na E07 histórica, não reamostrados neste trabalho de GitHub.
- `estudo_I/e07/run.ps1`: comando Windows original, exige base E04 e R/CmdStanR local.
- `estudo_I/e07/R/`: código histórico R/CmdStanR (reprocessamento de resumos disponível; MCMC somente por comando explícito).
- `estudo_I/e08/`: quatro scripts Python E08.1–E08.4; diagnóstico, sensibilidades e confronto documental históricos, exigem entradas externas.
- `estudo_I/e09/consolidar_e09.py`: código **histórico** de consolidação E09; não roda isolado, exige as saídas E08.
- [Nota científica](../../reports/pnadc_estudo_i/nota_cientifica.md) com limitações inferenciais.

**Fontes preservados integralmente:** os **15 arquivos de código** do pacote local E10 (Python E04, E05, E08.1–E08.4 e E09; R/CmdStanR E07; três modelos Stan; run.ps1 Windows), no diretório `estudo_I/`. A identidade do conteúdo foi conferida por hashes Git SHA-1 (**15/15 correspondências**). Isso comprova fidelidade de transporte dos fontes, não reexecução.

**Limite da execução pública:** os metadados SIDRA, o snapshot de 22.130 linhas, as saídas intermediárias E04–E08, a E09 integral e as 52 cadeias Stan históricas permanecem **apenas no pacote E10 local**, fora do GitHub. O coletor original da API SIDRA não foi recuperado. Scripts originais requerem os respectivos insumos e, alguns, os caminhos históricos; **não** executar `run.ps1 -Modo completo` para auditar saídas históricas. Não afirmar reprodução E04–E09 com um clone isolado.

## Comando público testado

Na raiz do repositório:

```bash
python scripts/pnadc_estudo_i/auditar_e10.py
```

Testado em diretório local de ensaio com Python 3, **sem pacotes externos**, em 02/11/2026. Verifica tabela congelada e cálculos aritméticos, **não** recalcula E04–E09, baixa dados ou faz MCMC.

## Modelo e identificabilidade

E07 histórica: para cada agregado `y` com coeficiente de variação publicado (CV), `s=|y|×CV/100` e `y|θ,s ~ Normal(θ,s)`. Taxas: `θ=100p,p~Beta(1,1)`; totais: `θ=P14×q,q~Beta(1,1)` com P14 fixa; rendimento: `θ=μ,μ~Normal(0,10000)`, restringida a `μ>0`. São **13 modelos marginais**, não um sistema conjunto identificado. A E08/E09 produziu cenários para correlações temporais **hipotéticas** (`ρ=-0,5;0;0,5;0,9`); não há covariâncias reais suficientes para `P(todos os 5 eixos melhorarem|dados)`.

Desenho amostral complexo, sobreposição de trimestres móveis, arredondamento, mudanças de definição e acesso apenas a agregados impedem alegar causalidade ou posterior conjunta. R-hat e ESS favoráveis não resolvem identificabilidade.

Fonte: [IBGE/SIDRA](https://sidra.ibge.gov.br/) e [Quadro Sintético PNADC 202609](https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Mensal/Quadro_Sintetico/2026/pnadc_202609_quadroSintetico.pdf). Licença específica da vintage de extrato **não confirmada**, por isso não publicamos o snapshot integral nem as cadeias. Ver inventário `data/DATA_PROVENANCE.csv`.

**Ambiente histórico E10:** Python 3 e versões observadas em `requirements-historicos.txt`. A instalação das versões não foi testada em novo clone. Scripts originais podem gravar saídas nos diretórios legados `estudo_I/e04/saida`, `estudo_I/e05/saida` ou `estudo_I/e08/`; eles foram preservados literalmente para auditabilidade, mas esses caminhos não representam a convenção atual `outputs/<estudo>/<tipo>/`. Uma execução independente futura precisa insumos e adaptação de caminhos, com validação antes de publicar saídas.

**Estados:** fontes (15/15) conferidos por identidade Git; síntese descritiva conferível neste repositório; inferências MCMC históricas e cenários condicionais; reprodução integral desde API/MCMC não testada aqui.
