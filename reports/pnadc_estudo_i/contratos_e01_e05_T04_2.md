# T04.2 — Contratos documentais E01–E05 | PNAD Contínua, Estudo I

**Natureza:** `RECONSTRUCAO_EDITORIAL` datada de 09/11/2026; não é um conjunto de relatórios E01–E05 contemporâneos nem pré-registro.  
**Tarefa literal:** “Documentar E01–E05 com dicionário, proveniência, contratos de dados e verificações efetivamente disponíveis”.  
**Base Git:** `main` `94b5c23643d68cc34f47f39485a38905cab774a1`, após integração T04.1/PR #37.  
**Objeto:** estudo aplicado de agregados nacionais da PNAD Contínua; nunca microdados individuais.

O [E00 editorial T04.1](e00_consolidado_T04_1.md) apresenta a pergunta original referida no README, 13 indicadores substantivos em cinco eixos e duas janelas de contraste. Este inventário **não confirma execução independente de E01–E05**. Registra o que existe na árvore Git e os contratos declarados nos fontes históricos E04/E05. Executar um `assert` na presença dos insumos apropriados não equivale a tê-lo executado nesta tarefa.

## 1. Evidência por etapa

| Etapa | Artefato versionado encontrado | Alcance comprovável no clone | Estado documental |
| --- | --- | --- | --- |
| E01 | Não há pasta nem documento E01 contemporâneo em `scripts/pnadc_estudo_i/estudo_I/`. | Nem plano original de coleta nem consulta SIDRA original foram recuperados. | `NAO_DISPONIVEL_NO_GIT`; descrição atual é editorial. |
| E02 | Não há fonte E02 independente versionada. | Metadados JSON e snapshot SIDRA mencionados por E04 estão fora da árvore. | `NAO_DISPONIVEL_NO_GIT`; sem validação de extração. |
| E03 | Não há fonte E03 independente versionada. | `MANIFESTO_SHA256.csv` e controles do transporte E10 são referidos no código histórico, mas não publicados no clone. | `NAO_DISPONIVEL_NO_GIT`; integridade da coleta não reavaliada. |
| E04 | [`construir_base_e04.py`](../../scripts/pnadc_estudo_i/estudo_I/e04/construir_base_e04.py) | Algoritmo, dicionário, esquema, verificações e nomes de produtos inspecionáveis; sem entradas para reexecutar. | `FONTE_HISTORICA_VERSIONADA` / execução atual `NAO_REPRODUZIDA`. |
| E05 | [`explorar_e05.py`](../../scripts/pnadc_estudo_i/estudo_I/e05/explorar_e05.py) | Transformações descritivas, contratos e nomes de produtos inspecionáveis; sem E04 local nem manifesto para reexecutar. | `FONTE_HISTORICA_VERSIONADA` / execução atual `NAO_REPRODUZIDA`. |

**Distinção temporal:** nomes e estados E01–E03 são mapeamento editorial das lacunas, não evidência de que essas etapas foram formalizadas na época. O README conserva histórico do transporte de 15 fontes E04–E09 e verificações de identidade por hash Git; isso não reconstitui extração da API, snapshot, execução E04/E05 nem seus artefatos.

## 2. Proveniência, universo e vintage

**Origem declarada nos fontes:** IBGE/SIDRA, PNAD Contínua agregada para a localidade `Brasil`; [catálogo SIDRA](https://sidra.ibge.gov.br/). A extração histórica foi montada a partir de variáveis de tabelas SIDRA identificadas no dicionário da seção 3, não de microdados. O coletor original e a URL parametrizada exata utilizada na coleta **não estão disponíveis** no clone. O vínculo de cada variável/categoria com sua tabela é **declarado no código**; metadados JSON correspondentes e a vintage completa não foram confrontados com a API nesta T04.2.

**Escopo temporal declarado no E04:** 175 referências mensais `AAAAMM` de `201203` a `202609`, cada uma representando o último mês de um trimestre móvel de três meses. As comparações do piloto são `202509 → 202609` (anual) e `202606 → 202609` (recente). Dentro de cada par, meses de referência não se sobrepõem; os dois contrastes compartilham o mesmo destino `202609`. O desenho amostral rotativo impede deduzir independência entre estimadores apenas da disjunção de meses.

**Unidade de observação analítica:** par (indicador, trimestre móvel) em agregado nacional. A população de 14 anos ou mais é suporte para alguns indicadores, não denominador universal. Contagens são totais populacionais expandidos em milhares; taxas têm denominadores distintos e rendimento é média real conforme a vintage da extração. Não interpretar o banco como 2.405 pessoas nem como painel de respondentes.

**Governança:** [`data/DATA_PROVENANCE.csv`](../../data/DATA_PROVENANCE.csv) classifica `pnadc_estudo_i` como `aggregates_only`, com fonte oficial pública e licença específica da vintage ainda não confirmada. O recorte público versionado é [E09 descritiva, 26 linhas](../../data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv), não a coleta SIDRA integral. Etapa 01/P0, T01.3–T01.7 e [Issue #14](https://github.com/riscotta/bayes-stan-r/issues/14) continuam sem homologação; nenhuma publicação adicional de dados ou nova release é autorizada por este relatório.

## 3. Dicionário versionado dos 13 indicadores e um apoio

Cada linha abaixo transcreve um registro do vetor `ESPECIFICACOES` em E04. `Tabela`, `Variável`, `Categoria` e `CV` são **IDs SIDRA declarados no código**, ainda não conferidos contra a extração histórica nesta revisão; `—` significa categoria vazia na especificação. `CV` designa a variável de coeficiente de variação, não uma covariância.

| IndicadorId | Eixo / papel | Tabela | Variável | Categoria | CV | Medida e denominador declarado |
| --- | --- | ---: | ---: | ---: | ---: | --- |
| `desocupacao_pct` | A / principal | 6381 | 4099 | — | 4103 | Desocupados / força de trabalho (%). |
| `desocupados_mil` | A / principal | 6318 | 1641 | 32446 | 4087 | Desocupados (mil pessoas; total expandido). |
| `nivel_ocupacao_pct` | B / principal | 6379 | 4097 | — | 4101 | Ocupados / população 14+ (%). |
| `participacao_pct` | B / principal | 5944 | 4096 | — | 4100 | Força de trabalho / população 14+ (%). |
| `ocupados_mil` | B / principal | 6318 | 1641 | 32387 | 4087 | Ocupados (mil pessoas; total expandido). |
| `forca_trabalho_mil` | B / principal | 6318 | 1641 | 32386 | 4087 | Ocupados + desocupados (mil pessoas). |
| `populacao_14mais_mil` | B / apoio | 6318 | 1641 | 32385 | 4087 | População 14+ (mil pessoas; apoio/denominador). |
| `subutilizacao_pct` | C / principal | 6441 | 4118 | — | 4119 | Desocupados + subocupados por horas + força potencial / força ampliada (%). |
| `subocupacao_horas_pct` | C / principal | 6785 | 9819 | — | 9820 | Ocupados subocupados por horas / ocupados (%). |
| `desalentados_mil` | C / principal | 6438 | 1641 | 46254 | 4087 | Desalentados (mil pessoas; total expandido). |
| `desalento_pct` | C / principal | 6807 | 9869 | — | 9870 | Desalentados / (força de trabalho + desalentados) (%). |
| `informalidade_pct` | D / principal | 8513 | 12466 | — | 12467 | Ocupados em condições de informalidade / ocupados (%). |
| `contrib_previdencia_pct` | D / principal | 3919 | 8463 | — | 8464 | Ocupados contribuintes / ocupados (%); **não** é 100 − informalidade. |
| `rendimento_real_habitual_rs` | E / principal | 6390 | 5933 | — | 5941 | Média mensal real habitual de ocupados com rendimento (R$ da vintage). |

O E04 declara diferentes numeradores/denominadores no campo `Numerador`/`Denominador`. A enumeração central é **A=2, B=4, C=4, D=2, E=1 = 13**; `populacao_14mais_mil` é um 14º registro **de apoio**, não um 14º indicador substantivo nem um sexto eixo. O universo para rendimento é o dos ocupados com rendimento, diferente do denominador da taxa de desocupação. Mudanças de definição/conceito não são corrigidas automaticamente pelo código.

## 4. Contrato de entrada e transformações E04

**Entradas exigidas no pacote histórico, não versionadas no repositório:**

- `dados/04_series_csv/pnadc_todas_variaveis_brasil_ate_202609.csv`: CSV `;`, UTF-8 com BOM, 22.130 linhas esperadas e colunas **exatas** `Tabela;Periodo;PeriodoDescricao;Localidade;VariavelId;Variavel;CategoriasIds;Categorias;Unidade;ValorOriginal`;
- `dados/02_metadados/tabela_{Tabela}_metadados.json`: IDs de tabela/variáveis, unidade, categoria e frequência `trimestral móvel` a confrontar; o conteúdo JSON não está publicado no clone;
- Python 3 e `pandas`, versões precisas do pacote histórico não reproduzidas aqui.

**Identidade da entrada:** E04 exige localidade Brasil, 175 períodos contínuos, ausência de duplicatas em `Tabela,Periodo,VariavelId,CategoriasIds`; compara SHA-256 da entrada antes/depois, sem modificá-la. Emparelha valor e CV na mesma tabela/categoria/período/localidade mediante junção `one_to_one`, distinguindo `ValorOriginal`, `CVOriginal`, `Valor` e `CVPercentual`. Apenas números decimais passam como valor; `...` representa `NAO_DIVULGADO_OFICIAL`, **não zero**. Unidades em células sem divulgação são preenchidas em `UnidadeAnalitica` com a declaração do metadado, sem alterar `Unidade` original.

**Saída longa prevista:** 2.405 linhas × 31 colunas; 2.230 linhas de indicadores principais e 175 de apoio. A taxa oficial de informalidade tem só 130 períodos, iniciando `201512`, não dados retroprojetados para 2012–2015. Há quatro ausências numéricas declaradas: duas taxas de horas/subutilização em `201510` e `201511` (2 indicadores × 2 períodos). `RegimeConceitual2015` separa antes/depois da ruptura e transição não divulgada. O código contém invariantes para essas contagens, mas não foram executados sobre a entrada ausente nesta tarefa.

**Saídas E04 declaradas:** `estudo_I/e04/saida/base_analitica_e04_longa.csv`, `base_analitica_e04_larga.csv`, `resumo_cobertura_e04.csv` e `auditoria_e04.json`. A base larga prevista possui 175 períodos, com colunas para 14 indicadores, seus 14 CVs e três campos temporais (31 colunas). As verificações de força de trabalho = ocupados + desocupados (tolerância até 1 mil pessoa) e identidades de quatro taxas (até 0,051 ponto percentual) são **asserções históricas do script**. Valores de referência `202609` estão codificados; não foram confrontados nesta execução com a extração ausente.

## 5. Contrato E05 e limites descritivos

**Entradas exigidas:** a base E04 longa (2.405 × 31) e larga (175 × 31), mais `MANIFESTO_SHA256.csv` e arquivos de controle do pacote histórico. O E05 verifica SHA-256 e tamanhos de itens imutáveis sob `dados/02_metadados/` e `dados/04_series_csv/`, não a licença/privacidade desses itens. Requer Python, `pandas` e `matplotlib`; caminhos locais são legados e não o padrão `outputs/<estudo>/<tipo>/`.

**Transformação:** 14 registros de `contrastes_e05.csv`, inclusive o de apoio, com níveis de `202509`, `202606`, `202609`, diferenças `atual − origem` e CV percentual atual; 16 registros de `distribuicoes_temporais_e05.csv`, pois duas taxas de horas são separadas em dois regimes conceituais; gráficos PNG para os eixos A–E; `auditoria_e05.json`. Mediana temporal e extremos são estatísticas **ao longo dos trimestres publicados**, não distribuições de indivíduos. Não há estimador de diferença ajustado ao desenho nem inferência posterior nessa etapa.

**CV e incerteza:** em E04, `CVPercentual` é % da estimativa agregada. A construção exploratória `s=|y|×CVPercentual/100` apenas aproxima o erro-padrão marginal na unidade de `y`; CV não informa covariância temporal ou entre indicadores. Como o código E05 não estima essas covariâncias, os contrastes anuais e recentes não possuem, por essa etapa, distribuição conjunta identificada. `202609` é destino de ambos os contrastes; não supor independência.

**Cortes e risco de comparabilidade:** as duas taxas de horas têm quebra em 2015; `201510` e `201511` são não publicados, não valores imputados; informalidade somente desde `201512`; informalidade ≠ complemento da contribuição previdenciária; não fazer deflação adicional ao rendimento real da mesma vintage. O fato de março de 2012 a setembro de 2026 incluir 175 trimestres móveis não cria 175 observações amostrais independentes.

## 6. Quadro de verificações realmente disponíveis

| Verificação | Evidência observável no Git | Status nesta T04.2 |
| --- | --- | --- |
| Existência e conteúdo dos scripts E04/E05 | Dois arquivos Python históricos legíveis e links estáveis. | `VERIFICADO_ESTATICAMENTE` |
| Cardinalidade e especificação dos indicadores | 14 tuplas `ESPECIFICACOES`, 13 principais e uma de apoio no código. | `VERIFICADO_ESTATICAMENTE` |
| Contratos de CSV, metadados, hash, CV e lacunas | Declarações `assert`/`exigir` inspecionáveis nos fontes. | `DECLARADO_NO_CODIGO`; não executado em dados ausentes. |
| Descrição pública de E09 | CSV versionado com 26 contrastes para 13 indicadores, duas origens e destino 202609; valores/deltas auditáveis aritmeticamente. | `VERIFICAVEL_NO_CLONE`; não substitui E04/E05. |
| Coleta SIDRA original, 22.130 linhas e metadados JSON | Insumos fora do clone, sem coletor original recuperado. | `NAO_REPRODUZIDO` |
| E04 (2.405/175/31), E05 (14/16), CVs e gráficos efetivos | Saídas locais históricas não versionadas. | `NAO_REPRODUZIDO` |
| Cobertura científica, MCMC, posterior conjunta ou correlação | Não identificável a partir de contrato estrutural e E09 descritiva. | `NAO_HOMOLOGADO` |
| Licença/privacidade da extração | P0/Issue #14 aberta. | `PENDENTE_INSTITUCIONAL` |

**Gate para revisão T04.2:** conferir linha a linha o dicionário contra `ESPECIFICACOES`, diferenciar valores de suporte e totais expandidos, conferir os caminhos e dimensões contra E04/E05, validar 26 contrastes do CSV público, executar os testes novos em CI no HEAD final e registrar limitações de falsos positivos. Revisão documental independente não substitui homologação da vintage nem reexecução científica.

## 7. Fontes, decisões e próxima dependência

Fontes locais de base: [README PNAD](../../scripts/pnadc_estudo_i/README.md); [E00 consolidado](e00_consolidado_T04_1.md); [E04](../../scripts/pnadc_estudo_i/estudo_I/e04/construir_base_e04.py); [E05](../../scripts/pnadc_estudo_i/estudo_I/e05/explorar_e05.py); [inventário T03.1](inventario_estimandos_e07_T03_1.md); [auditoria T03.4](auditoria_suporte_cv_arredondamento_T03_4.md); [nota científica](nota_cientifica.md); [E09 descritiva](../../data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv); [proveniência](../../data/DATA_PROVENANCE.csv).

**Decisão editorial:** preservar todos os artefatos históricos e dados, não reconstruir E01–E03 como documentos originais, não executar o Python sobre dados substitutos ou públicos de vintage distinta e não interpretar testes estáticos como reprodução. T04.3 poderá tratar a especificação matemática E06, separadamente, após T04.2 receber QA independente e integração. T01.3–T01.7/Issue #14 e Issues #15/#16 continuam abertas.
