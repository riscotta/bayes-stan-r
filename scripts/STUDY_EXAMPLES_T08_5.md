# T08.5 — O que os resultados dizem sobre cada fenômeno

Os oito exemplos seguem o [guia T08.4](STUDY_SUMMARIES_T08_4.md) e usam resultados publicados na árvore de `4a3cebe3b1f40fc70cb900ecd7a165ab66b6dddc`. A interpretação mantém o recorte e as limitações da análise de origem. A [decisão de proveniência](../data/DATA_PROVENANCE_DECISIONS.md) documenta as lacunas das cópias históricas.

## Cães — quanto a raça acrescenta à previsão do F5?

Acrescentar a raça declarada ao modelo com idade, sexo, porte e esterilização reduziu o RMSE interno entre 0,85% e 1,41%, conforme a família do modelo. A melhora foi pequena e o modelo com raça teve erro maior no primeiro dos cinco folds em todas as famílias. Isso descreve o ganho preditivo dentro da coorte já explorada. O F5 é um escore de questionário e a análise não valida previsão individual de mordidas ou generalização para outra população. [Relatório E10, Resultados e Distribuições preditivas](../reports/caes_raca_f5/relatorio_cientifico.md#resultados)

## Mortalidade em São Paulo — a taxa depende da composição que se compara

De 2019 a 2023, a taxa bruta de mortalidade de 0–74 anos subiu 2,8% e a taxa padronizada caiu 5,0%. A primeira descreve a mortalidade com a composição por sexo e idade de cada ano. A segunda usa a composição de 2019 para comparar as taxas específicas. Assim, a população observada em 2023 teve taxa bruta maior mesmo com taxa padronizada menor. A correspondência territorial segue provisória e a comparação é descritiva. [Síntese E10](../reports/mortalidade_sp_0_74/relatorio_cientifico.md#resultados)

## PNAD Contínua — o desemprego pode cair com aumento do desalento

A taxa publicada de desocupação chegou a 5,2% em setembro de 2026. Na comparação de junho a setembro, o total publicado de desalentados passou de 2,288 para 2,459 milhões. Os dois indicadores captam dimensões distintas e mostram por que a taxa de desocupação, sozinha, não resume a evolução recente do mercado de trabalho. A comparação usa agregados publicados e arredondados. As probabilidades históricas dos contrastes dependem da correlação temporal postulada no cenário gaussiano. [Nota científica](../reports/pnadc_estudo_i/nota_cientifica.md)

## Mega-Sena — por que a posição publicada muda a frequência das dezenas?

A posição na lista publicada depende da ordenação das seis dezenas. Uma dezena pequena pode aparecer com frequência diferente na primeira posição porque essa posição recebe o menor valor do concurso. A associação por posição é esperada pela ordenação. Nos fatores principais, como os recortes temporais e o dia da semana, a análise histórica sustenta estabilidade prática na escala examinada. A comparação por ano ficou como sensibilidade exploratória e o estudo não sustenta previsão de concursos futuros. [Conclusão no README](mega_sena_dezenas/README.md#conclusão-substantiva-do-estudo) · [Mensagens E09](../data/raw/mega_sena_dezenas/tabelas_auditadas/E09_mensagens_chave.csv)

## IOM Missing Migrants — dias de exposição e volume de travessias

A tendência média anual positiva do ajuste histórico se refere à carga registrada de mortos e desaparecidos nas rotas analisadas. O modelo contabiliza a exposição em dias. Para comparar o risco de uma pessoa em travessia seria necessário um denominador compatível com o volume de migrantes expostos, além de lidar com a cobertura dos registros. A interpretação da tendência também depende das ressalvas de convergência e das falhas das checagens preditivas em zeros e dispersão. [Conclusão no README](missing_migrants/README.md#conclusão-substantiva-do-estudo)

## Sherlock Holmes — comparar menções com a extensão das obras

Sherlock Holmes e Dr. Watson aparecem no topo das taxas globais estimadas entre os termos estudados. Para comparar obras de extensões diferentes, a análise usa taxas por 10 mil tokens. Esse denominador permite distinguir uma taxa alta de menções de um total alto em uma obra longa. A leitura vale para o corpus e o vocabulário selecionados, com cautela adicional nos contrastes de termos raros e sensibilidade a priors alternativos e à NB2 ainda pendente. [Mensagens finais](../data/raw/sherlock_holmes_terms/tabelas_finais_auditadas/tabela_final_mensagens_chave.csv)

## Porto Alegre — o que significa sobreviver cinco anos no cadastro?

A sobrevivência observada aos 60 meses foi de 57,9% na amostra analítica de estabelecimentos de alimentação fora do lar. O evento é a baixa cadastral. O indicador descreve a permanência antes desse evento administrativo. A operação econômica pode terminar em data diferente, e o estudo não valida previsão para coortes futuras. [Resultados e limites no README](sobrevivencia_cadastral_poa/README.md#resultados-principais)

## UNODC — a composição temporal afeta o contraste por sexo

No painel pareado, a diferença média observada entre as proporções feminina e masculina de presos sem sentença foi de +3,72 pontos percentuais. No painel temporal completo, com composição temporal fixa, a diferença média observada foi de +1,79 ponto percentual. A direção se manteve e a magnitude diminuiu sob a padronização. Essa sensibilidade importa ao resumir registros de períodos diferentes. O contraste é observacional e a avaliação agrupada por país não validou sua extrapolação para novos países. [Resultados centrais no README](prisons_prisoners_unodc/README.md#resultados-centrais-do-estudo)
