# E10 — Comunicar, reproduzir e preservar sem ampliar indevidamente as conclusões

**Estudo:** mortalidade registrada de pessoas de 0 a 74 anos nos distritos administrativos de São Paulo, 2006–2023. **Data:** 28/10/2026. **Modo Estudo.**

## 1. A questão de comunicação não é escolher o gráfico mais bonito

Uma série de mortalidade pode ser corretamente calculada e ainda assim conduzir o leitor a uma conclusão inadequada. O exemplo central deste estudo é 2023: frente a 2019, a taxa bruta ficou 2,8% acima, mas a taxa padronizada por sexo × idade, usando os pesos populacionais internos de 2019, ficou 5,0% abaixo. As duas medidas são matematicamente compatíveis: elas respondem a perguntas diferentes. Apresentar somente uma delas ocultaria parte da história.

Comunicar bem exige declarar **qual população, qual numerador, qual denominador, qual comparação e qual alcance inferencial**. Na E10, não se altera o estimando da E09 nem se recalcula um novo resultado apenas para torná-lo mais publicável. Preservamos as medidas calculadas, explicamos suas consequências e delimitamos o que ainda é desconhecido.

## 2. Uma resposta substantiva em quatro movimentos

**Mudança de 2006 a 2019.** A taxa bruta passou de 383,34 a 383,89 óbitos por 100 mil pessoas estimadas na janela 0–74 anos, diferença de apenas +0,55. A taxa diretamente padronizada com estrutura de 2019 caiu de 482,54 a 383,89, diferença de −98,65 por 100 mil (−20,4%). A decomposição simétrica da diferença bruta associa −88,08 por 100 mil à mudança das taxas específicas e +88,63 à composição populacional. Não se trata de dois resultados contraditórios: o padrão fixo e a decomposição simétrica usam pesos distintos.

**Mudança de 2019 a 2021.** A taxa bruta foi de 383,89 a 559,07 (+45,6%); a padronizada, de 383,89 a 536,57 (+39,8%). Sob a decomposição simétrica, +156,17 por 100 mil está no componente de taxas específicas e +19,02 no componente da composição. O aumento não se esgota em uma mudança da distribuição por idade e sexo.

**Mudança de 2019 a 2023.** A taxa bruta alcançou 394,67 (+2,8%); a padronizada, 364,64 (−5,0%). A decomposição simétrica descreve −20,09 por 100 mil associados às taxas específicas e +30,87 à composição. A conclusão depende do indicador: não há uma única sentença que substitua ambos.

**Validação preditiva.** No ano de 2021, a NB2 ajustada com dados até 2019 produziu 42.279,93 óbitos previstos, ante 61.166 observados (razão 1,447 e diferença de aproximadamente 18.886). Intervalos preditivos celulares com 95% nominal cobriram 73,30% das células. O bootstrap distrital da E08 produziu intervalo de 69,93% a 76,77% para a *cobertura empírica*. Esse intervalo não mede a incerteza do total de óbitos nem da diferença observado–previsto. O desvio é uma falha de previsão perante um choque/regime diferente; **não** identifica causalmente mortes decorrentes da pandemia.

## 3. Por que a padronização pertence ao argumento, não apenas ao apêndice

Para cada ano t e estrato k de sexo × idade, definimos Y(t,k) como a soma de óbitos dos distritos e P(t,k) como a soma das exposições populacionais. A taxa do estrato é r(t,k) = 100.000 × Y(t,k)/P(t,k). Escolhemos w(2019,k) = P(2019,k)/soma_k P(2019,k), que soma 1 entre os 30 estratos. A taxa padronizada de um ano usa soma_k w(2019,k) × r(t,k). Fixar os pesos responde: **como se comparam as taxas específicas se a estrutura por idade e sexo for a mesma?**

No código, essa operação não é `mean(taxa)` nem `sum(óbitos)/sum(população)` aplicado depois de agregações inadequadas. Exige preservar os 30 estratos, estimar as taxas em cada um e recombiná-los com os mesmos pesos. O script `script/03_E09_calcular.R` contém essas operações literais. O arquivo `tabelas/E09_pesos_2019_sexo_idade.csv` permite conferir pesos e cada parcela.

A decomposição simétrica responde outra pergunta: quanto da diferença bruta pode ser atribuído *algebricamente* à troca de taxas específicas e quanto à troca de pesos, usando as médias dos dois períodos? Sua soma é uma identidade exata. Não é atribuição causal do envelhecimento nem do período pandêmico.

## 4. O que a modelagem acrescentou e o que não acrescentou

A E07 ajustou modelos de contagem Poisson e NB2 em R, com `offset(log(populacao))`, efeitos de idade × sexo, efeitos fixos de distrito e tendência temporal. A E08 comparou períodos de teste e examinou calibração, cobertura, sensibilidade e persistência de resíduos entre distritos ao longo de anos. A NB2 serviu como referência entre os modelos avaliados, mas não está homologada para projetar rupturas futuras ou para calcular excesso de mortalidade atribuível a uma causa.

Nenhum modelo Stan foi implementado neste estudo. A simples disponibilidade dos manuais de Stan e Jaynes no projeto não substitui evidência empírica de ajuste, diagnóstico ou identificação causal.

## 5. Três níveis de reprodução diferentes

1. **Recalcular a descrição E09 com entradas congeladas:** possível com `dados/base_analitica_celular_PROVISORIA.csv`, `dados/E07_R_validacao_anual.csv` e `dados/E08_incerteza_cobertura_bootstrap_distrito.csv`. O código Python foi executado anteriormente e a auditoria da E10 reconfirmou os principais números e reexecutou as rotinas Python E08/E09, reproduzindo as tabelas históricas. O script R foi entregue, mas não executado neste ambiente.
2. **Reajustar E07 e reavaliar E08/E09:** possível *em princípio* desde a base E04 derivada, pelo `script/reproduzir_E10.R --modo=completo`, com R e MASS. Esse caminho não foi executado durante a E10. A reamostragem bootstrap em R usa gerador pseudoaleatório diferente daquele de NumPy; os percentis simulados podem variar ligeiramente. O pipeline não deve ser anunciado como testado enquanto não for efetivamente executado.
3. **Reconstruir desde as fontes primárias:** **ainda não é possível** com este pacote. O script E04 exige `obitos_APVP_2006_2023_0a74.csv`, `populacao_SEADE_2006_2023_0a74_LONG.csv` e `crosswalk_distritos_PROVISORIO.csv`, indisponíveis. A documentação sobre a estabilidade dos limites administrativos históricos e dos denominadores também continua pendente. Sem essas fontes não se certifica a correspondência territorial.

Esses três níveis não são equivalentes. Um teste de consistência de uma base derivada não certifica a origem; uma concordância entre duas implementações não valida pressupostos científicos; e convergência numérica de um ajuste não resolve identificação causal.

## 6. Como preservar resultados interpretáveis

A unidade principal é este caderno consolidado. Os diretórios `dados/` e `fontes_historicas/` preservam insumos e evidências recebidos; `tabelas/` e `figuras/` são as saídas congeladas da E09; `script/` contém o código R de ajuste e interpretação e a auditoria Python; `execucoes/`, quando criado, registra novas execuções sem substituir a referência. O manifesto SHA-256 identifica byte a byte o conteúdo entregue, não sua qualidade estatística. O arquivo `provas/VALIDACAO_PYTHON_E10.json` contém os testes efetivamente executados na E10.

Publicar o repositório não é o mesmo que demonstrar reprodutibilidade desde os dados originais. Se as fontes primárias forem anexadas, reconstruir a E04 em um diretório novo, comparar as chaves e todos os totais, revisar o problema de correspondência territorial e então avaliar se as conclusões agregadas permanecem. Não sobrescrever as evidências congeladas.

## 7. O texto público pode afirmar, sem extrapolar

> No estudo exploratório, a taxa bruta de mortalidade de pessoas de 0 a 74 anos praticamente não mudou entre 2006 e 2019, mas a taxa padronizada por idade e sexo caiu. A diferença decorre da mudança da composição populacional e das taxas específicas, medidas sobre uma base territorial provisória. Entre 2019 e 2021, a alta permanece expressiva após padronização. Os modelos preditivos ajudam a identificar desajustes, mas não estabelecem suas causas.

Não devemos afirmar que a base oferece “riscos individuais”, que o modelo estima “mortes causadas pela pandemia”, que as comparações entre distritos são oficiais ou que há intervalos calibrados de incerteza para a diferença agregada. Também não está autorizado publicar o estudo fora desta conversa; os textos são rascunhos preparados para possível revisão e decisão do usuário.

## 8. Fechamento E10

O ganho do percurso é metodológico: uma boa pergunta exige definir unidade, selecionar o denominador correto, verificar integridade de junções, comparar indicadores sob composições diferentes, compreender o que um modelo prevê e reconhecer quando um resultado não pode ser traduzido em causalidade. O que sabemos é delimitado, mas suficiente para uma conclusão exploratória útil. O que ainda não sabemos está registrado em relação a fontes e verificações concretas, sem reiniciar as dez etapas ou criar novas aprovações formais.

**Concluído:** narrativa, tabelas e gráficos preservados, código de reprodução R organizado, auditorias e reexecuções Python E08/E09 executadas, documentos de publicação redigidos, inventário e manifesto do pacote. **Ainda não demonstrado:** execução R E10, remontagem APVP–SEADE desde fontes primárias, homologação territorial histórica, calibração preditiva agregada e identificação causal. **Próximo passo, apenas se necessário:** executar o comando R disponibilizado ou anexar as fontes primárias para as verificações de procedência e uso territorial que permaneceram abertas.