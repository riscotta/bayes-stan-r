# T03.4 — Auditoria de suporte, CV, arredondamento e dependência temporal (PNAD Contínua, Estudo I)

**Natureza:** auditoria metodológica **estática** dos fontes E04–E09 já versionados e verificações **sintéticas**. **Não** é reexecução da PNAD, homologação de CVs por indicador, estimação de covariâncias, posterior temporal conjunta ou validação científica das 26 probabilidades históricas.
**Base examinada:** `main` `3c0c560921deabd18fe97a2eadb778c46fa07f62` (08/11/2026); continuação de T03.1–T03.3.
**Escopo:** 13 indicadores substantivos, dois contrastes por indicador (202509→202609 e 202606→202609), quatro correlações temporais **hipotéticas** (ρ = −0,5; 0; 0,5; 0,9). Nenhum arquivo de dados institucionais, saídas históricas, scripts E04–E09, modelos Stan ou configurações de CI foi modificado.

## 1. Matriz de verificação e conclusão

| Dimensão | Evidência no código preservado | Diagnóstico | Condição para avançar |
| --- | --- | --- | --- |
| Suporte de taxas | `e07/stan/taxa_marginal.stan`: `0 ≤ p ≤ 1`, `theta=100*p`; `e08/sensibilidade_e082.py`: Normal sem truncamento do contraste | **Divergência metodológica:** suporte marginal [0,100] em E07; a aproximação E08.2 permite massa fora do suporte dos níveis (e potencialmente de diferenças fisicamente admissíveis) | Quantificar massa de cauda para dados observados, ou justificar aproximação local; T03.5 decide alternativa |
| Suporte de contagens | `contagem_marginal.stan`: `0 ≤ q ≤ 1`, `theta=P14*q` com P14 fixa; E08.3 classifica `_mil` como contagem | Normal E08.2 não impõe `0 ≤ theta ≤ P14`; denominação/P14 tem incerteza ignorada pela condição | Verificar consistência das unidades, P14 e efeitos das restrições antes de conclusão |
| Suporte de rendimento | `rendimento_marginal.stan`: `mu ≥ 0`; E08.2 usa Normal não truncada | Há massa formal em níveis negativos para alguns pares de média/escala, não quantificada nos 26 casos | Avaliar tamanho da massa a partir de entradas autorizadas |
| CV e escala | `e04/construir_base_e04.py` emparelha `ValorOriginal` e `CVOriginal` por chave; `e08/sensibilidade_e082.py` exige CV numérico e `CVPercentual>0`, usa `s=abs(Valor)*CVPercentual/100` | CV marginal em **porcentagem** é tratado como desvio-padrão aproximado, não como matriz de erro ou desvio de posterior E07; `CV>0` sozinho não impede `s=0` se `Valor=0`; não há validação de cobertura de desenho aqui | Conferir metadados, qualidade, arredondamento, versões e 39 entradas E04 em ambiente autorizado; verificar escalas/zeros |
| Arredondamento | `e08/sensibilidade_e083.py` supõe ±0,05 nas taxas, ±0,5 nos totais/rendimento, ±0,05 p.p. no CV; enumera 16 cantos (`repeat=4`) | **Hipóteses de resolução**, não auditoria do número de casas original de cada item; extremos de cantos não são, sem prova de monotonicidade, limites globais sobre o hipercubo; sinal do delta pode não ser identificado pela subtração arredondada | Inspecionar `ValorOriginal`/`CVOriginal`, confrontar quadro oficial, e otimizar intervalos se necessário |
| Janelas e dependência | E04: `InicioJanela = fim − 2 meses`, `FimJanela=fim`; E08.2 define os pares 202509→202609 e 202606→202609 | Janelas são **disjuntas em meses de referência**, mas não se segue independência amostral: rotação de domicílios 1-2(5), dependência de estimadores e efeitos de pesos permanecem | Obter plano amostral e covariâncias de diferenças apropriadas antes de alegar inferência de desenho |
| Covariâncias | E08.2: `RHOS=(-0.5,0.0,0.5,0.9)`, `SDDelta=sqrt(s0²+s1²−2ρs0s1)`; E07 ajusta somente marginais 202609 | ρ não é estimado, nem covariância de períodos ou entre indicadores é disponibilizada. A escolha ρ=0 em E09 **não equivale a independência demonstrada** | Tratar resultados como cenários condicionais, sem posterior temporal conjunta/“todos os cinco eixos” |
| Evidência pública disponível | `data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv` fornece somente projeção **descritiva**, 26 linhas, sem CVs, P14, draws ou covariâncias; E04/E07/E08 integrais não estão no clone | **Não é possível** reavaliar quantitativamente os 26 pares, a massa real fora do suporte ou cobertura de ICs somente com o repositório | Acesso institucional e homologação P0/Issue #14, sem redistribuição |

**Estado por dimensão:** fonte e risco estrutural **verificados estaticamente**; consequências numéricas nos 26 pares **não verificadas**; validade científica das probabilidades **não homologada**. Nenhum limite sobre risco real de cauda, cobertura ou covariância foi estimado nesta tarefa.

## 2. Suporte: por que uma Normal operacional não herda as restrições de Stan

A likelihood histórica usa `y | theta,s ~ Normal(theta,s)`, mas **os parâmetros E07 têm suporte próprio**: taxa `theta∈[0,100]`; contagem `theta∈[0,P14]` com P14 fixo; rendimento `theta≥0`. Já o cenário E08.2 representa os níveis via aproximação gaussiana bivariada não truncada. Uma Normal com desvio positivo atribui massa a valores impossíveis mesmo quando sua média está dentro do suporte. O impacto **pode ser desprezível ou relevante** dependendo de `y/s` e da proximidade da fronteira; os valores reais necessários para quantificar isso não estão versionados.

Exemplo **puramente sintético**: para uma taxa `y=0,2` ponto percentual e `s=1` ponto percentual, `P_normal(theta<0)=Φ(−0,2)≈0,4207`. Não é estimativa da PNAD. Para contagem com limite hipotético P14=150, média 148 e s=4, `P_normal(theta>150)=1−Φ(0,5)≈0,3085`. Não pressupor que tais situações ocorram no estudo.

Taxas de diferentes denominadores **não são complementares**; não impor vínculos contábeis não justificados (por exemplo, informalidade ≠ complemento da contribuição à previdência). A escala dos totais `_mil` deve ser compatível com P14 em milhares. O E08.3 restringe suportes em sua quadratura, mas usa CDF normal **dos momentos** dos contrastes, não cauda exata de uma posterior temporal identificada.

## 3. CV, degenerescência e metadados

O emparelhamento por tabela, variável, categoria e período em E04 indica rastreabilidade **de código** do CV. A expressão `s_t=|y_t| CV_t/100` é coerente quando CV é percentual da estimativa; não fornece, por si, incerteza conjunta, erro do CV, distribuição do estimador de diferença ou desenho amostral. A checagem `CV>0` é necessária ao cenário histórico, mas não é suficiente para `s>0` se `y=0`. A expressão `norm.cdf(delta/sd)` no E08.2 não trata explicitamente o caso `sd=0` para os quatro ρ internos; por isso não extrapolar a robustez operacional para entradas arbitrárias. Não há evidência de `y=0` nas 39 entradas locais a partir do clone.

Não confundir `s_t` derivado do CV de estimativa agregada com `theta_sd` das cadeias E07. O uso de `e07.theta_sd` em E08.2 é checagem marginal separada. Informação de incerteza em denominadores e correlações entre taxas derivadas também não é fornecida neste escopo.

## 4. Arredondamento: eventos e envelopes

Sob **hipótese editorial de arredondamento ao mais próximo**, a representação `y*` com erro `±h_y` significa `y∈[y*−h_y,y*+h_y]` (fronteiras inclusivas apenas como cobertura conservadora; empates dependem de convenção). Para dois valores, `Delta∈[(y1*−y0*)−(h0+h1), (y1*−y0*)+(h0+h1)]`. Se esse intervalo inclui zero, a **direção verdadeira da diferença dos agregados não fica determinada pelos valores arredondados**, embora exista um delta nominal. Exemplo sintético com taxas 5,2 e 5,3: `Delta_nominal=0,1 p.p.`, `Delta∈[0,0,2]` p.p. (e não “melhora confirmada”). Dois números publicados iguais podem esconder delta diferente de zero. Em E08.3 os 16 cantos variam `y0,y1,CV0,CV1`; um mínimo em canto não é automaticamente o extremo sobre todo o domínio contínuo, sobretudo com `s(y,CV)` e correlação.

O relatório histórico de E08.4 diferencia subtrações de números arredondados de deltas oficiais publicados. T03.4 identifica o risco, **não** reconcilia números da PNAD sem acessar as fontes adequadas. Usar `CV±0,05` p.p. exige que a precisão publicada do CV seja efetivamente de uma casa decimal; a fonte local completa não está disponível para verificação pública.

## 5. Sobreposição: meses diferentes ≠ amostras independentes

O trimestre móvel encerrado em 202509 compreende julho–setembro/2025; o encerrado em 202606, abril–junho/2026; e o encerrado em 202609, julho–setembro/2026. Para **ambos** os contrastes específicos, os pares **não compartilham meses de referência**. Isso não significa independência dos estimadores nem ausência de domicílios repetidos. O esquema 1-2(5) da PNAD Contínua entrevista o mesmo domicílio em cinco trimestres consecutivos, uma vez por trimestre; a documentação do IBGE descreve sobreposição planejada entre trimestres adjacentes e do mesmo trimestre de anos diferentes. Percentuais **teóricos de rotação**, quando disponíveis, não são ρ nem covariâncias dos 13 estimandos, e não autorizam substituir `ρ=0,8` ou `0,2` em E08.2.

**Referências públicas de método:** IBGE, *Notas técnicas PNAD Contínua*, esquema 1-2(5), https://www.ibge.gov.br/biblioteca/visualizacao/livros/liv101548_notas_tecnicas.pdf ; IBGE, *II Fórum do SIPD – esquema de rotação*, https://www.ibge.gov.br/arquivo/projetos/sipd/segundo_forum/segundo_amostra.php . Observação: documentos descrevem a **arquitetura de coleta**, não a correlação empírica dos indicadores em 2025/2026.

## 6. Dependências, decisões e fronteira de aprovação

- **Decisão T03.4:** manter imutáveis os 15 fontes históricos, dados, CSVs e saídas. Registar a incompatibilidade de suporte e a impossibilidade de identificar covariância; nenhuma probabilidade E08/E09 será renomeada como posterior conjunta.
- **T03.5:** avaliar aproximação condicional **versus** modelo conjunto, com identificabilidade e tratamento do suporte; não implementar ajuste novo nesta tarefa.
- **T03.6:** revisar nota científica, nomes de campos (`IC95*`, `ProbSentido`) e rótulos históricos, distinguindo cenário, arredondamento e posterior.
- **T03.7:** ampliar invariantes e testes funcionais, inclusive casos de fronteira; a prova sintética aqui só verifica **o diagnóstico do risco**, não valida os 26 contrastes.
- **P0 T01.3–T01.7 / Issue #14:** autorizações e ambiente de privacidade ainda não homologados. **Não examinar** conteúdo institucional em ambientes públicos; nenhuma saída restrita foi exposta nesta tarefa.

### Gate específico de QA

Revisor **independente** deve confrontar cada linha da matriz com os fontes no SHA final, verificar álgebra e janelas, executar os testes R/CI, buscar falsos positivos nos testes de estrutura, conferir diff de escopo e avaliar se a distinção entre sobreposição amostral e temporal está correta. Não promover QA nem realizar merge só porque a CI está verde; a revisão pelo executor é insuficiente. A análise de riscos científicos dos resultados históricos permanece inconclusiva até dados e desenho acessíveis autorizados.
