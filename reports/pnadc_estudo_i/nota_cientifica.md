# Nota científica — PNAD Contínua, Estudo I

**Pergunta original:** “O desemprego brasileiro caiu para 5,2%. Mas o mercado de trabalho melhorou em todas as dimensões?”

**População e unidade:** Brasil, estatísticas agregadas oficiais, 13 indicadores A–E. Comparações: 202509→202609 e 202606→202609.

A taxa de desocupação publicada caiu para **5,2%** em setembro de 2026. Não se segue disso que todos os eixos melhoraram no recorte recente. Desalentados: 2.288→2.459 mil, taxa de desalento: 2,1→2,2%, subocupação por insuficiência de horas: 4,0→4,1%. A taxa composta de subutilização 12,9→13,0% e rendimento médio real 3.753→3.790 R$ foram **classificados como estáveis** pelo IBGE nessa comparação. Deltas calculados com números arredondados nem sempre coincidem com deltas oficiais.

**Inferência histórica:** 13 modelos marginais, 52 cadeias Stan históricas, nenhum MCMC novo. CVs publicados alimentam aproximação normal para estimativas agregadas. Os cenários E08/E09 dependem de `ρ` temporal escolhido hipoteticamente; não há covariâncias para identificar posterior conjunta de melhora nos cinco eixos. No histórico, 0 divergências e E-BFMI mínimo de 0,96646; R-hat máximo 1,01051 no nível de ocupação (ligeiramente acima de 1,01). Diagnóstico computacional não substitui adequação ao desenho amostral.

**Reprodução:** os 19 CSVs do pacote E10 local foram reprocessados e comparados por SHA-256 no estudo de origem, sem nova amostragem. Esta contribuição GitHub é **mais limitada operacionalmente**: contém os 15 fontes de código históricos com hashes Git idênticos aos originais, uma projeção descritiva de 26 contrastes e verificador independente; os arquivos de dados/saídas necessários à reprodução E04–E09 estão fora do GitHub. Não se reproduziu aqui API, E04–E09 ou MCMC. Não usar essa auditoria como validação científica integral.

**Fontes:** IBGE [SIDRA](https://sidra.ibge.gov.br/), [API](https://servicodados.ibge.gov.br/api/v3/agregados) e [Quadro Sintético julho–setembro/2026](https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Mensal/Quadro_Sintetico/2026/pnadc_202609_quadroSintetico.pdf). Não se atribui causalidade.
