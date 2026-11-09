# Nota científica — PNAD Contínua, Estudo I

**Pergunta original:** “O desemprego brasileiro caiu para 5,2%. Mas o mercado de trabalho melhorou em todas as dimensões?”

**População e unidade:** Brasil, estatísticas agregadas oficiais, 13 indicadores A–E. Comparações: 202509→202609 e 202606→202609.

A taxa de desocupação publicada caiu para **5,2%** em setembro de 2026. Não se segue disso que todos os eixos melhoraram no recorte recente. Desalentados: 2.288→2.459 mil, taxa de desalento: 2,1→2,2%, subocupação por insuficiência de horas: 4,0→4,1%. A taxa composta de subutilização 12,9→13,0% e rendimento médio real 3.753→3.790 R$ foram **classificados como estáveis** pelo IBGE nessa comparação. Deltas calculados com números arredondados nem sempre coincidem com deltas oficiais.

**Inferência histórica:** 13 modelos marginais, 52 cadeias Stan históricas, nenhum MCMC novo. CVs publicados alimentam aproximação normal para estimativas agregadas. Os cenários E08/E09 dependem de `ρ` temporal escolhido hipoteticamente; não há covariâncias para identificar posterior conjunta de melhora nos cinco eixos. No histórico, 0 divergências e E-BFMI mínimo de 0,96646; R-hat máximo 1,01051 no nível de ocupação (ligeiramente acima de 1,01). Diagnóstico computacional não substitui adequação ao desenho amostral.

**Reprodução:** os 19 CSVs do pacote E10 local foram reprocessados e comparados por SHA-256 no estudo de origem, sem nova amostragem. Esta contribuição GitHub é **mais limitada operacionalmente**: contém os 15 fontes de código históricos com hashes Git idênticos aos originais, uma projeção descritiva de 26 contrastes e verificador independente; os arquivos de dados/saídas necessários à reprodução E04–E09 estão fora do GitHub. Não se reproduziu aqui API, E04–E09 ou MCMC. Não usar essa auditoria como validação científica integral.

**Fontes:** IBGE [SIDRA](https://sidra.ibge.gov.br/), [API](https://servicodados.ibge.gov.br/api/v3/agregados) e [Quadro Sintético julho–setembro/2026](https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Mensal/Quadro_Sintetico/2026/pnadc_202609_quadroSintetico.pdf). Não se atribui causalidade.

## Interpretação correta dos campos probabilísticos históricos (T03.6)

**Decisão editorial:** a comparação probabilística E08.2/E09 permanece **somente como cenário exploratório gaussiano condicional** a CVs agregados e a uma correlação temporal `rho` postulada. Nenhum dos números ou rótulos de E08/E09 deve ser lido como uma posterior temporal conjunta identificada pela PNAD ou como uma probabilidade oficial do IBGE. A escolha `rho=0` na E09 é hipótese de covariância temporal nula no par, **não** independência observada. A mesma margem de erros admite diferentes covariâncias e diferentes probabilidades condicionais.

Em E07, `theta` e `y_rep` descrevem exclusivamente os **níveis marginais de 202609**; o `ppc_cdf_obs` da E08.1 é uma frequência preditiva de `y_rep ≤ y`, não chance de melhora. Em E08.2, `ProbSentido` é a probabilidade de sinal favorável calculada por Normal condicionada a **indicador, janela, direção e `rho` hipotético**. Os campos `IC95Inf` e `IC95Sup` representam **extremos da faixa normal condicional nominal de 95% para a diferença**, calculados como `Delta ± 1,959963984540054*sDelta`; não constituem intervalo credível de posterior conjunta nem intervalo de confiança com cobertura do desenho demonstrada.

Os campos `ProbMinCenariosRho`/`ProbMaxCenariosRho` da E09 são **mínimo e máximo de quatro cenários de `rho`**, não quantis de uma posterior para a probabilidade ou para `rho`. E08.3 acrescenta sensibilidade a priors e arredondamento com quadratura e aproximação aos momentos; os 16 vértices hipotéticos não demonstram limites globais contínuos. Os limites de Fréchet em E08.4 usam **cinco indicadores proxy**, não todos os 13 indicadores ou os cinco eixos substantivos integralmente definidos. Portanto, não há sustentação para atribuir probabilidade pontual à melhora simultânea dos cinco eixos.

**Como citar um cenário:** declarar nome do indicador e unidades, período inicial e final, direção favorável, hipótese de `rho`, método Normal aproximado, origem dos CVs e limites de suporte/arredondamento. Separar **deltas de agregados arredondados**, **classificação oficial de estabilidade ou mudança do IBGE** e **probabilidades condicionais calculadas pelos scripts históricos**. Diagnósticos Stan de E07 não validam os contrastes E08/E09.

O [dicionário de rótulos e regras de apresentação T03.6](rotulos_inferenciais_T03_6.md) identifica os campos preservados (`ProbSentido`, `IC95Inf/Sup`, `ProbMin/MaxCenariosRho`, `ProbSentido_NormMomentos`, `ppc_cdf_obs`, limites de Fréchet) e os respectivos rótulos interpretativos. Este documento não altera os CSVs, os scripts de E07–E09 nem as conclusões históricas; fornece critérios para impedir leituras cientificamente indevidas. A T03.7 e a governança de dados T01.3–T01.7/Issue #14 permanecem pendentes.
