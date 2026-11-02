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

## Código e resultados aqui disponíveis

- `auditar_e10.py` e [síntese histórica E09](../../data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv): auditoria de SHA-256, cardinalidade, períodos e identidades dos **26 contrastes** (projeção descritiva de 10 colunas da E09 original).
- `estudo_I/e07/stan/`: **três modelos Stan originais** usados na E07 histórica, não reamostrados neste trabalho de GitHub.
- `estudo_I/e07/R/`: código histórico R/CmdStanR (reprocessamento de resumos disponível; MCMC somente por comando explícito).
- `estudo_I/e08/diagnostico_e081.py`: diagnóstico histórico, requer cadeias externas.
- `estudo_I/e09/consolidar_e09.py`: código **histórico** de consolidação E09; não roda isolado, exige as saídas E08.
- [Nota científica](../../reports/pnadc_estudo_i/nota_cientifica.md) com limitações inferenciais.

**Escopo desta contribuição é deliberadamente parcial:** os demais fontes E04, E05 e E08.2–E08.4, os metadados SIDRA, o snapshot original de 22.130 linhas, o arquivo E09 completo e as 52 cadeias históricas constam apenas do **pacote local E10** entregue ao autor, e ainda não foram incluídos nesta branch. A obtenção original via API SIDRA não foi recuperada. Não afirmar reprodução integral E04–E09 a partir deste repositório.

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

**Estados:** síntese descritiva conferível neste repositório; inferências MCMC históricas e cenários condicionais; reamostragem/API não testadas aqui.
