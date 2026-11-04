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

## Rastreabilidade dos estimandos E07 (T03.1)

O [inventário documental da E07](../../reports/pnadc_estudo_i/inventario_estimandos_e07_T03_1.md) relaciona os 13 estimandos `theta` **marginais de 202609**, os arquivos de saída previstos no R/Stan e as diferenças **descritivas** e **cenários condicionais** produzidos somente em E05/E08/E09. Os resumos MCMC E07 históricos não estão versionados neste repositório; a T03.1 não os reexecuta nem valida uma posterior temporal conjunta. A revisão independente e a validação científica de T03.2–T03.7 continuam pendentes.

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
