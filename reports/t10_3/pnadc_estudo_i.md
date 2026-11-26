# pnadc_estudo_i — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Histórico com síntese rastreável (T08.4). **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/pnadc_estudo_i/README.md) preservado, blob `f1fd01d06f2ec03092ab11cedac3a4e7e946dc8f`.

## 1. Pergunta e objeto

Objetivo original documentado no README do estudo: investigar se a queda do desemprego brasileiro veio acompanhada de melhora em todas as dimensões do mercado de trabalho (formulação histórica, não uma atualização da taxa).

**Objeto documentado:** Brasil, 13 indicadores em cinco eixos (desocupação, ocupação/participação, subutilização/desalento, informalidade/previdência e rendimento); contrastes 202509→202609 e 202606→202609.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

Agregados PNAD Contínua/IBGE com CV, população 14+ como apoio/denominador; síntese E09 versionada; snapshot e entradas históricas E04–E08 incompletos no clone. pnadc_estudo_i (redistribution_status=aggregates_only, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** pnadc_estudo_i: license_status=public_source_no_explicit_dataset_license, redistribution_status=aggregates_only.

**Situação posterior:** Sem decisão específica de permanência identificada entre os 14 conjuntos da issue #14 para esta unidade; permanecem os limites do inventário e da fonte. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

13 θ marginais de setembro de 2026; 26 contrastes descritivos em duas janelas. Cenários probabilísticos condicionais a ρ, não posterior conjunta dos cinco eixos.

y|θ,s ~ Normal(θ,s), s=|y|CV/100. Taxas θ=100p, p~Beta(1,1); totais θ=P14q, q~Beta(1,1) com P14 fixa; rendimento θ=μ>0, μ~Normal(0,10000) truncada. E08/E09 utiliza cenários gaussianos para ρ∈{−0,5;0;0,5;0,9}; ρ=0 não é independência demonstrada.

Notação condensada editorial da especificação inspecionada; Normal usa média e desvio-padrão, Lognormal usa localização e escala no log, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Aproximação Normal com erro padrão derivado do CV, modelos marginais para taxas/totais/rendimento; contrastes descritivos e cenários gaussianos condicionais a rho postulado, não posterior conjunta. Código identificado: auditar_e10.py e três modelos Stan marginais históricos E07, descritos na reconstrução E06.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/pnadc_estudo_i/README.md`](../../scripts/pnadc_estudo_i/README.md) — linhas 30–55, 61–92, 101–120; blob `f1fd01d06f2ec03092ab11cedac3a4e7e946dc8f`.

- [`reports/pnadc_estudo_i/e06_especificacao_modelos_T04_3.md`](../../reports/pnadc_estudo_i/e06_especificacao_modelos_T04_3.md) — blob `772b94db51d72d1c8412749c5339569f92a4233c`.
- [`reports/pnadc_estudo_i/inventario_estimandos_e07_T03_1.md`](../../reports/pnadc_estudo_i/inventario_estimandos_e07_T03_1.md) — blob `b5bf15dbede351c355164b721985b37ce02304c7`.
- [`reports/pnadc_estudo_i/ponte_e06_e07_T04_4.md`](../../reports/pnadc_estudo_i/ponte_e06_e07_T04_4.md) — blob `59865b476b987d29d7dd0f746c275d9f27db0726`.
- [`reports/pnadc_estudo_i/ponte_e08_e09_T04_5.md`](../../reports/pnadc_estudo_i/ponte_e08_e09_T04_5.md) — blob `9c412d11551246412fc83fa09d52e0d5ef6eeea9`.
- [`reports/pnadc_estudo_i/e10_matriz_evidencias_reproducao_T04_6.md`](../../reports/pnadc_estudo_i/e10_matriz_evidencias_reproducao_T04_6.md) — blob `80119b460c73e7b8dd232f3ad455a1507bec4b55`.
- [`reports/t07_6/README.md`](../../reports/t07_6/README.md) — blob `e2b2484e74f80fda73576cf12b6effbb373d4327`.

## 5. Diagnósticos e resultados

N2 público: 26 contrastes, identidade aritmética e períodos. E09 recente: desocupação 5,4→5,2%, aumento de desalento e subocupação por horas conforme T08.4. As 52 cadeias e E08 probabilística não acompanham clone; equivalência adaptador contemporânea, não cotejo histórico.

**Natureza da saída descrita no catálogo:** Síntese E09 de contrastes descritivos e cenários condicionais; README seções Código e resultados aqui disponíveis e Rastreabilidade dos estimandos E07; resumos MCMC E07 históricos não versionados; sem republicar valores e sem probabilidade conjunta identificada.

A interpretação histórica também é delimitada nos [sumários T08.4](../../scripts/STUDY_SUMMARIES_T08_4.md) e [evidências T08.6](../../scripts/STUDY_EVIDENCE_T08_6.md).

## 6. Limites e reprodutibilidade

N2 público limitado; covariâncias não observadas impedem atribuir probabilidade conjunta identificada; E04–E08 ausentes.

N2 de agregados públicos, sem refazimento de fontes/modelos; confirmação dos passos N2 na CI da base T10.2, não nova execução desta T10.3. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Identificar covariâncias de desenho e snapshots/entradas, não publicar posterior conjunta sem identificação, obter insumos/cadeias autorizados e resolver decisões D00 e ressalvas registradas sem retroagir o histórico.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
