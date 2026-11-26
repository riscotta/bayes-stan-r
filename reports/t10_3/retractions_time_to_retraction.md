# retractions_time_to_retraction — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Aplicado com método/rota descritos; síntese final não localizada na triagem. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/retractions_time_to_retraction/README.md) preservado, blob `c878e2cb31c73f83caf179c1727b2455d0372555`.

## 1. Pergunta e objeto

Como varia o intervalo entre publicação e retratação de artigos?

**Objeto documentado:** Tempo entre publicação original e retratação de artigos da série global 1927–2026; corte de publicação original configurável (2021).

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

CSV externo Kaggle Global Scientific Retractions, validação de datas/lag e agrupamento de categorias raras. retractions_time_to_retraction (redistribution_status=not_versioned, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** retractions_time_to_retraction: license_status=page_specific_unverified, redistribution_status=not_versioned.

**Situação posterior:** Sem decisão específica de permanência identificada entre os 14 conjuntos da issue #14 para esta unidade; permanecem os limites do inventário e da fonte. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Distribuição e contrastes do atraso entre publicação e retratação entre artigos retratados observados, condicionados a editora e covariáveis; não tempo de todos os artigos até eventual retratação.

y=(log_lag_days−média)/DP, y~Normal(μ,σ), μ=α+Xβ e efeitos de editora e indicadores multi-rótulo. α ~ Normal(0,0,35), β~Normal(0,0,20), coeficientes multi-rótulo Normal(0,0,15); σ~Lognormal(log(0,6),0,25), σ_pub~Lognormal(log(0,10),0,35).

Notação condensada editorial da especificação inspecionada; Normal usa média e desvio-padrão, Lognormal usa localização e escala no log, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Modelo Bayesiano com efeitos fixos e aleatório por editora, cmdstanr. Código identificado: retractions_time_to_retraction_cmdstanr.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/retractions_time_to_retraction/retractions_time_to_retraction_cmdstanr.R`](../../scripts/retractions_time_to_retraction/retractions_time_to_retraction_cmdstanr.R) — linhas 320–370, 447–465; blob `d9460bf5f9bd30639f310dbe570c3561305c5ae9`.


## 5. Diagnósticos e resultados

Diagnósticos e sumários previstos, síntese final não localizada. Seleção em retratações observadas e datas truncadas limita leitura; não equiparar à análise de sobrevivência com população completa.

**Natureza da saída descrita no catálogo:** Diagnósticos, sumários posteriores, tabelas CSV, relatório TXT, gráficos PNG e RDS opcional; seção Arquivos; valores não verificados.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

Fonte Kaggle não versionada; datas observadas sujeitas a seleção/censura; reexecução não atestada.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Conferir definição de log_lag_days, vintage e filtros de datas, censura/truncamento/seleção e transformação de volta para dias; revisar agrupamento de categorias raras.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
