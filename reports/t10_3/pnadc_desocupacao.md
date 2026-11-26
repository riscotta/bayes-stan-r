# pnadc_desocupacao — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Aplicado com método/rota descritos; síntese final não localizada na triagem. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/pnadc_desocupacao/README.md) preservado, blob `869d564737a51d9c97eb32fa5eb5999e6f23e1c4`.

## 1. Pergunta e objeto

Como evolui a desocupação segundo séries agregadas PNAD por período e sexo?

**Objeto documentado:** Taxa oficial de desocupação, série mensal total e trimestral por sexo, checagem janeiro de 2026.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

CSVs exportados PNAD Contínua tabelas 6381 e 4093 e extrato opcional de jan/2026; vintage não homologada. pnadc_desocupacao (redistribution_status=review_required, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** pnadc_desocupacao: license_status=public_source_no_explicit_dataset_license, redistribution_status=review_required.

**Situação posterior:** A decisão de 26/11/2026 abrange `pnadc_desocupacao`: preservar cópias históricas com rastreabilidade parcial, apoiada na declaração do responsável. Não certifica vintage, licença específica, semântica ou autorização de release. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Trajetória latente da taxa total e gaps por sexo na escala logito; contrastes na escala de taxa precisam transformação explícita.

y_total_logit ~ Normal(η_total; σ_total); masculino/feminino trimestral ~ Normal(η_total[tq]+gap_sexo; σ_sexo). level0 ~ Normal(logit(0,08); 0,60); inovações e sazonalidade normais, escalas Exponential; erros observacionais Exponential(30). Gaps por sexo não são diferenças causais.

Notação condensada editorial da especificação inspecionada; ponto e vírgula separa parâmetros, vírgula representa decimal. Normal usa média e desvio-padrão; Lognormal usa localização e escala no log; Exponential usa taxa, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Coerência das séries, modelo temporal em escala logito via rstan; modo debug e final. Código identificado: pnadc_desocupacao_rstan.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/pnadc_desocupacao/pnadc_desocupacao_rstan.R`](../../scripts/pnadc_desocupacao/pnadc_desocupacao_rstan.R) — linhas 471–492, 621; blob `ef1d91dcbec2ff45db8508787637ac065ea26259`.


## 5. Diagnósticos e resultados

Coerência das séries, MCMC e RMSE previstos; síntese final versionada não localizada. Unidade diferente de PNAD Estudo I.

**Natureza da saída descrita no catálogo:** Diagnóstico MCMC e relatório final no console; README abertura e Observações; sem arquivo analítico padrão.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

Interpretação depende do desenho e vintages do IBGE; modelo não reestimado neste catálogo.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Fixar exportações 6381/4093, alinhar trimestres móveis e sexo, avaliar dependência/desenho amostral e erro de observação; não tratar séries sobrepostas como independentes sem revisão.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
