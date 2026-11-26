# mortalidade_sp_0_74 — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Histórico com síntese rastreável (T08.4). **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/mortalidade_sp_0_74/README.md) preservado, blob `7e2d093b17edd068940ee6069439b96b567eb31d`.

## 1. Pergunta e objeto

Como evoluem indicadores de mortalidade de 0–74 anos nos distritos de São Paulo?

**Objeto documentado:** Óbitos de 0–74 anos em 96 distritos de São Paulo, 2006–2023, por sexo e faixas quinquenais.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

Integração provisória APVP/PRO-AIM (óbitos) e SEADE (população); agregados congelados; E04 celular externa, equivalência territorial não homologada. mortalidade_sp_0_74 (redistribution_status=aggregates_only, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** mortalidade_sp_0_74: license_status=source_terms_unverified, redistribution_status=aggregates_only.

**Situação posterior:** Sem decisão específica de permanência identificada entre os 14 conjuntos da issue #14 para esta unidade; permanecem os limites do inventário e da fonte. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Taxa bruta e padronizada por 100 mil, decomposição simétrica da mudança em taxas específicas/composição; desvio preditivo temporal. Recorte 0–74, 96 distritos, 2006–2023.

Taxa padronizada = Σ_s w_s,2019 r_s,t com 30 estratos. Ajustes históricos em R, glm Poisson com link log e MASS::glm.nb; NB2 usa parâmetro theta para variância μ+μ²/theta. Não há modelo Stan. Decomposição é identidade descritiva, não identificação causal.

Notação condensada editorial da especificação inspecionada; Normal usa média e desvio-padrão, Lognormal usa localização e escala no log, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Padronização por idade/sexo com pesos internos de 2019; decomposição simétrica, sensibilidade e validação NB2 histórica. Código identificado: mortalidade_sp_0_74.R e E09_analise_completa.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/mortalidade_sp_0_74/01_E07_ajustar_modelos.R`](../../scripts/mortalidade_sp_0_74/01_E07_ajustar_modelos.R) — linhas 41–53, 68; blob `059636b4be7ced79c7f45ace938c016175307e49`.
- [`scripts/mortalidade_sp_0_74/README.md`](../../scripts/mortalidade_sp_0_74/README.md) — linhas 3–12, 41–60; blob `7e2d093b17edd068940ee6069439b96b567eb31d`.

- [`reports/mortalidade_sp_0_74/caderno_E01_E10.md`](../../reports/mortalidade_sp_0_74/caderno_E01_E10.md) — blob `b82c6bd0b7942bf75eaae1491f62cb3393633c0d`.
- [`reports/mortalidade_sp_0_74/relatorio_cientifico.md`](../../reports/mortalidade_sp_0_74/relatorio_cientifico.md) — blob `6768c4169703ef4f610a7a6238b24e3aae5695eb`.
- [`reports/mortalidade_sp_0_74/matriz_evidencias.md`](../../reports/mortalidade_sp_0_74/matriz_evidencias.md) — blob `1ce905f669adedbb09b7415f50efa9aef3e8b4ed`.
- [`reports/t07_7/README.md`](../../reports/t07_7/README.md) — blob `5c08cd99a6b1f5301f3a74feab07949ec91920d6`.

## 5. Diagnósticos e resultados

Taxa bruta 2019→2023: +2,8%; padronizada −5,0%, conforme T08.4. Auditoria N2 agrega E09; E07/E08 e integração celular não reexecutadas nesta T10.3. Desvio 2021 não mede mortes atribuíveis causalmente à pandemia.

**Natureza da saída descrita no catálogo:** Tabelas agregadas E09, resumos e figuras de taxas/decomposição e validação NB2; README seções Evidências e escopo e O que está neste repositório, localizador neutro do relatório científico; sem republicar cifras ou inferência; não estima mortes causalmente atribuíveis.

A interpretação histórica também é delimitada nos [sumários T08.4](../../scripts/STUDY_SUMMARIES_T08_4.md) e [evidências T08.6](../../scripts/STUDY_EVIDENCE_T08_6.md).

## 6. Limites e reprodutibilidade

Denominadores, concordância distrital histórica e E04 externos não homologados; sem interpretação causal.

N2 de agregados públicos, sem refazimento de fontes/modelos; confirmação dos passos N2 na CI da base T10.2, não nova execução desta T10.3. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Homologar correspondência territorial, denominadores, insumos E04 e paridade R/Python; revisar cobertura e sensibilidade de bootstrap. Adaptador suspenso; alerta semântico T09.1 não resolvido.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
