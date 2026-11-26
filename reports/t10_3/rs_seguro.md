# rs_seguro — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Aplicado com método/rota descritos; síntese final não localizada na triagem. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/rs_seguro/README.md) preservado, blob `2599c242b6d2e7243a5996608e3a9e34beafd0f6`.

## 1. Pergunta e objeto

Como evoluem séries de macrocriminalidade e perfis de vítimas em RS Seguro?

**Objeto documentado:** Série mensal por crime/macrocrime no RS e perfil de vítima condicionado a macrocrimes-alvo; período exato não explicitado.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

CSVs mensais de macrocrime, perfil de uma vítima e camada crime opcional não versionada; origem e permissões institucionais pendentes. rs_seguro_macrocrime (redistribution_status=review_required, não autorização de acesso); rs_seguro_macrocrime_profile (redistribution_status=review_required, não autorização de acesso); rs_seguro_crime_optional (redistribution_status=not_versioned, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** rs_seguro_macrocrime: license_status=not_identified, redistribution_status=review_required; rs_seguro_macrocrime_profile: license_status=not_identified, redistribution_status=review_required; rs_seguro_crime_optional: license_status=not_identified, redistribution_status=not_versioned.

**Situação posterior:** A decisão de 26/11/2026 abrange `rs_seguro_macrocrime`, `rs_seguro_macrocrime_profile`: preservar cópias históricas com rastreabilidade parcial, apoiada na declaração do responsável. Não certifica vintage, licença específica, semântica ou autorização de release. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

M1: tendência de contagens mensais por crime/macrocrime. M2: tendência de contagens de ocorrências com uma vítima por sexo, faixa e crime condicionada a alvos; não risco individual.

M1 y~NB2_log(η; φ_crime), α_global~Normal(log_y_mean; 0,5), β_global~Normal(0; 0,05); efeitos hierárquicos por crime e mês. M2 y~NB2_log(η; φ_group), α_global~Normal(log_y_mean; 0,6), β_global~Normal(0; 0,08), efeitos por grupo e mês; dispersões hierárquicas. A seleção de alvos M2 é explícita ou via saída M1, não uma amostragem independente.

Notação condensada editorial da especificação inspecionada; ponto e vírgula separa parâmetros, vírgula representa decimal. Normal usa média e desvio-padrão; Lognormal usa localização e escala no log; Exponential usa taxa, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

M1 série mensal em duas camadas; M2 perfil condicionado a alvos explícitos ou selecionados pelo M1; cmdstanr. Código identificado: rs_seguro_m1_dual_layer_cmdstanr.R e rs_seguro_m2_macrocrime_conditional_cmdstanr.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/rs_seguro/rs_seguro_m1_dual_layer_cmdstanr.R`](../../scripts/rs_seguro/rs_seguro_m1_dual_layer_cmdstanr.R) — linhas 215–243; blob `e08af681fe20dad4127e7c828127befec3168927`.
- [`scripts/rs_seguro/rs_seguro_m2_macrocrime_conditional_cmdstanr.R`](../../scripts/rs_seguro/rs_seguro_m2_macrocrime_conditional_cmdstanr.R) — linhas 222–250; blob `f52572d36e604e01b3163b406df30a72e0f6c88c`.
- [`scripts/rs_seguro/README.md`](../../scripts/rs_seguro/README.md) — linhas 5–57; blob `2599c242b6d2e7243a5996608e3a9e34beafd0f6`.


## 5. Diagnósticos e resultados

Saídas de tendência e perfis previstas, síntese final não localizada; alerta T09.1 de cabeçalhos e classificação semântica requer revisão. Não publicar dados de perfil adicionais.

**Natureza da saída descrita no catálogo:** CSV/PDF de tendência M1 por macrocrime e CSV/PDF de perfil condicional M2; README seção Arquivos; sem divulgar perfis ou estimativas protegidas.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

Arquivos operacionais sem origem/redistribuição institucional homologadas; exposição de perfil requer cautela.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Conferir extração/agrupamento, unidade de contagem, cobertura e eventuais mudanças de classificação; avaliar seleção de alvos, exposição e risco de identificação antes de novo uso/publicação.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
