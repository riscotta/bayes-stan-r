# censo_escolar_tempo_integral — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Aplicado com método/rota descritos; síntese final não localizada na triagem. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/censo_escolar_tempo_integral/README.md) preservado, blob `459f4a074ac87283110f8bebd3f4ab72b129168d`.

## 1. Pergunta e objeto

Como variam as matrículas presenciais em tempo integral na rede pública nos recortes oficiais?

**Objeto documentado:** Matrículas presenciais em tempo integral na rede pública brasileira, 2021–2025, por ano, UF, dependência administrativa e etapa.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

Microdados oficiais INEP, ZIPs por ano, contagens por escola/etapa e dicionários oficiais; cache não versionado. censo_escolar (redistribution_status=not_versioned, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** censo_escolar: license_status=public_source_no_explicit_dataset_license, redistribution_status=not_versioned.

**Situação posterior:** Sem decisão específica de permanência identificada entre os 14 conjuntos da issue #14 para esta unidade; permanecem os limites do inventário e da fonte. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Proporção de matrículas em tempo integral por ano e estrato (UF, dependência administrativa e etapa), com síntese nacional observada e posterior. O denominador é o total de matrículas elegíveis, não número de escolas.

y_i | n_i,η_i ~ Binomial(n_i,logit⁻¹η_i). O preditor inclui intercepto e efeitos de UF, dependência, etapa e ano. α ~ Normal(alpha_prior_mean,0,75); efeitos ~ Normal(0,σ correspondente), escalas positivas com prior Normal(0,0,25) truncada. Valida 0 ≤ y ≤ n e n > 0.

Notação condensada editorial da especificação inspecionada; Normal usa média e desvio-padrão, Lognormal usa localização e escala no log, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Validação de colunas, agregação de contagens e modelo binomial multinível Stan/cmdstanr. Código identificado: censo_escolar_tempo_integral_cmdstanr.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/censo_escolar_tempo_integral/censo_escolar_tempo_integral_cmdstanr.R`](../../scripts/censo_escolar_tempo_integral/censo_escolar_tempo_integral_cmdstanr.R) — linhas 812–821, 902–918; blob `8f41a374322c40619a00f8af902e812b4e5fbed9`.


## 5. Diagnósticos e resultados

O README prevê relatório nacional observado e posterior por ano. Nenhuma síntese final de execução localizada na triagem T08.4; não há estimativa atual produzida.

**Natureza da saída descrita no catálogo:** Relatório no console com estatística observada nacional e resumo posterior por ano; TXT e CSV opcionais, README seção Arquivos; nenhuma saída final gravada por padrão.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

Downloads/vintages e autorização de uso do INEP precisam ser conferidos para nova reprodução.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Fixar vintages INEP e dicionários anuais; validar comparabilidade dos denominadores, agregação e pesos nacionais; conferir diagnósticos e saída posterior antes de divulgar tendência.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
