# sobrevivencia_cadastral_poa — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Histórico com síntese rastreável (T08.4). **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/sobrevivencia_cadastral_poa/README.md) preservado, blob `b9346f159ac123dadf46ffa954d4ff4ecd9e61f0`.

## 1. Pergunta e objeto

Objetivo do README do estudo: qual a sobrevivência cadastral nos primeiros cinco anos dos estabelecimentos de alimentação fora do lar em Porto Alegre e como varia o risco de baixa entre coortes, subclasses, natureza jurídica e matriz/filial?

**Objeto documentado:** Estabelecimentos de alimentação fora do lar em Porto Alegre; primeiros cinco anos, risco de baixa por coorte, subclasse, natureza jurídica e matriz/filial.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

Insumos agregados de sobrevivência e resultados congelados; base individual CNPJ/endereço não publicada; fonte/vintage exata pendente. sobrevivencia_cadastral_poa (redistribution_status=aggregates_only, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** sobrevivencia_cadastral_poa: license_status=source_terms_unverified, redistribution_status=aggregates_only.

**Situação posterior:** Sem decisão específica de permanência identificada entre os 14 conjuntos da issue #14 para esta unidade; permanecem os limites do inventário e da fonte. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Sobrevivência cadastral até 60 meses e hazard de baixa discreto por coorte, subclasse, natureza jurídica e matriz/filial; desfecho administrativo.

M1: η_gm=α_m+lin_const_g+tv_coef_g·time_basis_m. Hazard h=1−exp(−exp(η)) (cloglog); s sobreviventes contribuem −s·exp(η), d eventos d·log(h). α tem RW2; α₁~Normal(−4,7; 1), diferença inicial Normal(0; 0,35), σ_RW2 positiva Normal(0; 0,15); β_cohort Normal(0; 0,35), β Normal(0; 0,50), γ_tv Normal(0; 0,25). M0 mantém efeitos constantes.

Notação condensada editorial da especificação inspecionada; ponto e vírgula separa parâmetros, vírgula representa decimal. Normal usa média e desvio-padrão; Lognormal usa localização e escala no log; Exponential usa taxa, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Sobrevivência em tempo discreto, M0 e M1 RW2 centrado QR, M1 com efeitos variáveis no tempo. Código identificado: sobrevivencia_cadastral_poa_cmdstanr.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/sobrevivencia_cadastral_poa/modelo_M1_rw2_centrado_qr.stan`](../../scripts/sobrevivencia_cadastral_poa/modelo_M1_rw2_centrado_qr.stan) — linhas 39–63; blob `a32fb95ac4a70a5806f0ddea8f18a899fc298c60`.
- [`scripts/sobrevivencia_cadastral_poa/README.md`](../../scripts/sobrevivencia_cadastral_poa/README.md) — linhas 18, 36, 44–54; blob `b9346f159ac123dadf46ffa954d4ff4ecd9e61f0`.


## 5. Diagnósticos e resultados

README: 19.134 estabelecimentos, 7.110 baixas observadas; M1 ΔELPD-LOO=284,9 (EP 23,6), zero divergências/treedepth, R-hat máximo 1,0099 e E-BFMI mínimo 0,409, todos históricos. Sobrevivência observada a 60 meses 57,9% conforme T08.4. Base individual externa, sem nova amostragem.

**Natureza da saída descrita no catálogo:** Tabelas auditadas, duas figuras centrais, curvas de sobrevivência e comparação interna M0/M1, diagnóstico; seção Resultados principais; sem republicar cifras, rankings ou inferência protegida.

A interpretação histórica também é delimitada nos [sumários T08.4](../../scripts/STUDY_SUMMARIES_T08_4.md) e [evidências T08.6](../../scripts/STUDY_EVIDENCE_T08_6.md).

## 6. Limites e reprodutibilidade

CNPJ/endereço individuais fora do Git; baixa cadastral não equivale a fechamento real; sem direitos totais homologados; sem validação de coortes futuras, causalidade ou dependência entre unidades do mesmo CNPJ básico.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Conferir coortes e censura, agrupamento por CNPJ básico e validade temporal externa; não identificar encerramento real nem efeito causal. Não abrir dados individualizados.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
