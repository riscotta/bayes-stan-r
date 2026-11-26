# isus_sia — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Aplicado com método/rota descritos; síntese final não localizada na triagem. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/isus_sia/README.md) preservado, blob `6b78e33f36c1474cbe41446aa887dd7c6ed96724`.

## 1. Pergunta e objeto

Como simular custos de procedimentos raros para precificação em registros ISUS/SIA?

**Objeto documentado:** Precificação de procedimentos, incluindo caros e raros, com agrupamento por CNES; universo/período não homologados.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

Extrato institucional ISUS/SIA em ZIP/CSV separado por ponto e vírgula; origem e autorização pendentes. isus_sia (redistribution_status=review_required, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** isus_sia: license_status=not_identified, redistribution_status=review_required.

**Situação posterior:** A decisão de 26/11/2026 abrange `isus_sia`: preservar cópias históricas com rastreabilidade parcial, apoiada na declaração do responsável. Não certifica vintage, licença específica, semântica ou autorização de release. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Preços unitários de referência e dispersão bootstrap por grupo, além de erro absoluto por unidade e frações de sobre/subpagamento out-of-bag. Não são estimandos de posterior Stan.

Bootstrap de clusters CNES: reamostrar n clusters com reposição; multiplicidades m aplicadas a quantidades q e valores v; média ponderada p=Σm_i v_i/Σm_i q_i. Compara mediana ponderada, média aparada, winsorizada, geométrica e Huber; avalia OOB. Não há likelihood/prior nesse método.

Notação condensada editorial da especificação inspecionada; Normal usa média e desvio-padrão, Lognormal usa localização e escala no log, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Monte Carlo robusto com cluster bootstrap por CNES e caudas pesadas; pacote data.table. Código identificado: mc_isus_sia.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/isus_sia/mc_isus_sia.R`](../../scripts/isus_sia/mc_isus_sia.R) — linhas 528–577, 593–608; blob `5dea3629202c020718ac3231cb37de8461348bad`.


## 5. Diagnósticos e resultados

Relatório console-only; saídas de simulação, quantis e OOB descritas, sem síntese final localizada. Nenhum dado individual ou valor de precificação foi acessado.

**Natureza da saída descrita no catálogo:** Console-only de simulação para apoio à precificação; README identifica a saída e finalidade, sem especificar métricas individuais; não transcritas por restrição institucional.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

Extrato institucional ZIP com origem, permissões e classificação LGPD pendentes; não ampliar acesso.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Fixar consulta e transformação DATASUS/SIA, classificação semântica e permissões para novo uso; avaliar representatividade dos CNES e incerteza de caudas, além de adequação de preço como decisão.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
