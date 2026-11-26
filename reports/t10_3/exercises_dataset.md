# exercises_dataset — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Aplicado com método/rota descritos; síntese final não localizada na triagem. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/exercises_dataset/README.md) preservado, blob `5e9ce43dd72f357e814f5116fb1b297adc37e319`.

## 1. Pergunta e objeto

Como dificuldade e cobertura de exercícios se relacionam em uma base catalogada?

**Objeto documentado:** Dificuldade ordinal (beginner/intermediate/advanced) de exercícios do catálogo, por grupo corporal, categoria, passos e músculos secundários.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

CSV final_exercise_dataset do catálogo Kaggle; base analítica derivada e JSON Stan; equipamento em modelo diagnóstico. exercises_dataset (redistribution_status=review_required, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** exercises_dataset: license_status=page_specific_unverified, redistribution_status=review_required.

**Situação posterior:** A decisão de 26/11/2026 abrange `exercises_dataset`: preservar cópias históricas com rastreabilidade parcial, apoiada na declaração do responsável. Não certifica vintage, licença específica, semântica ou autorização de release. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Probabilidades condicionais de dificuldade beginner/intermediate/advanced por características do catálogo, sem efeito causal do equipamento.

Modelo principal sem equipamento: y_i ~ OrderedLogistic(η_i,c), η combina passos padronizados, músculos secundários e efeitos hierárquicos de grupo corporal/categoria. Cutpoints ~ Normal(0,2) com ordenação; coeficientes Normal(0,1), escalas de grupo Normal(0,0,7) truncadas. Modelo de equipamento é diagnóstico; softmax é alternativa de sensibilidade, não fusão dos modelos.

Notação condensada editorial da especificação inspecionada; Normal usa média e desvio-padrão, Lognormal usa localização e escala no log, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Modelos ordinais com/sem equipamento e softmax de sensibilidade, especificações Stan nomeadas na seção Arquivos. Código identificado: exercises_dataset_cmdstanr.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/exercises_dataset/modelo_ordinal_dificuldade_sem_equipamento.stan`](../../scripts/exercises_dataset/modelo_ordinal_dificuldade_sem_equipamento.stan) — linhas 30–55; blob `b246bfc055c1d1d996694e594d45353319b7aba9`.
- [`scripts/exercises_dataset/README.md`](../../scripts/exercises_dataset/README.md) — linhas 8–23, 62; blob `5e9ce43dd72f357e814f5116fb1b297adc37e319`.


## 5. Diagnósticos e resultados

Rota padrão prepara a base/JSON e tabelas, sem MCMC. Síntese final versionada não localizada na T08.4. Não atesta calibração ordinal.

**Natureza da saída descrita no catálogo:** Tabelas descritivas, logs, JSON de entrada Stan e CSVs de amostragem somente quando habilitada; seção Saídas geradas; equipamento tratado como composição, não causalidade.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

Identidade/licença da fonte Kaggle e seleção de modelos exigem revisão; resultados não reestimados.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Auditar rótulos, escala ordinal, codificação e cobertura do catálogo; confrontar modelos sem/com equipamento e softmax, diagnósticos e validação fora de amostra.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
