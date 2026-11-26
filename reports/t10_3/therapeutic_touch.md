# therapeutic_touch — adendo científico T10.3

**Natureza:** consolidação editorial no corte 26/11/2026; Laboratório metodológico. **Base:** `06ef29c7e233396809d13804989efa19f7d49538`. Não é pré-registro contemporâneo, novo resultado, validação científica ou autorização de publicação. [README histórico](../../scripts/therapeutic_touch/README.md) preservado, blob `d41ed21eb282918f9b4d12eb65f3d7ea703de647`.

## 1. Pergunta e objeto

Como diferem ajustes pooled, unpooled e hierárquico no exemplo Therapeutic Touch?

**Objeto documentado:** Exemplo didático do dataset clássico Therapeutic Touch; comparação pooled/unpooled/hierarchical.

A pergunta é recuperada do catálogo T08.2, não declarada E00 original ratificada.

## 2. Dados, proveniência e licença

CSV demonstrativo TherapeuticTouchData; origem da compilação e licença não identificadas. therapeutic_touch (redistribution_status=review_required, não autorização de acesso)

**Registro de direitos no catálogo (corte histórico, não licença nova):** therapeutic_touch: license_status=not_identified, redistribution_status=review_required.

**Situação posterior:** A decisão de 26/11/2026 abrange `therapeutic_touch`: preservar cópias históricas com rastreabilidade parcial, apoiada na declaração do responsável. Não certifica vintage, licença específica, semântica ou autorização de release. Ver [inventário](../../data/DATA_PROVENANCE.md) e [decisão #14](../../data/DATA_PROVENANCE_DECISIONS.md). Não houve leitura de registros individuais nem reextração.

## 3. Estimandos e formulação matemática

Probabilidade de acerto no experimento, população/indivíduo e heterogeneidade; comparação pooled e hierárquica, restrita ao desenho experimental.

Pooled y~Bernoulli_logit(α), α~Normal(0; 1,5); hierárquico y_i~Bernoulli_logit(α+a_sujeito), efeitos não centrados z~Normal(0; 1), σ~Exponential(1). A presença de outras variantes no README não comprova execução.

Notação condensada editorial da especificação inspecionada; ponto e vírgula separa parâmetros, vírgula representa decimal. Normal usa média e desvio-padrão; Lognormal usa localização e escala no log; Exponential usa taxa, salvo parametrização explícita da fonte. Hiperparâmetros configuráveis e variantes não listadas permanecem nos fontes. Não equivale a verificação exaustiva E06→E07.

## 4. Implementação e rastreabilidade

Modelos Bayesianos Stan/cmdstanr pooled, unpooled ou hierárquicos conforme script; PPC e LOO como diagnósticos. Código identificado: therapeutic_touch.R.

Fontes específicas no SHA base (linhas indicam trechos de implementação, não logs de execução):

- [`scripts/therapeutic_touch/therapeutic_touch.R`](../../scripts/therapeutic_touch/therapeutic_touch.R) — linhas 258–271, 323–344; blob `1711b8cae063d4a0f07b6de71225cc62e709f4ba`.


## 5. Diagnósticos e resultados

Resumos, PPC e LOO previstos; nenhum resultado de efeito aferido nesta tarefa.

**Natureza da saída descrita no catálogo:** Console com resumos/diagnósticos e gráficos; PDF de plots e TXT de relatório por padrão via Rscript, seção Como rodar; --no-save permite console-only.

O registro de uma saída planejada ou descrita não constitui resultado observado.

## 6. Limites e reprodutibilidade

Licença do CSV reconstruído não identificada; resultado didático não comprova efeito clínico.

N1 documental; execução, pipeline e inferência históricos não reatestados nesta tarefa. Rotas descritas não equivalem a execução. Cinco eixos: [matriz T10.2](../t10_2/dimensoes_reprodutibilidade.csv); estados E00–E10: [matriz T10.2](../t10_2/estados_e00_e10.csv), todos PENDENTE quanto à homologação formal, não inexistência de trabalho. Não há promoção N3/N4 ou validação científica pela presente leitura.

## 7. Decisões abertas e diligências

Confirmar tabela de origem, codificação de acertos, número de tentativas e estrutura por sujeito; revisar unidade de LOO e interpretação restrita ao experimento.

Responsável científico e parecer por etapa não presumidos; a revisão transversal T10.6 deve avaliar os achados e priorizar diligências. O [catálogo T08.2](../../scripts/STUDY_CATALOG_T08_2.md) preserva o recorte de origem. Nenhum código, dado, resultado histórico ou README protegido foi alterado.
