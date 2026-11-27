# T01.5 — Decisão por dataset e implicações no histórico Git (27/11/2026)

**Tarefa literal:** “Registrar decisão justificada por dataset: manter, restringir, substituir ou retirar; avaliar implicações no histórico Git.” Escopo: decisão documental no perímetro revisado aprovado pelo responsável, sem alteração material ou homologação de licença/privacidade. A decisão existente da issue #14 é preservada; P01/P02 permanecem P0 para os objetivos afetados. Nenhuma substituição, retirada ou reescrita de histórico é aprovada neste documento.

## 1. Base, autoridade e evidências

Base [main de3658d7601a13bfcc793f631059728edd90974c](https://github.com/riscotta/bayes-stan-r/commit/de3658d7601a13bfcc793f631059728edd90974c), árvore `66e3e25eaaeca7dfbe6188e49b99d3d090fecc0f`; [CI push #45879285885](https://github.com/riscotta/bayes-stan-r/actions/runs/45879285885) completed/success no SHA exato. PRs [#84](https://github.com/riscotta/bayes-stan-r/pull/84) e [#85](https://github.com/riscotta/bayes-stan-r/pull/85) merged; Codex completed, sem achados nos HEADs exatos. Ruleset 26061035 ativo, main protegida, PR e check R obrigatório; zero aprovações humanas obrigatórias não dispensam QA do plano. #14 fechada; #15/#16 abertas; #64 único PR aberto antes deste ciclo, draft; Deck `24f6a6f4c827de6437799b2eb841afb4ae102261`, release v1.0.0 sem assets.

Fontes na base: [decisão #14](DATA_PROVENANCE_DECISIONS.md), blob `41740623c8da8e6c4c0e1bab72af84e7bb5d4ec8`; [inventário](DATA_PROVENANCE.csv), blob `97cfe122f903b8c1b52c57ed0a6c287695f33ba3`; [manifesto](TRACKED_DATA_MANIFEST.csv), blob `ed99382fbe6764a00567d7ec534268398a580145`; [T01.3](DATA_PRIVACY_T01_3_RECONCILIATION_2026-11-26.md), [T01.4](DATA_AUTHORIZATION_T01_4_RECONCILIATION_2026-11-26.md), [T09.1](../reports/t09/README.md), [T10.7](../reports/t10_7/README.md), [issue #14](https://github.com/riscotta/bayes-stan-r/issues/14) e seus comentários. Word mestre 25/11, adendo Etapa 10 26/11 e adendo de continuidade 27/11 preservados. Responsável aprovou concluir o plano revisado com ressalvas e transferir evolução científica para manutenção contínua; não autorizou inspeção neste ambiente, alteração de dados, ampliação de divulgação ou release.

A declaração de 26/11 (fontes públicas, preparação própria, ausência de restrições segundo relato) fundamenta **permanência histórica**, não comprovação independente da licença específica ou autorização institucional. Declaração de 03/11 para ISUS/SIA e RS Seguro não valida G1–G5 nem abrange automaticamente perfil, mortality ou IOM. T01.3/T01.4 originais permanecem sem homologação; não reexecutadas.

## 2. Matriz de decisão (25 IDs)

Decisões aplicáveis:
- **H** = manter cópia histórica já existente conforme #14 (review_required); restringir **nova divulgação** que dependa de direitos/privacidade comprovados. Não equivale a tornar o repositório privado ou retirar o que já está publicado.
- **A** = manter o perímetro de agregados já versionados (aggregates_only), sem ampliação de exposição; não presumir anonimização ou licença de derivados. Nova divulgação depende de verificação aplicável.
- **E** = manter aquisição externa (not_versioned); zero arquivos no manifesto atual, não inferir ausência em todo o histórico. Não incorporar novo insumo sem condições verificadas.

Em cada linha, a decisão e a evidência vêm das fontes acima; a alternativa é condicional, não uma autorização de execução. Autoridade **P+D**: responsável do plano (decisão administrativa), titular/licenciante/custodiante e responsável de dados/privacidade competentes conforme o caso (a identificar e validar), além de responsável científico e QA para mudança de pipeline. Critério **V**: comprovar versão/cópia exata, origem, termos, atribuição, permissões de uso e redistribuição separadamente, transformação e risco residual; se houver mudança de insumo, exigir contrato, estimando, regressão e QA. **G**: também cumprir G1–G5 cumulativos e parecer sanitizado autorizado; **C**: também satisfazer o contrato de reabertura de rota T07.7. Para todos, ver efeitos Git na seção 3; o impacto de pipeline indicado é condicional, não testado por execução.

| ID | Arquivos | Decisão | Evidência | Lacuna | Ação condicional (não aprovada) | Pipeline afetável | Autoridade | Reabertura |
| --- | ---: | --- | --- | --- | --- | --- | --- | --- |
| isus_sia | 1 | H | DATASUS SIA/SUS; série 2025; autorização declarada | Consulta, recorte, ZIP, classificação e escopo G1–G5 | Extração oficial/agregação aprovada, preservando unidade de precificação | precificação | P+D | V+G |
| rs_seguro_macrocrime | 1 | H | SSP/RS; camada mensal; autorização declarada | Publicação, filtros, período e agrupamento; G1–G5 | Série oficial e agregação validada | M1 macrocrime | P+D | V+G |
| rs_seguro_macrocrime_profile | 1 | H | SSP/RS; contagens mensais por sexo/faixa/crime; autorização declarada | Escopo específico, versão, transformação e alerta de cruzamento | Agregado oficial/recorte aprovado após avaliação de raridade | M2 perfil, sem presumir equivalência | P+D | V+G |
| mortality | 1 | H | Planilha municipal de contagens/população | Produtor, período, junção, termos, classificação XLSX; G1–G5 | Fontes oficiais territorial/temporalmente concordantes | modelo municipal | P+D | V+G |
| missing_migrants | 16 | H | Portal IOM; rota-mês | Snapshot, termos primários, linhagem e privacidade; G1–G5 | Snapshot autorizado ou agregado rota-mês | contagens registradas, não risco individual | P+D | V+G |
| mega_sena_dezenas | 20 | H | Histórico CAIXA; preparação e derivados | Concursos, extração, transformação e termos | Extração oficial identificada e transformação documentada | pipeline e estatísticas de ordem | P+D | V |
| pnadc_desocupacao | 3 | H | SIDRA 6381/4093; alinhamento e sexo | Consulta, datas e entrada adicional janeiro/2026 | Consultas parametrizadas e versão atribuída | séries e alinhamento trimestral | P+D | V |
| pms_servicos | 1 | H | SIDRA 5906; entrada consolidada | Extração, consolidação e termos | Consulta identificada e consolidação reproduzível | série de serviços | P+D | V |
| importacoes_world_bank | 1 | H | Catálogo World Bank; termos gerais condicionais CC BY 4.0 | Indicador, versão, colunas enriquecidas e termos próprios | Indicadores e enriquecimentos rastreáveis | pipeline de importações | P+D | V |
| prisons_prisoners_unodc | 19 | H | Portal UNODC; atribuição documentada | Exportação, filtros, transformação e redistribuição | Exportação autorizada com metadados e atribuição | pipeline nacional e derivados | P+D | V |
| therapeutic_touch | 1 | H | Artigo Rosa et al. | Tabela, transcrição e termos da compilação | Reconstrução autorizada e documentada; sintético apenas para teste | laboratório de toque terapêutico | P+D | V |
| batting_average | 1 | H | Referência Kruschke a CSV homônimo; 2012 | Edição, identidade/reconstrução e termos | Fonte de estatísticas com termos e reconstrução versionada | laboratório hierárquico | P+D | V |
| exercises_dataset | 1 | H | Referência genérica Kaggle | Página, versão, licença e preparo | Dataset com licença demonstrada e contrato compatível | estudo de exercícios | P+D | V |
| sherlock_holmes_terms | 16 | H | Compilação local de 12 textos | Edição, origem, montagem e direitos da compilação | Corpus de edição com direitos verificados e pipeline reproduzível | pipeline textual e vocabulário | P+D | V |
| sobrevivencia_cadastral_poa | 15 | A | Receita Federal/Prefeitura; apenas agregados | Fontes, termos, transformação e risco residual | Manter exclusão de CNPJ/endereço; eventual agregado aprovado | sobrevivência cadastral, sem dados individuais | P+D | V |
| caes_raca_f5 | 5 | A | Dryad DOI e CC0 da fonte; derivados não individuais | Linhagem e termos de materiais externos | Manter agregados; não expandir para registros individuais | F5; rota adaptada suspensa por contrato | P+D | V+C |
| mortalidade_sp_0_74 | 13 | A | PRO-AIM/SEADE; indicadores E09 e cobertura E08 | Termos, concordância territorial e alerta de campo T09.1; P02 | Avaliação autorizada; manter exclusão de células E04 e originais | APVP; rota adaptada suspensa por contrato | P+D | V+G+C |
| pnadc_estudo_i | 1 | A | SIDRA; síntese descritiva derivada | Versão e termos do snapshot | Manter agregados; não incluir microdados/cadeias E07 | N2 público delimitado; custódia histórica pendente | P+D | V |
| saheart | 0 | E | Aquisição externa; origem no inventário | Termos da versão exata | Não incorporar; aquisição autorizada e documentada | insumo externo, sem blob catalogado | P+D | V |
| rs_seguro_crime_optional | 0 | E | Insumo opcional externo | Origem, termos e autorização específica | Não incorporar; avaliar escopo de autorização e privacidade | rota opcional | P+D | V |
| deter_mensal_bioma_uf | 0 | E | Aquisição externa; DETER mensal | Endpoint/exportação e termos | Manter aquisição externa documentada | série ambiental externa | P+D | V |
| consumer_shopping_trends | 0 | E | Aquisição externa; licença da página não verificada | Página, versão e condições | Não incorporar antes de verificar licença | insumo externo de consumo | P+D | V |
| censo_escolar | 0 | E | Fonte pública; ZIP excluído | Condições e privacidade da versão | Preservar exclusão; aquisição autorizada | insumo externo de educação | P+D | V |
| sat | 0 | E | Aquisição externa | Origem e termos do dataset/package | Manter aquisição externa documentada | insumo externo SAT | P+D | V |
| retractions_time_to_retraction | 0 | E | Aquisição externa; licença da página não verificada | Licença específica e versão | Não incorporar antes de verificar termos | insumo externo de retratações | P+D | V |

Controle: H 14 IDs/83 arquivos; A 4/34; E 7/0; total 25 IDs, 18 com 117 caminhos. Nenhum status de licença ou inventário alterado.

## 3. Implicações Git, forks, backups e pipeline

| Classe | Histórico, forks e backups | Pipeline |
| --- | --- | --- |
| H | Manter blobs e commits no estado atual; restrição de nova divulgação é de governança, não revogação técnica de acesso anterior. Se posterior decisão competente determinar restrição/retirada, inventariar refs, tags/releases, forks, clones, caches, artefatos de CI e backups no perímetro autorizado; não presumir controle de terceiros. | Retirada ou substituição pode quebrar paths, contratos, hashes e reprodutibilidade; avaliar dependências, versão, estimando e regressão antes de mudar. |
| A | Preservar somente o perímetro atualmente versionado. Se avaliação indicar inadequação de agregado, aplicar plano de resposta autorizado; remover em novo commit não elimina objetos anteriores ou cópias externas. | Alterar agregação pode mudar granularidade, denominadores, cobertura, resultados e contratos; manter rotas suspensas. |
| E | Nenhum blob catalogado no manifesto atual; não investigar histórico bruto sem autorização. Evitar incorporar arquivos por conveniência, inclusive em logs, artefatos e backups públicos. | Aquisição depende de disponibilidade, versão e permissões; não introduzir substituto sem QA e contrato. |

Uma eventual retirada/restrição técnica ou reescrita exige decisão específica de autoridade competente, análise de exposição, plano reversível quando possível, preservação protegida de evidências, coordenação de forks/clones/caches/backups conforme competência, impactos sobre SHAs/tags/manifests e CI, comunicação e verificação posterior. Não garantir eliminação de cópias já distribuídas. Reescrita/force push/exclusão não autorizados, não contornar ruleset. Dados sintéticos não substituem resultados empíricos. Se surgir indício verificável, encaminhar à autoridade competente sem divulgar registros; risco atual não equivale a incidente confirmado.

## 4. Testes e entrega

Consulta via conector GitHub, sem clone e sem abrir registros brutos: leitura dos metadados de inventário/manifesto, parser CSV com campos entre aspas, agrupamento por ID e cotejo da árvore recursiva (não truncada). 25 IDs, 117 caminhos únicos, 117/117 caminho/blob/tamanho coincidentes, zero blobs adicionais em data/raw fora do manifesto excluídos Markdown e .gitkeep. Matriz construída com 25 IDs únicos e contagem por classe conferida. Não executados novos fits/testes científicos, inspeção semântica, pesquisa jurídica ou autenticação institucional. QA independente e CI no HEAD deste PR ainda necessários; integração por squash com expected_head_sha e CI pós-merge antes da T01.6.

Escopo do diff: somente este Markdown; nenhum dado, modelo, resultado, manifesto, licença, issue, release ou regra alterados. T01.6 deve executar apenas ações aprovadas (inclusive documentar não alteração material), com plano de resposta; T01.7 consolidará controles e pendências; T09.7 e encerramento global em ciclos separados. Não antecipar aceite/merge. #14 permanece fechada no escopo documental e P01/P02 bloqueiam nova divulgação afetada/homologação material.
