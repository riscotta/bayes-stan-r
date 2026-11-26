# T01.4 — Reconciliação de autorizações e alternativas (26/11/2026)

**Tarefa literal:** “Obter evidências documentais de autorização ou definir substituição por dados públicos/derivados seguros.” **Escopo desta entrega:** reconciliação documental, não autorização autenticada, substituição aprovada ou inspeção. T01.4 Exec. ☐ / QA ☐ no escopo original; QA deste adendo não promove essas caixas. A decisão de permanência histórica da issue #14 continua vigente.

Base: [main 5e303ecc3e59534a62dae972b378b70c62e10380](https://github.com/riscotta/bayes-stan-r/commit/5e303ecc3e59534a62dae972b378b70c62e10380), PR #84 integrado, [CI pós-merge 45877758506](https://github.com/riscotta/bayes-stan-r/actions/runs/45877758506) completed/success no SHA exato. Fontes: [inventário](DATA_PROVENANCE.csv) (blob `97cfe122f903b8c1b52c57ed0a6c287695f33ba3`), [decisões](DATA_PROVENANCE_DECISIONS.md) (blob `41740623c8da8e6c4c0e1bab72af84e7bb5d4ec8`), [manifesto](TRACKED_DATA_MANIFEST.csv) (blob `ed99382fbe6764a00567d7ec534268398a580145`), [T01.3](DATA_PRIVACY_T01_3_RECONCILIATION_2026-11-26.md), [T09.1](../reports/t09/README.md), [T10.7](../reports/t10_7/README.md), [issue #14](https://github.com/riscotta/bayes-stan-r/issues/14). Word mestre e instrução RETOMADA_ETAPA_01 do pacote pós-Etapa 10: uma tarefa por PR, evidência sanitizada, QA independente e CI pré/pós-merge.

## 1. Limites do aceite

A declaração de 26/11/2026 de origem pública, montagem própria e ausência de restrições é evidência de declaração do responsável, não autenticação dos direitos de terceiros. A declaração de 03/11 sobre autorização institucional e ambiente protegido para ISUS/SIA e RS Seguro não foi autenticada e não se estende automaticamente a outras bases ou versões. O fechamento da #14 resolveu a decisão de permanência documental de 14 conjuntos, não a autorização de novas distribuições. Não reabrir a issue por inércia.

Contagem em metadados: 25 IDs no inventário, 18 com 117 arquivos no manifesto; 14 review_required (83 arquivos), 4 aggregates_only (34 arquivos), 7 not_versioned (zero arquivos catalogados). Não abrir registros. Não inferir privacidade pela granularidade descrita nem por licença aberta. A licença MIT do software não se estende automaticamente a dados de terceiros.

## 2. Matriz de evidências e alternativas — 14 conjuntos históricos

Todos abaixo têm decisão de permanência com rastreabilidade parcial; as alternativas são condicionais, não aprovação de nova coleta, publicação, troca de insumo ou execução científica. A coluna de licença reproduz o estado do inventário, sem nova pesquisa jurídica ou cotejo com arquivos originais. A autorização da cópia exata permanece insuficientemente comprovada para nova redistribuição.

| ID (arquivos) | Evidência documental / licença | Lacuna específica | Alternativa prospectiva a avaliar |
| --- | --- | --- | --- |
| isus_sia (1) | DATASUS SIA/SUS; série 2025; autorização declarada; not_identified | Consulta, recorte, montagem, classificação e abrangência da autorização | Extração oficial documentada e agregação aprovada que preserve a unidade de precificação, condicionadas a G1–G5 e contrato científico |
| rs_seguro_macrocrime (1) | SSP/RS; autorização declarada; not_identified | Publicação, filtros, período, agrupamento e versão | Série oficial documentada; agregação validada antes de eventual substituição |
| rs_seguro_macrocrime_profile (1) | SSP/RS; contagens mensais por sexo/faixa/crime; autorização RS Seguro declarada; not_identified | Vínculo e escopo específico; triagem de cruzamento pendente | Dados oficiais agregados ou recortes aprovados após avaliação de raridade; não presumir equivalência M2 |
| mortality (1) | Planilha municipal, contagens e população; not_identified | Produtor de cada variável, período, junção, classificação e direitos | Fontes oficiais de mortalidade e população com concordância territorial/temporal e termos comprovados; novo protocolo se necessário |
| missing_migrants (16) | Portal IOM, rota-mês; not_identified; menção secundária de licença não basta | Snapshot, termos primários, linhagem e privacidade | Snapshot primário autorizado ou agregado rota-mês aprovado; verificar adequação e risco |
| mega_sena_dezenas (20) | Histórico CAIXA como referência; not_identified | Concursos, extração, transformação, termos | Extração oficial identificada com documentação de transformação e condições de uso |
| pnadc_desocupacao (3) | SIDRA 6381/4093; public_source_no_explicit_dataset_license | Consulta, datas, alinhamento e entrada adicional de janeiro/2026 | Consultas SIDRA parametrizadas, versão e atribuição preservadas, termos verificados |
| pms_servicos (1) | SIDRA 5906; public_source_no_explicit_dataset_license | Extração, consolidação e condições | Consulta SIDRA identificada e transformação reproduzível, verificação de termos |
| importacoes_world_bank (1) | Termos gerais condicionais CC BY 4.0; partial_verified | Indicador, versão, origem das colunas enriquecidas e termos específicos | Indicadores com metadados/licenças próprios e enriquecimentos rastreáveis |
| prisons_prisoners_unodc (19) | Portal e atribuição; attribution_documented_license_unverified | Exportação, filtros, transformação e autorização de redistribuição | Exportação autorizada, atribuição e metadados; agregados somente após análise de direitos e risco |
| therapeutic_touch (1) | Artigo Rosa et al.; not_identified | Tabela, transcrição e termos da compilação | Dados com licença demonstrada ou reconstrução própria autorizada e documentada; sintético só para teste |
| batting_average (1) | Referência Kruschke a CSV homônimo; not_identified | Edição, identidade da cópia ou reconstrução e termos | Fonte de estatísticas com termos comprovados e reconstrução versionada; não assumir identidade |
| exercises_dataset (1) | Referência genérica Kaggle; page_specific_unverified | Página, versão, licença e preparo | Dataset com licença identificada e variáveis adequadas, após avaliação do contrato |
| sherlock_holmes_terms (16) | Compilação local de 12 textos; not_identified | Edição, fonte, montagem e direitos da compilação | Corpus de edição identificada com direitos verificados na jurisdição pertinente e pipeline reproduzível |

## 3. Demais 11 IDs

| ID (arquivos) | Estado documental | Controle prospectivo |
| --- | --- | --- |
| sobrevivencia_cadastral_poa (15) | aggregates_only; source_terms_unverified | Manter exclusão de CNPJ/endereço; documentar fontes, termos, agregação e risco residual antes de nova divulgação |
| caes_raca_f5 (5) | aggregates_only; source_terms_verified (fonte Dryad CC0) | Preservar atribuição, verificar linhagem e não generalizar a materiais externos; sem ampliar dados individuais |
| mortalidade_sp_0_74 (13) | aggregates_only; source_terms_unverified | Alerta T09.1 separado de mortality; avaliação autorizada P02, termos e concordância territorial |
| pnadc_estudo_i (1) | aggregates_only; public_source_no_explicit_dataset_license | Manter apenas agregados documentados; não presumir licença do snapshot integral |
| saheart (0) | not_versioned; not_identified | Manter aquisição externa; avaliar termos antes de versionar |
| rs_seguro_crime_optional (0) | not_versioned; not_identified | Não incorporar sem origem, termos e autorização específica |
| deter_mensal_bioma_uf (0) | not_versioned; not_identified | Registrar endpoint/exportação e termos |
| consumer_shopping_trends (0) | not_versioned; page_specific_unverified | Conferir licença da versão da página antes de incorporação |
| censo_escolar (0) | not_versioned; public_source_no_explicit_dataset_license | Preservar exclusão do ZIP; verificar condições e privacidade da versão |
| sat (0) | not_versioned; not_identified | Conferir origem e termos do dataset/package |
| retractions_time_to_retraction (0) | not_versioned; page_specific_unverified | Verificar licença específica, versão e condições |

## 4. Critérios para futura decisão e transferência de escopo

Para cada cópia, registrar fonte e versão/consulta, ligação entre fonte e blob, transformações, termos/atribuição, autoridade competente quando aplicável, autorização para uso/redistribuição separadamente, triagem autorizada de privacidade e limitações. Para qualquer substituição, definir o novo insumo exato, termos, ambiente, granularidade, equivalência ou alteração de estimando, testes de regressão, versão dos resultados e QA próprio; dados sintéticos não substituem resultados empíricos. Não executar alternativa só porque aparece nesta matriz. Não exigir divulgação de documentos protegidos.

O responsável aprovou em 26/11 a direção de finalizar o plano de correção com ressalvas e transferir evolução científica para manutenção contínua. Isso não é aprovação de alteração de dados, autorização institucional, release ou declaração de execução da inspeção. Recomendar que o encerramento administrativo, se formalmente delimitado, mantenha P01/P02 como bloqueios para nova divulgação dos materiais afetados e preserve a diferença entre execução material, suspensão por evidência insuficiente e decisão documental. T01.5 deve reconciliar decisões por ID e implicações de histórico; T01.6 e T01.7 em ciclos próprios. T09.7 exige decisão própria. Não alterar o Word histórico neste PR.

## 5. Resultado e QA

25/25 IDs reconciliados em documentação, 14/14 decisões históricas preservadas; 117 arquivos contabilizados sem leitura de conteúdo. Nenhuma autorização nova obtida/autenticada, nenhuma substituição definida como aprovada, nenhum dado, modelo, resultado, manifesto, issue ou release alterado. T01.4 original não integralmente executada; adendo documental submetido a QA independente no HEAD exato, CI favorável e squash com SHA esperado, confirmação da CI pós-merge. Não usar este QA para declarar homologação jurídica/institucional ou científica.
