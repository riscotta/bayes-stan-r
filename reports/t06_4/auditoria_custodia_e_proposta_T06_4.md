# T06.4 — Auditoria factual de custódia B3 e proposta de aceite restrito (13/11/2026)

**Natureza:** investigação documental e de metadados Git, não homologação histórica ou QA independente. **PR #54**, base/main `34f34e83c4918cbb885d51647d5d0b2b785796bb`, HEAD inicial `748e1ee84cf26d4e673f016cde9c66e0437f49d3`. SHA da árvore de referência T06.1: commit `4ed45b81156dfced56b37e226980e1e7da17dcd8`, tree `dab799cbb8ed64eeedb996167ad87a2fc7fb8b50`. A presente adição documental altera o HEAD do PR; a CI e o QA do SHA anterior não cobrem o novo HEAD.

## Método e achados

- Manifestos T06.1, T06.2 e T06.3 contêm os mesmos **33 IDs** na mesma ordem. Os 33 primeiros arquivos de código associados a cada estudo tiveram caminho, tamanho catalogado e SHA de blob Git conferidos na árvore de `4ed45b81156dfced56b37e226980e1e7da17dcd8` (33/33); isto **não** atesta a execução ou o output científico.
- **Nenhuma** das 33 entradas em T06.1 possui itens em `arquivos.outputs_versionados`. Entretanto, há artefatos descritos como resultados/diagnósticos em `entradas[*].arquivos_git` e relatórios versionados. Uma triagem por nomes/estrutura localizou ao menos um **arquivo de resultado ou diagnóstico para 7/33 estudos**. As sete referências escolhidas foram validadas no histórico Git e tiveram SHA-256 dos bytes de texto lidos calculado (7/7); as demais **26/33 não tiveram artefato individual de resultado localizado nesse critério** e permanecem NAO_VERIFICADA para a finalidade específica de resultado histórico. Isso não afirma ausência de outputs em todas as fontes ou que os sete arquivos sejam os originais científicos.
- A planilha `matriz_custodia_T06_4.csv` tem **82 linhas por artefato**: 33 linhas primárias de resultado histórico (7 Git sem custódia; 26 não verificadas), 33 linhas de código rastreável e 16 linhas adicionais de um insumo versionado por estudo quando identificado. Arquivos de entrada potencialmente sensíveis **não foram abertos nem redistribuídos**; sua evidência é apenas metadata de objeto Git. Cada linha separa o tipo de objeto da classe de custódia.
- Registros anteriores localizados: `data/TRACKED_DATA_MANIFEST.csv` e `data/DATA_PROVENANCE.csv`; `scripts/caes_raca_f5/MANIFESTO_PUBLICACAO_SHA256.csv`; `reports/mortalidade_sp_0_74/MANIFESTO_E10_ORIGINAL_SHA256.csv`; `scripts/pnadc_estudo_i/auditar_e10.py` com SHA-256 fixado para CSV E09 antes da T06.4. Histórico Git aponta criação do manifesto E10 em 28/10/2026, inventário de dados em 28/10/2026 (revisado em 02/11), auditor PNAD em 02/11. **Anterioridade no mesmo Git não constitui pin independente/custodiante externo.** O manifesto E10 descreve fotografia do pacote original, não hash canônico do repositório adaptado.
- Verificação de controle: SHA-256 recalculado da síntese pública PNAD = `8bd38ce75917d981431f4c2a9c09ba73648ff1151fd6ac717068637b682fe120`, coincide com constante anteriormente versionada em `scripts/pnadc_estudo_i/auditar_e10.py`. Confirma bytes daquele CSV no Git, não a integridade da cadeia E04→E09 nem a origem externa da síntese.
- **Custódia independente verificada: 0/33 nesta auditoria.** Não foram fornecidos prova de terceiro custodiante, pin pré-comparação externo confirmado, vínculo de autenticidade científica original e autorização de dados por estudo. A ausência é documental; não é prova de adulteração. Issues #14 P0, #15 e #16 continuam abertas. N2 descritivo limitado da T06.2 para PNAD E09 e mortalidade não é estendido aos demais ou promovido a N3/N4.

## Matriz primária dos 33 estudos (resultado histórico, não código)

| Nº | Estudo | Classe do resultado selecionado | Arquivo de evidência |
|---:|---|---|---|
| 01 | `caes_raca_f5` | Git sem custódia | `data/raw/caes_raca_f5/resultados_auditados/auditoria_estratos_suporte.csv` |
| 02 | `censo_escolar_tempo_integral` | Não verificada | Não identificado em T06.1 |
| 03 | `consumer_shopping_trends_beta` | Não verificada | Não identificado em T06.1 |
| 04 | `coortes_aposentadoria` | Não verificada | Não identificado em T06.1 |
| 05 | `deck_15wins` | Não verificada | Não identificado em T06.1 |
| 06 | `deter_mensal_bioma_uf` | Não verificada | Não identificado em T06.1 |
| 07 | `exercises_dataset` | Não verificada | Não identificado em T06.1 |
| 08 | `fruitflies_aft` | Não verificada | Não identificado em T06.1 |
| 09 | `hidrometro_degradacao` | Não verificada | Não identificado em T06.1 |
| 10 | `hierarchical` | Não verificada | Não identificado em T06.1 |
| 11 | `importacoes_world_bank` | Não verificada | Não identificado em T06.1 |
| 12 | `inhaler_ordinal` | Não verificada | Não identificado em T06.1 |
| 13 | `ipca_sidra_1419` | Não verificada | Não identificado em T06.1 |
| 14 | `isus_sia` | Não verificada | Não identificado em T06.1 |
| 15 | `kidney_survival` | Não verificada | Não identificado em T06.1 |
| 16 | `media_ruido_mu` | Não verificada | Não identificado em T06.1 |
| 17 | `mega_sena_dezenas` | Git sem custódia | `data/raw/mega_sena_dezenas/diagnosticos_auditados/E08_checagem_preditiva_posterior_stan.csv` |
| 18 | `missing_migrants` | Git sem custódia | `data/raw/missing_migrants/resultados_auditados/E08_checagem_preditiva_cmdstan_rota.csv` |
| 19 | `mortalidade_sp_0_74` | Git sem custódia | `data/raw/mortalidade_sp_0_74/resultados_auditados/E07_R_diagnosticos_ajuste.csv` |
| 20 | `mortality` | Não verificada | Não identificado em T06.1 |
| 21 | `pms_servicos` | Não verificada | Não identificado em T06.1 |
| 22 | `pnadc_desocupacao` | Não verificada | Não identificado em T06.1 |
| 23 | `pnadc_estudo_i` | Git sem custódia | `data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv` |
| 24 | `prisons_prisoners_unodc` | Git sem custódia | `data/raw/prisons_prisoners_unodc/resultados_auditados/E09_quantidades_derivadas_M1.csv` |
| 25 | `retractions_reason_composition` | Não verificada | Não identificado em T06.1 |
| 26 | `retractions_time_to_retraction` | Não verificada | Não identificado em T06.1 |
| 27 | `retractions_trend` | Não verificada | Não identificado em T06.1 |
| 28 | `rs_seguro` | Não verificada | Não identificado em T06.1 |
| 29 | `saheart` | Não verificada | Não identificado em T06.1 |
| 30 | `sat_model_selection` | Não verificada | Não identificado em T06.1 |
| 31 | `sherlock_holmes_terms` | Não verificada | Não identificado em T06.1 |
| 32 | `sobrevivencia_cadastral_poa` | Git sem custódia | `data/raw/sobrevivencia_cadastral_poa/resultados_auditados/comparacao_modelos_loo_e07b.csv` |
| 33 | `therapeutic_touch` | Não verificada | Não identificado em T06.1 |

A matriz CSV contém os commits, caminhos, blobs, SHA-256 onde calculado, catálogos anteriores identificados, ausência de pin independente, fontes, limitações e medidas pendentes. `NAO_IDENTIFICADO` significa ausência de referência direta no recorte documentado, não uma busca exaustiva de dados fora do repositório.

## Decisão proposta à governança (NÃO aprovada)

**A — Escopo técnico estrito da T06.4:** publicar política geral de preservação e reconciliação, contrato de dados, comparador de igualdade e tolerância e testes **sintéticos**. O resultado máximo `CONFERENCIA_TECNICA` informa apenas autoconsistência sob os parâmetros fornecidos; não certifica cadeia de custódia histórica. B1/B2/B4/B5 preservados, sem reparos adicionais nesta mudança. Os modelos, saídas históricas, priors, likelihoods, estimandos, direitos e níveis não são alterados.

**B — Homologação de resultados históricos:** permanece **FORA da aceitação restrita**. Exige catálogo com origem/versão/custodiante independentes, pin prévio verificável, confirmação commit→path→blob→SHA-256 dos bytes, permissões/privacidade e reprodutibilidade pertinente. Um hash fornecido no mesmo processo ou um arquivo no Git não bastam. Não usar a saída isolada `CONFERENCIA_TECNICA` como atestado de autenticidade, inclusive nos sete estudos com CSV catalogado.

**Gate literal em vigor:** objetivo T06.4 no Plano de Correção = “Definir políticas para resultados históricos: preservação, reconciliação e tolerância a variação numérica”; Word Apêndices CW/CX/CY/CZ exigem evidência B3 para comparações reais e parecer independente favorável para integrar. O CZ prevê como alternativa uma **decisão formal de governança** delimitando a T06.4 exclusivamente à política/comparador sintético. **Esta proposta não altera por si só o gate**, nem substitui o QA REPROVADO #5889678825.

**Solicitação ao responsável competente:** registrar no PR decisão explícita sobre (1) aceitação desse limite estrito como entrega T06.4; (2) permanência da homologação real em trilha pendente, vinculada à #14 e tarefas posteriores, sem status N2/N3/N4 novo; (3) proibição de atestado histórico pelo comparador sem evidência custodiada; (4) necessidade de novo QA independente no SHA resultante e CI verde antes de squash. Caso a governança negue ou considere B3 obrigatório também para política pura, manter PR aberto e implementar somente o gate aprovado.

**Integração proibida até decisão formal, nova CI e novo QA independente favorável.** T06.5 não foi iniciada.
