# T01.3 — Gate de privacidade: inspeção bloqueada (03/11/2026)

> **Estado factual: BLOQUEADA / NÃO EXECUTADA.** Este arquivo é um controle documental preventivo, **não** um relatório de inspeção de privacidade, anonimização, conformidade LGPD, autorização de uso ou direito de redistribuição. **T01.3: Exec. ☐ / QA ☐. Etapa 01 permanece aberta.** A revisão independente do próprio procedimento também está pendente. Nenhum dado bruto foi examinado nesta entrega.

## 1. Identidade, fonte e limite deste registro

- Plano de Correção `bayes-stan-r` v1.1, tarefa literal T01.3: **“Fazer inspeção de privacidade em ambiente autorizado, sem expor registros em chats, issues ou logs públicos.”**
- Repositório: `riscotta/bayes-stan-r`; base da branch: `main` em `e84546b62a228105b52ce865eafef118129b9fd9` (pós-merge PR #21).
- Fonte da priorização: [T01.2](DATA_RISK_PRIORITIZATION_T01_2_2026-11-03.md), [inventário](DATA_INVENTORY_2026-11-03.md), [proveniência](DATA_PROVENANCE.csv) e [manifesto](TRACKED_DATA_MANIFEST.csv), restritos **nesta tarefa** à leitura de metadados de governança já públicos.
- Vinculação: [Issue #14](https://github.com/riscotta/bayes-stan-r/issues/14), que **deve permanecer aberta** até que as evidências exigidas sejam avaliadas.
- Proibição operacional: não abrir/baixar/analisar registros dos ZIP, XLSX e CSVs priorizados na ausência de habilitação institucional; não copiar dados, nomes de pessoas, células, observações, cabeçalhos potencialmente identificáveis ou documentos institucionais em PRs, chats ou logs públicos.
- Escopo negativo: nenhum dado ou derivado foi modificado, nenhum script/modelo/resultado foi reexecutado e nenhuma conclusão de ausência de risco foi obtida. Não se modificou o status de licenciamento dos datasets.

## 2. Critérios de pré-acesso — cinco gates cumulativos

| Gate | Evidência mínima **a ser comprovada fora do repositório público** | Estado em 03/11/2026 |
| --- | --- | --- |
| G1 — autoridade | Identidade funcional do custodiante, produtor da cópia, autoridade competente e contato institucional para resposta | **NÃO COMPROVADA** nos artefatos de governança examinados |
| G2 — legitimidade | Autorização documentada para examinar **esta cópia**, com finalidade, abrangência, prazo, participantes e limites | **NÃO COMPROVADA** |
| G3 — ambiente | Ambiente institucional isolado/aprovado, gestão de identidade e acesso, logs protegidos, vedação de exportação/publicação e política de descarte | **NÃO COMPROVADO** |
| G4 — custódia | Identificação da versão/snapshot e hash da cópia, registro de acesso e cadeia de transformação, sem copiar conteúdo ao Git | **NÃO COMPROVADA** |
| G5 — relatório | Protocolo aprovado para comunicar somente conclusões agregadas, achados classificados e riscos residuais sem registros individuais | **NÃO COMPROVADO** |

**Regra de parada:** sem evidência suficiente para **todos** os gates, **não iniciar inspeção**. A autorização genérica do responsável para alterar o repositório **não** é autorização institucional para examinar ou publicar dados de terceiros. Ausência de evidências públicas não prova que a documentação institucional inexista; apenas impede declarar a inspeção autorizada a partir do material disponível.

## 3. Matriz de escopo e pendências — exclusivamente metadados

| `dataset_id` | Arquivos no manifesto | Requisitos específicos ainda pendentes | Inspeção T01.3 |
| --- | ---: | --- | --- |
| `isus_sia` | 1 | Identificar custodiante e autoridade; classificar a cópia ZIP e documentar finalidade, acesso e privacidade | **NÃO EXECUTADA** |
| `rs_seguro_macrocrime_profile` | 1 | Documentar origem do extrato, dicionário, granularidade, autorização e triagem de eventual risco de combinação de atributos | **NÃO EXECUTADA** |
| `rs_seguro_macrocrime` | 1 | Documentar origem, versão, transformação, autorização e correspondência **se demonstrável** com publicação oficial | **NÃO EXECUTADA** |
| `mortality` | 1 | Identificar produtor, classificação, finalidade, unidade analítica, versão e autorização sobre a planilha XLSX | **NÃO EXECUTADA** |
| `missing_migrants` | 16 | Identificar snapshot IOM, termos aplicáveis **à cópia**, linhagem dos artefatos e triagem autorizada de potencial identificação | **NÃO EXECUTADA** |

Os 16 arquivos de `missing_migrants` são artefatos associados ao **mesmo ID**, não 16 bases independentes. Total catalogado dos cinco IDs: **20 arquivos versionados**. Em `DATA_PROVENANCE.csv`, todos estão `versioned=yes`, `redistribution_status=review_required`, `license_status=not_identified` e `review_priority=high`. Estes são **status documentais**, não classificação empírica de sensibilidade.

## 4. Procedimento previsto quando os gates forem cumpridos

1. Autoridade institucional guarda em repositório **não público** a autorização com referência, data, escopo, operador responsável, período e condições. No Git registrar **somente** que evidência foi validada, por qual função e sob qual referência pública opaca permitida.
2. Operador autorizado acessa a cópia exata em ambiente protegido; verifica controles de acesso, hash/snapshot, logs e proibição de exportação não autorizada. Não disponibilizar o dataset à conversa ou a sistemas externos.
3. Aplicar metodologia documentada de triagem: identificação direta e indireta, granularidade, singularidade/raridade, possibilidade de cruzamentos, riscos a titulares e contexto de finalidade. A metodologia e os achados devem ser proporcionais ao acesso permitido.
4. Registrar em ambiente institucional protegido evidência suficiente para auditoria, com acesso restrito, minimização, retenção e encaminhamento formal a encarregado/jurídico/área responsável conforme competência.
5. Redigir **relatório público sanitizado**, por dataset, com alcance da avaliação, decisão técnica, limitações, risco residual, autoridade de aprovação e data, sem células, registros, recortes individualizáveis nem segredos operacionais.
6. Submeter execução, evidências sanitizadas e diff a **QA independente em outro chat/ciclo**. Somente após parecer favorável marcar T01.3 Exec. ☒ / QA ☒. A decisão final de manutenção/restrição/substituição/retirada cabe a T01.4–T01.6, conforme aprovações específicas.

**Se surgir indício verificável de exposição indevida:** preservar evidências e evitar ampliação da circulação; encaminhar à autoridade institucional para avaliação de contenção e resposta, **sem** reescrever histórico Git ou excluir arquivos unilateralmente.

## 5. Evidência de execução documental, risco residual e gate de QA

| Controle | Resultado **observado** | Limitação |
| --- | --- | --- |
| `main` | `e84546b62a228105b52ce865eafef118129b9fd9` conferido via ref e branch da API do GitHub em 03/11/2026 | Fotografia de momento; revalidar antes de futuras operações |
| PR #21 | Mesclado; squash `e84546b62a228105b52ce865eafef118129b9fd9` | Parecer T01.2 não cobre T01.3 |
| Issue #14 | Aberta; pendências institucionais de licença e privacidade | Issue pública não serve como custódia de evidência restrita |
| Cinco IDs | Identificados na proveniência, com os estados indicados acima | Metadados; não comprovam conteúdo, anonimização ou autorização |
| Inspeção de arquivos de dados | **NÃO EXECUTADA** | Acesso institucional, autorização e ambiente protegido não comprovados |
| Testes científicos, R/Python/Stan | **NÃO EXECUTADOS / N/A neste preparo** | Nenhum código, modelo ou resultado alterado |
| QA T01.3 | **NÃO REALIZADO** | Inspeção não executada; não marcar tarefa concluída |
| Merge deste documento | **VEDADO antes do QA próprio do PR** | Revisão do protocolo não substitui QA futuro da inspeção |

**Classificação desta entrega:** preparação documental verificável, sem cumprimento do verbo central “inspecionar”. A tarefa **permanece BLOQUEADA**. A existência deste arquivo, de uma branch ou de um PR não promove o estado da T01.3 a “executada”.

## 6. Continuidade entre chats

**Próximo executor T01.3:** confirmar novamente `main`, estado do PR desta preparação e Issue #14. Solicitar **ao custodiante institucional competente, não à conversa**, os documentos e a infraestrutura que satisfaçam G1–G5. Sem todos os gates, manter **T01.3 BLOQUEADA, Exec. ☐ / QA ☐**, sem tocar os dados. Se aprovados, executar em ambiente institucional conforme o escopo escrito, produzir relatório público sanitizado e abrir **novo ciclo/PR de execução** para QA independente; não confundir avaliação documental deste preparo com inspeção de privacidade. Preservar as tarefas concluídas T00.* e T01.1–T01.2. Não encerrar Issue #14 nem Etapa 01.
