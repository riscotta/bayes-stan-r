# T01.3 — Gate de privacidade: autorização declarada, inspeção pendente (03/11/2026)

> **Estado factual atualizado: INSPEÇÃO NÃO EXECUTADA / QA PENDENTE.** O responsável declarou possuir autorização institucional documentada **para inspecionar e publicar no GitHub** as cópias de ISUS/SIA e RS Seguro e ambiente institucional autorizado para inspeção. **A documentação, abrangência por versão e controles do ambiente não foram examinados/verificados por esta execução**; o relato é uma **declaração**, não homologação de licença, privacidade ou direito de publicação. Este arquivo continua sendo controle documental preventivo, não laudo. **T01.3: Exec. ☐ / QA ☐; Etapa 01 aberta.** Nenhum dado bruto foi examinado no chat/GitHub.

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
| G1 — autoridade | Identidade funcional do custodiante, produtor da cópia, autoridade competente e contato institucional para resposta | **DECLARADA pelo responsável** para ISUS/SIA e RS Seguro; competência e referência documental **não verificadas** |
| G2 — legitimidade | Autorização documentada para examinar **esta cópia**, com finalidade, abrangência, prazo, participantes e limites | **DECLARADA** para inspeção e publicação pública; cópia exata, finalidade e condições **não verificadas** |
| G3 — ambiente | Ambiente institucional isolado/aprovado, gestão de identidade e acesso, logs protegidos, vedação de exportação/publicação e política de descarte | **DECLARADO existente**, controles e delimitação **não verificados** |
| G4 — custódia | Identificação da versão/snapshot e hash da cópia, registro de acesso e cadeia de transformação, sem copiar conteúdo ao Git | **NÃO COMPROVADA** |
| G5 — relatório | Protocolo aprovado para comunicar somente conclusões agregadas, achados classificados e riscos residuais sem registros individuais | **NÃO COMPROVADO** |

**Regra de parada:** o relato do responsável atende ao ponto de partida para preparar a inspeção **offline**, mas não comprova os gates. Antes de abrir dados no ambiente institucional, o operador habilitado deve validar **internamente** G1–G5, incluindo permissões para a versão exata e controles de acesso. Nada disso exige transferir documentos ou registros para o chat/GitHub. A autorização para modificar o Git não substitui a autorização institucional nem laudo de privacidade. Ausência de evidência pública não implica inexistência de documento protegido.

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
| Inspeção de arquivos de dados | **NÃO EXECUTADA** | Autorização e ambiente **declarados** pelo responsável, mas validação dos documentos e inspeção de conteúdo não realizadas por esta execução |
| Testes científicos, R/Python/Stan | **NÃO EXECUTADOS / N/A neste preparo** | Nenhum código, modelo ou resultado alterado |
| QA T01.3 | **NÃO REALIZADO** | Inspeção não executada; não marcar tarefa concluída |
| Merge deste documento | **VEDADO antes do QA próprio do PR** | Revisão do protocolo não substitui QA futuro da inspeção |

**Classificação desta entrega:** preparação documental verificável, sem cumprimento do verbo central “inspecionar”. A tarefa **permanece NÃO EXECUTADA, aguardando validação de G1–G5 e inspeção offline no ambiente institucional**. A existência deste arquivo, de uma branch ou de um PR não promove o estado da T01.3 a “executada”.

## 6. Continuidade entre chats

**Próximo executor T01.3:** confirmar novamente `main`, estado do PR desta preparação e Issue #14. O responsável **declarou** autorização institucional de inspeção/publicação para ISUS/SIA e RS Seguro e ambiente protegido; verificar os documentos **dentro da instituição** e comprovar G1–G5, sem compartilhar material restrito. A declaração não vale automaticamente para `mortality` nem `missing_migrants` e não atesta que os dados sejam anônimos. Se G1–G5 forem internamente validados, o operador autorizado realiza a inspeção no ambiente protegido e preenche o [modelo sanitizado de relatório](DATA_PRIVACY_T01_3_PUBLIC_REPORT_TEMPLATE.md) após revisão institucional de divulgação. Até a execução efetiva, manter **T01.3 Exec. ☐ / QA ☐** e abrir ciclo/PR próprio para revisão independente das evidências de execução. Preservar T00.* e T01.1–T01.2, não encerrar Issue #14 nem Etapa 01.

## 7. Atualização após declaração do responsável — 03/11/2026

**Declaração recebida, não auditada:** o responsável informou possuir **autorização institucional documentada para inspeção e publicação pública no GitHub de ISUS/SIA e RS Seguro** e **ambiente institucional autorizado para a inspeção**. Não foram fornecidos documentos, números de autorização, nomes de operadores, dados brutos ou relatórios. Esta atualização registra apenas a declaração expressa, sem atribuir aprovação formal ao custodiante, ao jurídico ou ao encarregado de dados.

**Alcance:** a declaração refere-se a `isus_sia`, `rs_seguro_macrocrime` e, **sob confirmação do alcance documental para o segundo extrato**, `rs_seguro_macrocrime_profile`. Não há declaração correspondente sobre `mortality` ou `missing_migrants`. Direitos de divulgação e suficiência de proteção de privacidade permanecem avaliações distintas. O repositório segue público, e qualquer eventual publicação prévia requer análise específica da cópia/versionamento.

**Operações possíveis neste ambiente:** atualizar documentação e template sanitizado, confirmar diff/CI/PR/Issue, levar PR de protocolo a revisão independente. **Operações não executáveis com os meios disponíveis:** verificar a autenticidade dos documentos institucionais guardados fora do chat, inspecionar registros em ambiente protegido não conectado e emitir laudo/QA independente por autodeclaração.

**Estado atualizado dos gates (por autorização declarada):** G1 = autoridade institucional **declarada, não validada**; G2 = exame e publicação **declarados, documento e escopo não validados**; G3 = ambiente **declarado, controles não validados**; G4 = **pendente de custódia/inspeção**; G5 = **pendente de execução e aprovação**. Somente o operador autorizado pode promover o status com evidências internas protegidas. **T01.3 Exec. ☐ / QA ☐**.

### 7.1 Plano de inspeção sem exposição de dados

1. **No ambiente protegido**, verificar emissor/competência, assinatura ou validade, finalidade, prazo, permissões para cada cópia e para redistribuição pública. Guardar o documento internamente. Registrar apenas referência opaca autorizada no controle reservado.
2. Confrontar cada arquivo local com o snapshot/autorização correspondentes, computar integridade e confirmar cadeia de origem. Não copiar hash sensível de documentação nem identificadores pessoais para GitHub; conferir o SHA do arquivo de dados somente se permitido divulgar.
3. Fazer triagem das colunas e registros **localmente**, contemplando identificação direta, combinações quase-identificadoras, pequenas células, cruzamentos e vínculo temporal/espacial, conforme contexto do estudo. Não publicar amostras, cabeçalhos livres ou logs.
4. Registrar internamente achados, decisões do custodiante e medidas necessárias. Caso haja indício de exposição indevida, preservar evidência e encaminhar ao responsável institucional antes de ação destrutiva.
5. Preparar relatório público conforme [template T01.3](DATA_PRIVACY_T01_3_PUBLIC_REPORT_TEMPLATE.md), com linguagem restrita ao **alcance efetivamente verificado**, e obter autorização institucional de divulgação desse próprio relatório.
6. Submeter a inspeção e o relatório sanitizado a **QA independente**, com HEAD e evidências rastreáveis. Somente após aprovação marcar Exec./QA e examinar T01.4–T01.7. PR de preparação não substitui PR da inspeção.

**Regra de não exposição:** não anexar autorizações completas, arquivos de dados, linhas, células, grupos raros detalhados, nomes de titulares, listas de campos identificáveis, logs de acesso ou diretórios institucionais em chat, issue, PR ou CI.
