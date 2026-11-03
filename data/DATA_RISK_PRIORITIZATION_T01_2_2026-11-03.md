# T01.2 — Priorização da avaliação autorizada de dados (03/11/2026)

> **Estado:** proposta executada para QA independente; não é parecer jurídico, certificação de anonimização, autorização de redistribuição, laudo de incidente ou decisão de remoção. **Não abrir, reproduzir nem divulgar conteúdo potencialmente restrito em issues, PRs ou chats.**

## 1. Escopo, versão e material de controle

- **Tarefa literal:** "Priorizar avaliação autorizada de ISUS/SIA, RS Seguro, planilha de mortalidade e dados de incidentes IOM".
- **Repositório:** `riscotta/bayes-stan-r`. **Base da branch:** `c2c5b401149b20fd94e57e9c59a67eb8ec9b8566` (`main` após integração do PR #20, T01.1).
- **Fontes internas:** [DATA_PROVENANCE.csv](DATA_PROVENANCE.csv), [TRACKED_DATA_MANIFEST.csv](TRACKED_DATA_MANIFEST.csv), [DATA_INVENTORY_2026-11-03.md](DATA_INVENTORY_2026-11-03.md), [RAW_DATA_REVIEW_2026-10-28.md](RAW_DATA_REVIEW_2026-10-28.md) e [DATA_PROVENANCE.md](DATA_PROVENANCE.md).
- **Governança:** [Issue #14](https://github.com/riscotta/bayes-stan-r/issues/14), aberta. O relatório histórico de 28/10 é preservado como fotografia daquele momento.
- **Limite de acesso:** priorização feita **exclusivamente com metadados públicos já registrados** e documentos do repositório. Nenhuma linha de dados, célula XLSX ou conteúdo do ZIP institucional foi examinado.
- **Estado de origem:** 25 IDs no inventário e 117 arquivos de dados no manifesto. Os cinco IDs abaixo estão `versioned=yes`, `review_required`, `review_priority=high`, `license_status=not_identified`. Nenhum status foi promovido a autorizado.

## 2. Fila de avaliação — cinco datasets, quatro frentes

A ordenação define o **sequenciamento da obtenção de evidências e autorização**, não uma medida empírica de risco ou uma constatação de irregularidade. A2 e A3 podem ocorrer em paralelo. A prioridade B1 não dispensa a triagem de privacidade.

| Ordem | `dataset_id` | Arquivos versionados no grupo | Questão a resolver | Autoridade/evidência necessária |
| --- | --- | ---: | --- | --- |
| **A1** | `isus_sia` | 1 | ZIP de extrato institucional sem classificação, custodiante, proveniência e autorização documentadas | Identificar custodiante e autoridade competente; obter classificação e permissão escrita específica da cópia antes de abrir conteúdo |
| **A2** | `rs_seguro_macrocrime_profile` | 1 | Extrato operacional com recortes de perfil conforme descrição, sem vínculo confirmado aos arquivos públicos SSP/RS | Custodiante do RS Seguro, dicionário, parâmetros de extração/agregação, comprovação de origem e autorização para divulgação |
| **A2** | `rs_seguro_macrocrime` | 1 | Outro extrato operacional, sem demonstração de identidade com fonte pública ou direito de redistribuição | Mesmas autoridades e provas da frente A2, com avaliação individual da cópia |
| **A3** | `mortality` | 1 | Planilha XLSX de produtor e conteúdo não documentados | Identificar produtor, contexto, período, unidade analítica, termos e autorização; somente depois inspecionar |
| **B1** | `missing_migrants` | 16 | Grupo IOM com versão do arquivo de origem, licença primária e potencial de identificação ainda não verificados | Identificar snapshot exato, metadados do download, termos primários IOM, linhagem dos derivados e triagem autorizada |

**Nota de contagem:** os 16 arquivos de `missing_migrants` são artefatos versionados associados ao dataset, não 16 bases brutas individuais de incidentes. A referência à fonte IOM não homologa a cópia local; a menção secundária do ICRC à licença CC BY 4.0 tampouco comprova os termos do CSV específico.

### A1. ISUS/SIA

- **Objeto catalogado:** `data/raw/isus_sia/ISUS_SIA_PARS.zip`. Não inferir tipo de informação apenas pela sigla ou pela extensão.
- **Evidências faltantes:** responsável pelo extrato, fluxo de produção, data/versão, finalidade, classificação de acesso, campos e granularidade **a verificar em ambiente autorizado**, e autorização de divulgação da versão exata.
- **Porta de entrada:** autorização institucional de acesso, ambiente protegido, restrição de circulação adicional e avaliação por custodiante, encarregado de dados e jurídico, conforme competência.
- **Estado atual:** PENDENTE; não inspecionado.

### A2. RS Seguro

- **Objetos catalogados:** `data/raw/rs_seguro/rs_month_macrocrime.csv` e `data/raw/rs_seguro/rs_month_macrocrime_profile_v1_1vict.csv`.
- **Evidências faltantes:** sistema de origem, dicionário, série e data de extração, parâmetros e operações de agregação, cadeia de transformação e autorização explícita. A [fonte oficial SSP/RS](https://ssp.rs.gov.br/dados-abertos) existe, mas a identidade dos extratos locais com suas publicações **não está demonstrada**.
- **Atenção condicional:** se houver combinações raras por sexo/idade, tempo ou local, avaliar possibilidade de identificação por cruzamento. **Não foi constatado** que tais combinações ocorram nos arquivos.
- **Estado atual:** ambos PENDENTES; não inspecionados.

### A3. Planilha de mortalidade

- **Objeto catalogado:** `data/raw/mortality/Dados_Mortalidade.xlsx`.
- **Evidências faltantes:** instituição produtora, período e unidade analítica, finalidade, extração e versão, direitos sobre a cópia, derivação e avaliação de divulgação.
- **Limite:** sem abrir planilhas não se pode declarar que sejam microdados, tabelas agregadas ou dados pessoais. Não confundir com o estudo distinto `mortalidade_sp_0_74`.
- **Estado atual:** PENDENTE; não inspecionado.

### B1. Missing Migrants / IOM

- **Objeto catalogado:** grupo `data/raw/missing_migrants/`, com 16 blobs no manifesto, entre entradas e derivados.
- **Referência primária identificada:** [IOM — Downloads](https://missingmigrants.iom.int/downloads), sem comprovar identidade com o arquivo local.
- **Evidências faltantes:** URL de download específica, data, versão/vintage, hash da aquisição, termos primários de uso e redistribuição, atribuição, linhagem de artefatos e análise de características identificáveis caso presentes.
- **Atenção condicional:** relato, localização ou temporalidade potencialmente identificáveis devem ser examinados somente se houver campos desse tipo, em ambiente autorizado. Nenhum atributo foi confirmado.
- **Estado atual:** PENDENTE; não inspecionado.

## 3. Protocolo sequencial de avaliação (T01.3–T01.7)

1. **Controle imediato:** não ampliar circulação de cópias, trechos de dados, cabeçalhos identificáveis ou conteúdos em PRs/chats. Na hipótese de indício real de exposição indevida, comunicar o responsável institucional e decidir as ações com preservação de evidências; este documento não presume que houve incidente.
2. **Custódia:** identificar para cada ID produtor, detentor da cópia, autoridade de divulgação, técnico da transformação e instância de privacidade. Guardar documentação de autorização em ambiente institucional restrito.
3. **Evidências:** coletar URL/identificador da fonte, versão e data, parâmetros, cadeia de transformações, direitos sobre cópia e derivados e autorização de redistribuição.
4. **Inspeção T01.3:** somente mediante acesso e finalidade autorizados, em ambiente protegido. Classificar exposição e risco de identificação direto/indireto, cruzamento e granularidade sem reproduzir linhas ou atributos sensíveis em evidência pública.
5. **T01.4–T01.5:** separar direitos de utilização, redistribuição, privacidade e segurança; registrar decisão **por dataset**: manter, restringir, substituir, retirar ou pendente, com autoridade, data, fundamento e limite.
6. **T01.6–T01.7:** implementar decisões aprovadas em procedimento próprio, com análise de histórico Git, forks e backups, impacto em pipeline científico, reversibilidade e plano de incidentes. Não reescrever histórico nem remover dados unilateralmente.

**Critério de parada:** falta de autorização, fonte, versão, direito aplicável ou avaliação de privacidade impede homologação. Nenhuma ausência de achado documental pode ser interpretada como dado anônimo ou legalmente redistribuível. A licença MIT do software não licencia automaticamente dados de terceiros.

## 4. Evidências desta execução e limites de QA

| Controle | Resultado | Alcance |
| --- | --- | --- |
| Conferência dos IDs na proveniência | PASS documental | 5 IDs existentes, todos `versioned=yes`, `review_required`, prioridade `high`, licença `not_identified` |
| Contagens no manifesto | PASS documental | 1 + 2 + 1 + 16 = **20 arquivos versionados** associados aos 5 IDs |
| Priorização | PASS documental | Quatro frentes (A1, A2, A3, B1), responsáveis funcionais, evidências e gates explícitos |
| Abertura e inspeção de conteúdo | **NÃO EXECUTADO** | Não foi obtida autorização institucional; T01.3 permanece pendente |
| Licença e parecer jurídico | **NÃO EXECUTADO** | Não há prova de redistribuição autorizada destas cinco cópias |
| Teste científico R/Python/Stan | **N/A neste escopo** | Não alterados dados, códigos, equações, amostras, inferências ou resultados |
| QA independente da T01.2 | **PENDENTE** | Revisar arquivo e commit exatos em chat distinto; não mesclar antes da aprovação |

Este documento é deliberadamente uma **priorização pública de metadados já públicos**; não anexa documentos institucionais, células, registros de incidentes nem dados tabulares. A Issue #14 e os estados `review_required` permanecem abertos. O QA da T01.1, aprovado em 03/11/2026 e registrado no Apêndice N do Word, não é reutilizado como QA da T01.2.
