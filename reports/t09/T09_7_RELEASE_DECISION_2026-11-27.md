# T09.7 — Deliberação de não publicação integral (27/11/2026)

**Tarefa literal do Word mestre:** “Somente após P0 resolvido e autorização explícita, deliberar release e comunicação pública.”

**Decisão no escopo revisado:** NÃO PUBLICAR nova release integral neste ciclo; adiar avaliação de eventual pacote elegível até que suas condições próprias sejam satisfeitas. Preservar v1.0.0 e a publicação específica do Deck, sem republicação. A autorização do responsável de concluir o plano de correção com ressalvas e transferir evolução científica para manutenção contínua, reiterada na mensagem de retomada de 27/11, permite registrar a decisão de não publicação; não é autorização de release. O adendo de continuidade e a instrução RETOMADA_ETAPA_01 preveem expressamente a alternativa de não publicação/adiamento. O requisito original de resolver P0 para liberação positiva não foi cumprido; não marcar homologação do escopo original.

## 1. Base e precedência

Base [main e9ff9a081a2ed406b9bc9863531bf2dad1abcb10](https://github.com/riscotta/bayes-stan-r/commit/e9ff9a081a2ed406b9bc9863531bf2dad1abcb10), árvore `aaa327b1fb1045c5a62725a1954561f23023712d`. [PR #88](https://github.com/riscotta/bayes-stan-r/pull/88) integrado, HEAD `5123346ae0fd9f693c2b58cd47a38ea80f0de6bf` com QA Codex completed sem achados ou apontamentos não resolvidos e [CI pré-merge #45993887116](https://github.com/riscotta/bayes-stan-r/actions/runs/45993887116) success; [CI push #45994321918](https://github.com/riscotta/bayes-stan-r/actions/runs/45994321918) completed/success nos três jobs, SHA exato. [Comentário #7217666114 na issue #14](https://github.com/riscotta/bayes-stan-r/issues/14#issuecomment-7217666114) registrado e reconfirmado; issue closed. Etapa 01 encerrada administrativamente no escopo revisado, com P01/P02 mantidas P0. Diff desde a base pós-#85: três documentos T01.5–T01.7 adicionados, +170/-0, sem dados, código, resultados ou alterações de publicação. Ruleset 26061035 ativo, #64 único PR aberto antes do ciclo, draft; #15/#16 abertas.

Fontes: Word mestre de 25/11/2026, adendo Etapa 10 de 26/11, adendo e instruções de continuidade de 27/11; [T09.1–T09.6](README.md), [auditoria T10.7](../t10_7/README.md), [T01.3](../../data/DATA_PRIVACY_T01_3_RECONCILIATION_2026-11-26.md), [T01.4](../../data/DATA_AUTHORIZATION_T01_4_RECONCILIATION_2026-11-26.md), [T01.5](../../data/DATA_DISPOSITION_T01_5_2026-11-27.md), [T01.6](../../data/DATA_ACTIONS_RESPONSE_T01_6_2026-11-27.md), [T01.7](../../data/DATA_GOVERNANCE_T01_7_2026-11-27.md). T09.1–T09.6 permanecem encerradas pelo #76; sem reexecução.

## 2. Estado verificado de publicação e decisão

| Objeto | Evidência | Deliberação |
| --- | --- | --- |
| [Release v1.0.0](https://github.com/riscotta/bayes-stan-r/releases/tag/v1.0.0) | Única release retornada pela API, publicada 01/10/2026, não draft/prerelease, zero assets anexados | Preservar; não editar ou republicar |
| [Deck, downloads/deck](https://github.com/riscotta/bayes-stan-r/commit/24f6a6f4c827de6437799b2eb841afb4ae102261) | Branch no SHA histórico exato, publicação específica previamente deliberada conforme pacote e T10.7 | Preservar; não ampliar autorização a outros estudos ou materiais |
| Nova release integral | Não há pacote aprovado, nem manifesto de release exato, nem comprovação P01/P02 no escopo de redistribuição | NÃO PUBLICAR; não criar tag, draft de release, asset, exportação ou pacote público |
| Nova comunicação pública | Nenhuma autorização de ampliação da divulgação ou alegação de homologação integral | Restringir a documentação administrativa de governança; sem divulgação inédita de dados, resultados ou validação |
| [Issue #16](https://github.com/riscotta/bayes-stan-r/issues/16) | Aberta P2, homepage e ciclo de releases | Manter aberta em manutenção editorial/publicação; não declarar todos os itens resolvidos |

O manifesto de dados rastreia 117 caminhos, não é manifesto de pacote de release. O manifesto T06.1 rastreia objetos dos estudos, não comprova elegibilidade ou direitos de nova distribuição. A manutenção de cópias históricas (#14) não autoriza redistribuição adicional. Não preparar nem listar pacote supostamente elegível sem decisão, escopo e verificação; não abrir dados para fazê-lo. Não publicar código/documentação como pacote por simples exclusão de dados, pois podem existir derivados, referências e dependências a avaliar.

## 3. Critérios cumulativos para reavaliar publicação

| Gate | Evidência exigida no novo ciclo | Papel competente (a confirmar) |
| --- | --- | --- |
| RLS1 — finalidade e escopo | Definir versão, propósito, audiência, modalidade e conteúdo exatos; separar software, dados, resultados, derivados e documentação | Responsável pelo plano/mantenedor |
| RLS2 — manifesto de pacote | Caminho, hash/tamanho, origem/versão, dependências, inclusão/exclusão justificada, verificação de conteúdo permitido no ambiente adequado; verificar arquivos compactados, metadados, logs e artefatos, sem exposição indevida | Mantenedor e operador autorizado |
| RLS3 — direitos e privacidade | Resolver P01/P02 aplicáveis ao perímetro exato: termos/licenças, autorização e atribuição específicas, G1–G5 e parecer de privacidade quando necessário; não presumir anonimização ou MIT para dados | Autoridade de dados/privacidade, titular/licenciante/custodiante conforme competência |
| RLS4 — evidência científica e comunicação | Distinguir níveis N1/N2 delimitados, ausência de promoção N3/N4, 363 estados E00–E10 pendentes, escopo de testes e riscos P03–P07; revisar alegações e limitações de cada item incluído | Responsável científico e QA competente |
| RLS5 — validação técnica/editorial | Conferir pacote exato, integridade, links, instalação/execução autorizada, CI/QA aplicáveis e comunicação coerente; P08 e #16 conforme escopo | Mantenedor e QA independente |
| RLS6 — decisão positiva explícita | Autorização específica do responsável para a composição e comunicação verificadas; aprovação competente dos materiais de terceiros; controle de tag/release e confirmação posterior | Responsável pelo plano e autoridades aplicáveis |

Nenhum gate positivo é declarado satisfeito para um pacote novo. Se houver novo indício verificável de exposição ou licença incompatível, seguir plano T01.6 e autoridade competente. P09 passa de ausência de decisão a **não publicação deliberada no escopo revisado**, após aceite desta entrega; a condição para publicação continua bloqueada. Não transferir P01/P02 para backlog científico nem eliminar P03–P08/P10. Não declarar release ou ciência homologadas.

## 4. Verificações e aceite

Consulta de metadados GitHub: main/árvore, comparação de commits, PRs, issue #14–16, ruleset, lista de releases e branch Deck. Nenhum conteúdo de dados brutos aberto; nenhum novo teste inferencial, pesquisa de licença, empacotamento ou publicação. Único diff candidato: este documento, não alterando relatórios históricos, manifestos, tags, branches preexistentes, #16 ou resultados. Teste pertinente: conferir os links locais na árvore e o diff, verificar a manutenção das referências de publicação. QA independente no HEAD exato, CI success, squash com SHA esperado e CI pós-merge success antes de encerrar administrativamente T09.7. Só então iniciar o ciclo global de controle mestre sucessor. O parecer deste candidato não se autoaprova.
