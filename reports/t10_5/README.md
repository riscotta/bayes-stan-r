# T10.5 — Navegação e consistência documental

**Data:** 26/11/2026. Base fixa: [263e96f24be31f94aecc9534a8f66f964bc2e125](https://github.com/riscotta/bayes-stan-r/commit/263e96f24be31f94aecc9534a8f66f964bc2e125), árvore `9e60056f51162fbce6b38fc5dcb7133bf013cbf3` (676 entradas, não truncada). T10.4 integrada no [PR #80](https://github.com/riscotta/bayes-stan-r/pull/80), [CI pós-merge #45866718675](https://github.com/riscotta/bayes-stan-r/actions/runs/45866718675) success.

## Correções delimitadas

| Arquivo de entrada vigente | Alteração e justificativa |
| --- | --- |
| [README raiz](../../README.md) | Rotas T10.2–T10.4, cobertura real dos três jobs da CI; preservar os comandos, ressalvas e resultados já publicados. |
| [Catálogo de scripts](../../scripts/README.md) | 33 ligações individuais aos READMEs locais e aos adendos; referências T02.7 no contexto de adoção existente; decisão documental da issue #14 sem ampliação de direitos. |
| [Índice de relatórios](../README.md) | Corrigir o estado desatualizado de T07.6 e da issue #14; distinguir registro histórico de encerramento no escopo revisado; incluir índices T08–T10 e referência ao Deck publicado em branch própria. |
| [Guia de testes](../../tests/README.md) | Corrigir delimitadores U+00A7 no lugar de crases e atualizar CI Python PNAD, Stan sintático, R e piloto Cães; separar workflows científicos opcionais. |

Não modificar os 33 READMEs locais, snapshots T02–T10, modelos, dados, gráficos ou tabelas históricas. Não ampliar a exposição de resultados: somente links para documentos existentes e metadados editoriais. Nenhum dataset foi aberto.

## Verificação automatizada do executor

Subconjunto de Markdown inline/ATX: links e imagens inline, badge aninhado, atributos HTML href/src; excluir cercas de código, referências externas da resolução local e linhas de código indentado. Resolver caminhos e fragmentos Markdown contra a árvore base e documentos lidos; não é parser GFM integral ou preview nativo. As referências externas e GitHub não receberam nova validação HTTP universal.

| Origem | Links locais | Âncoras locais | URLs externas/GitHub fora da resolução local |
| --- | ---: | ---: | ---: |
| `README.md` | 39 | 0 | 9 |
| `scripts/README.md` | 90 | 0 | 4 |
| `reports/README.md` | 24 | 0 | 4 |
| `tests/README.md` | 8 | 0 | 0 |
| `scripts/STUDY_CATALOG_T08_2.md` | 736 | 33 | 3 |
| `reports/t10_3/README.md` | 38 | 0 | 3 |
| `reports/mortalidade_sp_0_74/caderno_E01_E10.md` | 8 | 0 | 0 |
| `reports/mortalidade_sp_0_74/relatorio_cientifico.md` | 0 | 0 | 0 |
| `reports/caes_raca_f5/relatorio_cientifico.md` | 1 | 0 | 2 |
| `reports/pnadc_estudo_i/nota_cientifica.md` | 1 | 0 | 3 |
| `reports/t09/README.md` | 18 | 1 | 10 |
| Total | 963 | 34 | 38 |

Destinos relativos ausentes: 0. Âncoras ausentes no subconjunto: 0; âncoras não verificadas no subconjunto: 0. Os 33 IDs do catálogo são únicos e correspondem a 33/33 READMEs e 33/33 adendos. Cotejo lexical de 48 ocorrências de referências explícitas a arquivos R, Python, Stan ou PowerShell: todas existentes na árvore. Não executar comandos nem inferir que flags, acesso, ambiente, pacote, dados ou efeitos colaterais foram validados. As referências de entrada de dados fora da árvore são avaliadas separadamente como opcionais, externas ou localmente requeridas na documentação; não criar arquivos para sanar ausência. Saídas regeneráveis não são tratadas como links quebrados.

## Inspeção manual delimitada

- Metadados: `CITATION.cff` identifica o software e licença MIT; `config/environment.yml` e README concordam em R 4.5.1, cmdstanr 0.9.0, rstan 2.32.7 e CmdStan 2.40.0. A inspeção é textual, não validação de schema CFF nem instalação. O título, licença do código e versão de referência são mantidos; não inferir licença dos dados.
- Comandos: três blocos bash da raiz e o exemplo Therapeutic Touch preservados; catálogo com 33 entradas e 48 referências explícitas de código no recorte lexical. O comando de PNAD N2 não é uma reexecução de E04–E09. O mapa T07.7 mantém suspensões do adaptador R/Stan; não recomendar execução dessas rotas.
- Gráficos: quatro SVGs de Mortalidade SP existem, com elementos svg de abertura/fechamento, dimensões/viewBox e títulos/aria-label. O caderno E01–E10 referencia as quatro figuras; os títulos correspondem às taxas, contribuições por idade, observado/previsto NB2 e decomposição. Não houve renderização visual nativa nem cotejo de valores plotados célula a célula.
- Tabelas/relatórios: leitura de cabeçalhos, legendas e escopo do caderno Mortalidade SP, relatório Cães/F5 e nota PNAD. Preservar unidade por 100 mil, RMSE interno e distinção entre contraste descritivo e posterior conjunta. Não recalcular métricas ou inferências; verificações substantivas pertencem à T10.6.
- Metadados GitHub: issue #14 fechada em 26/11/2026; #15 e #16 abertas no corte consultado; PR #75 integrado, registro T08.7 descreve encerramento T07.6 no escopo revisado. Não reescrever os registros de corte anterior T07.6/T07.7.

## Limites e próximos controles

A [T08.3](../t08_3/README.md) registrou 11 ocorrências de URLs externas inconclusivas, e T08.7 verificou um recorte de links internos. O endpoint de listagem de runs do workflow de URLs externas não retornou informação nesta rodada; não declarar esses links aprovados. O recorte atual é orientado às entradas e guias: não é auditoria de todas as 160 origens Markdown, flags de todos os comandos, disponibilidade externa, preview visual de todo o GitHub ou validade científica. A CI existente cobre outros caminhos relativos e contratos; CI do HEAD exato e parecer independente serão exigidos antes do merge. T10.6–T10.7 não iniciadas; release integral sem decisão.
