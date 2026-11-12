# T06.1 — Manifesto por estudo (snapshot Git; não homologação)

**Tarefa literal:** “Criar manifesto por estudo: arquivos, permissões, entrada, versão, SHA, seed, ambiente e comandos.”

**Corte:** 12/11/2026 · **Base `main`:** `4ed45b81156dfced56b37e226980e1e7da17dcd8` · **Cobertura:** 33 unidades da taxonomia T02.1, excluindo `scripts/_setup` (infraestrutura).

## Artefatos e interpretação

- [`manifesto_estudos_T06_1.json`](manifesto_estudos_T06_1.json): registros individualizados de estudos (classe editorial, scripts/Stan, README, relatórios e outputs versionados, referências de datasets, estados de licença, SHA Git por arquivo, seed/documentação, ambiente de referência e comandos).
- `tests/python/test_manifesto_t06_1.py`: confronto com a árvore Git e `data/DATA_PROVENANCE.csv` e testes adversariais contra falso SHA, falsa autorização, seed inventada e falsa execução.
- Cada `git_blob_sha` identifica os bytes versionados **no snapshot base**. É hash do objeto Git do arquivo, **não** SHA-256; `base_git_commit` identifica a versão auditada do repositório. Metadados não verificam a validade do conteúdo do arquivo.
- `entradas` é **vínculo com o catálogo de proveniência**, não promessa de que todos os arquivos nele são insumos brutos, necessários ou redistribuíveis. Subpastas `resultados_auditados` podem conter resumos derivados; vazios indicam ausência de relação catalogada, não ausência de insumos.
- `comandos_documentados` são comandos dos READMEs ou da CI, **não rodados nesta tarefa**. Os comandos opcionais `--run_stan=1` podem ser custosos e dependem de autorização e insumos locais; não executar sem os gates próprios.
- `seed.valor` é somente o valor explícito identificado em README onde existe; **não** prova que determina toda reamostragem, que foi usada no resultado publicado ou que o pipeline é determinístico. Demais unidades: `PENDENTE_DE_VERIFICACAO`.
- `config/environment.yml` e `renv.lock` são referência global; o ambiente efetivamente usado **por estudo não foi reconstruído**. Exemplo: Cães F5 registra CmdStan histórico 2.37.0, distinto da referência CmdStan 2.40.0.

## Limites científicos e jurídicos

Este PR é um inventário de superfície e integridade N1. Não lê arquivos brutos institucionais, não executa R/Stan e não cria resultados ou inferências novas. A CI com Python valida a estrutura e os hashes de fontes do snapshot; **CI verde não é PASS N3/N4**, validação científica ou licença.

`data/DATA_PROVENANCE.csv` contém status preliminares como `review_required`, `not_versioned`, `aggregates_only`. Nenhuma entrada recebe `HOMOLOGADA`: a issue [#14](https://github.com/riscotta/bayes-stan-r/issues/14) permanece P0, incluindo ISUS/SIA, RS Seguro, mortalidade, IOM e materiais com direitos não verificados. Issues [#15](https://github.com/riscotta/bayes-stan-r/issues/15) e [#16](https://github.com/riscotta/bayes-stan-r/issues/16) também abertas. Não redistribuir dados adicionais nem publicar release com base neste manifesto.

A tarefa T06.1 **não** conclui a Etapa 06: a classificação de cinco estados é T06.2, modos auditáveis T06.3, política de saídas T06.4, lacunas externas T06.5, piloto efetivamente executado T06.6, atualizações dos READMEs T06.7.

## Verificação

Na raiz do checkout do PR:

```bash
python -m unittest discover -s tests/python -p 'test_manifesto_t06_1.py' -v
```

O teste `git ls-tree -r HEAD` consulta metadados Git **sem ler o conteúdo dos datasets**. Se algum código de estudo for alterado no futuro, atualizar o manifesto sob novo QA antes de reivindicar integridade naquele novo HEAD.


## Correção após QA independente reprovado — 12/11/2026 (mesmo PR #51)

A revisão do SHA `d48f46a` reprovou o manifesto original por entradas e seeds omitidas e testes não adversariais. Nesta correção, o manifesto explicita **25** registros no catálogo de proveniência e **27 vínculos fonte→estudo** (incluindo aliases compartilhados de retratações e o cache INEP de Censo Escolar), sem atribuir propriedade/redistribuição a qualquer estudo. O catálogo foi confrontado com o mapa de associação explícito em `tests/python/test_manifesto_t06_1.py`; referências críticas de entrada e **14 seeds documentadas** são também confrontadas com o texto dos READMEs, não com o próprio manifesto. Aplicação das seeds nas execuções históricas **não foi verificada**.

Em `arquivos`, `outputs_versionados` contém somente arquivos que não são `.gitkeep`; os **23** arquivos `outputs/**/.gitkeep` ficam em `placeholders_diretorio_versionados`. Placeholders preservam diretórios e **não são resultados científicos**. Para fontes documentadas em README mas sem dataset próprio em `DATA_PROVENANCE.csv`, `lacunas_entradas_externas_nao_catalogadas` marca a pendência: coortes de aposentadoria (Yahoo Finance e BCB/SGS 4390/433), FruitFlies/Stat2Data, inhaler/brms, IPCA/SIDRA 1737/7060 e kidney/survival. A ausência de registro de catálogo **não prova inexistência do insumo**, nem os links garantem download, versão, licença ou execução.

Os testes verificam cobertura cruzada e remoções adversariais de entrada, alias, seed e lacuna, rejeitam claims indevidos de licença/execução e conferem os hashes Git. A validação é **N1 documental/estrutural**; não inspeciona ZIPs, CSVs institucionais ou outros dados brutos, não reproduz sampling nem homologa permissões. A issue #14 (P0) e #15/#16 permanecem abertas. O PR só poderá ser aprovado por **outro chat revisor no SHA corrigido**, após checks conclusivos.

## Retificação B4/B5 após novo QA REPROVADO — 12/11/2026

O estado `estado_entrada` é verificado contra `entradas` e lacunas externas declaradas: havendo vínculo catalogado, utiliza-se `REFERENCIAS_DOCUMENTADAS_COM_RESTRICOES`; sem vínculo e com lacuna documentada, `FONTE_EXTERNA_DECLARADA_SEM_REGISTRO_DE_PROVENIENCIA`; sem ambos, `SEM_VINCULO_INVENTARIO_VERIFICADO`. Os vínculos do Censo Escolar e dos dois estudos de retratações não são considerados homologação de dados ou permissão institucional.

O validador agora confronta **conjuntos completos de caminhos** de `reports/<estudo>/` e `outputs/<estudo>/` com a árvore Git: relatórios, saídas não-placeholder e `.gitkeep` têm grupos distintos. Caminhos fora dessas pastas por estudo permanecem fora desta cobertura (por exemplo, `reports/t05_7/` e `outputs/README.md`). Verifica duplicidade, itens ausentes, existência e SHA Git; não lê bytes dos dados institucionais. Cada comando deve constar **literalmente** no README do estudo ou no workflow CI indicado, não sendo executado pelo teste.

Casos adversariais adicionados: estados incompatíveis para entradas catalogadas, lacunas e ausência de vínculo; omissão de relatório em dois estudos; omissão de placeholder acompanhada de redução artificial da contagem; comando inventado em README e CI. Esses controles medem integridade e exaustividade do inventário N1 no snapshot, não validade inferencial N3/N4, exatidão dos comandos, presença de dependências externas ou direitos de redistribuição. QA independente permanece pendente para o novo SHA; issue #14 P0 e #15/#16 não encerradas.
