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
