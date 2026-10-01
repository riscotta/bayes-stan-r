# Outputs

As saídas regeneráveis do repositório seguem um único padrão:

```text
outputs/<estudo>/<tipo>/
```

O identificador do estudo vem sempre imediatamente após `outputs/`.

## Tipos usuais

Cada estudo cria somente os diretórios que utiliza. Os nomes padronizados são:

- `figures/` — gráficos e figuras
- `tables/` — tabelas, relatórios textuais e resumos tabulares
- `models/` — objetos de modelo que façam sentido persistir
- `logs/` — logs de execução
- `cmdstan_csv/` — CSVs produzidos pelo CmdStan
- `data_stan/` — dados preparados para Stan quando exportados como artefato regenerável

Exemplo:

```text
outputs/missing_migrants/
├── figures/
├── tables/
├── logs/
├── cmdstan_csv/
└── data_stan/
```

## Regras

1. Não usar `outputs/figures/<estudo>/`, `outputs/tables/<estudo>/` ou `outputs/models/<estudo>/`.
2. Scripts devem criar seus diretórios de saída quando necessário.
3. Artefatos regeneráveis permanecem fora do Git; apenas `.gitkeep` pode ser versionado para preservar estruturas úteis.
4. Um estudo pode ter subpastas adicionais dentro de um tipo quando isso melhora a organização, por exemplo:
   `outputs/importacoes_world_bank/tables/E07_Implementacao_Estimacao/`.
5. Dados brutos ou insumos auditáveis não devem ser deslocados para `outputs/`; continuam em `data/raw/` quando forem deliberadamente versionados.

A convenção é verificada por teste estático em `tests/testthat/test-output-convention.R`.
