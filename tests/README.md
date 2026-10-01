# Tests

O repositório é focado em scripts, mas os testes estruturais são executados automaticamente pelo CI.

Os testes em `tests/testthat/` cobrem smoke checks de estrutura do repo, documentação do índice principal, contratos mínimos de estudos específicos e a convenção `outputs/<estudo>/<tipo>/`.

Para rodar:

```bash
Rscript tests/run_tests.R
```

Se não houver testes, o runner apenas informa e encerra.

## Proveniência de dados

O teste `tests/testthat/test-data-provenance.R` garante que todo item de primeiro nível versionado ou documentado em `data/raw/` esteja coberto por `data/DATA_PROVENANCE.csv` e que registros marcados como versionados apontem para caminhos existentes.

## CI

O workflow `.github/workflows/ci.yml` valida a sintaxe dos arquivos R em `scripts/` e `tests/` e executa este runner em pushes e pull requests para `main`.
