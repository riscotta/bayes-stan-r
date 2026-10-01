# Tests

Testes são opcionais neste repositório (que é focado em scripts).

Os testes em `tests/testthat/` cobrem smoke checks de estrutura do repo, documentação do índice principal, contratos mínimos de estudos específicos e a convenção `outputs/<estudo>/<tipo>/`.

Para rodar:

```bash
Rscript tests/run_tests.R
```

Se não houver testes, o runner apenas informa e encerra.
