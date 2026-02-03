# Setup

Scripts auxiliares para deixar o ambiente pronto.

## 1) Dependências R

Mínimo (suficiente para rodar os exemplos atuais):

```bash
Rscript scripts/_setup/install_deps.R
```

Mais amplo (útil se você for gerar relatórios, testes, etc.):

```bash
Rscript scripts/_setup/install_deps.R --all
```

## 2) CmdStan (para `cmdstanr`)

```bash
Rscript scripts/_setup/install_cmdstan.R
```

Opções:

- `--version=2.35.0` (ou outra)
- `--cores=4`
- `--dir=/caminho/para/instalar` (opcional)

Exemplo:

```bash
Rscript scripts/_setup/install_cmdstan.R --version=2.35.0 --cores=4
```
