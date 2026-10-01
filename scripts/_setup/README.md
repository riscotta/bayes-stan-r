# Setup

Scripts auxiliares para deixar o ambiente pronto.

## Ambiente reproduzivel com renv

O repositorio adota **R 4.5.1** como versao de referencia para o ambiente reproduzivel.

Para inicializar e congelar o ambiente pela primeira vez, a partir da raiz:

```bash
Rscript scripts/_setup/init_renv.R
```

O comando usa o conjunto completo de dependencias declarado em `scripts/_setup/dependencies.R` e resolve o `renv.lock` sem instalar toda a biblioteca. A politica de resolucao e deliberada:

- pacotes gerais: **CRAN estavel**
- `cmdstanr`: **0.9.0**, pelo repositorio oficial Stan r-universe
- R de referencia: **4.5.1**
- CmdStan de referencia: **2.40.0** (instalado separadamente)

O repositorio Stan nao e usado para resolver os demais pacotes, evitando substituir versoes CRAN por builds de desenvolvimento do ecossistema Stan.

Para um ambiente reduzido:

```bash
Rscript scripts/_setup/init_renv.R --minimal
```

A opcao reduzida nao deve ser usada para produzir o lockfile oficial do repositorio.

Depois de clonar o repositorio, materialize a biblioteca registrada no lockfile com:

```bash
Rscript -e "renv::restore(prompt = FALSE)"
```

Se `renv` ainda nao estiver instalado globalmente:

```bash
Rscript -e "if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv'); renv::restore(prompt = FALSE)"
```

## Instalacao direta de dependencias R

O instalador tradicional continua disponivel para uso fora do ambiente `renv`.

Minimo:

```bash
Rscript scripts/_setup/install_deps.R
```

Conjunto completo:

```bash
Rscript scripts/_setup/install_deps.R --all
```

As duas rotas usam a mesma declaracao central de pacotes em `scripts/_setup/dependencies.R`. A instalacao direta tambem usa CRAN para os pacotes gerais e Stan r-universe apenas para `cmdstanr`.

## CmdStan

O `renv` congela os pacotes R, inclusive `cmdstanr`, mas **nao instala nem congela o binario do CmdStan**.

Para instalar a versao canonica **2.40.0**:

```bash
Rscript scripts/_setup/install_cmdstan.R
```

O script fixa `2.40.0` por padrao e confirma a versao instalada ao final.

Opcoes:

- `--version=<versao>` para uma reproducao deliberadamente diferente da referencia
- `--cores=4`
- `--dir=/caminho/para/instalar`

Exemplo explicito equivalente ao padrao:

```bash
Rscript scripts/_setup/install_cmdstan.R --version=2.40.0 --cores=4
```

A toolchain de referencia completa tambem esta registrada em `config/environment.yml`.
