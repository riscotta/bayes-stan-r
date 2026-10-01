# Config

Esta pasta concentra configuracoes e metadados globais do repositorio.

## Ambiente de referencia

O arquivo `config/environment.yml` registra a toolchain canonica usada para novas reproducoes do projeto:

- R 4.5.1
- cmdstanr 0.9.0
- CmdStan 2.40.0
- rstan 2.32.7

O `renv.lock` continua sendo a fonte operacional para as versoes dos pacotes R. O arquivo de ambiente documenta tambem componentes externos ao `renv`, especialmente o CmdStan.

## Outras configuracoes

Quando fizer sentido, esta pasta tambem pode concentrar defaults do repositorio, por exemplo:

- parametros padrao de MCMC
- caminhos relativos
- configuracoes de relatorios
- metadados globais de execucao

Scripts devem continuar montando caminhos a partir da raiz do repositorio, sem `setwd()`.
