# Importações anuais por país — World Bank + Stan

Script final reproduzível do estudo sobre importações anuais por país, consolidado para o padrão deste repositório.

## Arquivos incluídos

- Script principal: `scripts/importacoes_world_bank/Script.R`
- Modelo Stan: `scripts/importacoes_world_bank/modelo_principal_hierarquico_student_t.stan`
- CSV bruto versionado: `data/raw/importacoes_world_bank/world_bank_Import_Usd_enriched.csv`

## Como executar

A partir da raiz do repositório:

```bash
Rscript scripts/importacoes_world_bank/Script.R
```

Também é possível informar explicitamente a raiz do projeto:

```bash
Rscript scripts/importacoes_world_bank/Script.R .
```

## Dependências

Instale os pacotes R do repositório:

```bash
Rscript scripts/_setup/install_deps.R
```

Instale e configure o CmdStan:

```bash
Rscript scripts/_setup/install_cmdstan.R
```

Pacotes usados diretamente pelo script: `readr`, `dplyr`, `tidyr`, `tibble`, `stringr`, `posterior`, `cmdstanr`, `loo`, `bayesplot`, `ggplot2`, `jsonlite` e `purrr`.

## Saídas geradas

O script não salva objetos `.rds` por padrão. As saídas regeneráveis ficam em:

- `outputs/tables/importacoes_world_bank/E04_Preparacao_Base_Analitica/`
- `outputs/tables/importacoes_world_bank/E05_EDA/`
- `outputs/tables/importacoes_world_bank/E07_Implementacao_Estimacao/`
- `outputs/tables/importacoes_world_bank/E08_Diagnostico_Validacao/`
- `outputs/figures/importacoes_world_bank/E05_EDA/`
- `outputs/figures/importacoes_world_bank/E07_Implementacao_Estimacao/`
- `outputs/figures/importacoes_world_bank/E08_Diagnostico_Validacao/`
- `outputs/models/importacoes_world_bank/cmdstan_csv/`

## Observação computacional

A modelagem Stan é a parte mais custosa do processamento. Para reprodução completa, o ambiente precisa ter toolchain C++ e CmdStan funcional.
