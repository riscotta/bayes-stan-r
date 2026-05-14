# Mega-Sena — distribuição das dezenas por fatores nominais e temporais

Script final reproduzível do estudo sobre a distribuição das dezenas da Mega-Sena por fatores nominais e temporais.

## Arquivos incluídos

- Script principal: `scripts/mega_sena_dezenas/mega_sena_dezenas_cmdstanr.R`
- Modelo Stan principal: `scripts/mega_sena_dezenas/modelo_loglinear_multinomial_fator.stan`
- Modelo Stan global: `scripts/mega_sena_dezenas/modelo_multinomial_global.stan`
- Dados e artefatos auditáveis: `data/raw/mega_sena_dezenas/`

## Como executar

A partir da raiz do repositório:

```bash
Rscript scripts/mega_sena_dezenas/mega_sena_dezenas_cmdstanr.R
```

A execução padrão não reamostra o modelo Stan. Ela recompõe as tabelas e figuras finais a partir dos dados e artefatos auditáveis versionados.

Para reexecutar a amostragem Stan/cmdstanr:

```bash
Rscript scripts/mega_sena_dezenas/mega_sena_dezenas_cmdstanr.R --run_stan=1
```

Também é possível usar variável de ambiente:

```bash
RUN_STAN=true Rscript scripts/mega_sena_dezenas/mega_sena_dezenas_cmdstanr.R
```

## Dependências

Instale as dependências do repositório:

```bash
Rscript scripts/_setup/install_deps.R
```

Para reamostrar Stan, também é necessário ter CmdStan instalado:

```bash
Rscript scripts/_setup/install_cmdstan.R
```

Pacotes usados diretamente: `readr`, `dplyr`, `tidyr`, `ggplot2`, `jsonlite`, `cmdstanr` e `posterior`.

## Saídas geradas

As saídas regeneráveis são salvas em:

- `outputs/mega_sena_dezenas/tables/`
- `outputs/mega_sena_dezenas/figures/`
- `outputs/mega_sena_dezenas/logs/`
- `outputs/mega_sena_dezenas/cmdstan_csv/`, apenas quando `--run_stan=1`

## Conclusão substantiva do estudo

Na escala dos fatores avaliados, a distribuição das dezenas da Mega-Sena é praticamente estável. Os pequenos desvios estimados não sustentam interpretação de mudança substantiva nem autorizam rankings de dezenas ou conclusão preditiva sobre concursos futuros.
