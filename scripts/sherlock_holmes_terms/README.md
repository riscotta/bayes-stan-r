# Sherlock Holmes — frequências de termos/personagens

Script final reproduzível do estudo sobre frequências de termos/personagens no corpus Sherlock Holmes, com modelo Bayesiano hierárquico Poisson-lognormal e offset de exposição textual.

## Arquivos incluídos

- Script principal: `scripts/sherlock_holmes_terms/sherlock_holmes_terms_cmdstanr.R`
- Modelo Stan: `scripts/sherlock_holmes_terms/modelo_operacional_poisson_lognormal_e07.stan`
- CSV bruto: `data/raw/sherlock_holmes_terms/12_Sherlock_Holmes_Texts.csv`
- Dados de modelagem: `data/raw/sherlock_holmes_terms/model_inputs/`
- Tabelas finais auditadas: `data/raw/sherlock_holmes_terms/tabelas_finais_auditadas/`

## Como executar

A partir da raiz do repositório:

```bash
Rscript scripts/sherlock_holmes_terms/sherlock_holmes_terms_cmdstanr.R
```

A execução padrão não reamostra o modelo Stan. Ela recompõe auditorias, tabelas finais e figuras a partir dos dados e artefatos auditáveis versionados.

Para reexecutar a amostragem Stan/cmdstanr:

```bash
Rscript scripts/sherlock_holmes_terms/sherlock_holmes_terms_cmdstanr.R --run_stan=1
```

Também é possível usar variável de ambiente:

```bash
RUN_STAN=true Rscript scripts/sherlock_holmes_terms/sherlock_holmes_terms_cmdstanr.R
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

Pacotes usados diretamente: `readr`, `dplyr`, `tidyr`, `stringr`, `ggplot2`, `jsonlite`, `cmdstanr` e `posterior`.

## Saídas geradas

As saídas regeneráveis são salvas em:

- `outputs/sherlock_holmes_terms/tables/`
- `outputs/sherlock_holmes_terms/figures/`
- `outputs/sherlock_holmes_terms/logs/`
- `outputs/sherlock_holmes_terms/cmdstan_csv/`, apenas quando `--run_stan=1`

## Conclusão substantiva do estudo

A comparação deve ser comunicada por taxas posteriores por 10 mil tokens, não por contagens brutas. O modelo preserva a exposição textual de cada obra, incorpora incerteza posterior e evita transformar rankings simples em evidência substantiva isolada.
