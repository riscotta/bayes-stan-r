# Exercises Dataset — dificuldade, cobertura e modelagem ordinal

Script final reproduzível do estudo sobre o catálogo de exercícios do Kaggle, com preparação da base analítica, exploração descritiva, geração de dados para Stan e modelagem ordinal opcional via `cmdstanr`.

## Arquivos incluídos

- Script principal: `scripts/exercises_dataset/exercises_dataset_cmdstanr.R`
- Modelo ordinal sem equipamento: `scripts/exercises_dataset/modelo_ordinal_dificuldade_sem_equipamento.stan`
- Modelo ordinal com equipamento: `scripts/exercises_dataset/modelo_ordinal_dificuldade_com_equipamento_diagnostico.stan`
- Modelo softmax de sensibilidade: `scripts/exercises_dataset/modelo_softmax_dificuldade_sensibilidade.stan`
- Dado bruto versionado: `data/raw/exercises_dataset/final_exercise_dataset.csv`

## Como executar

A partir da raiz do repositório:

```bash
Rscript scripts/exercises_dataset/exercises_dataset_cmdstanr.R
```

A execução padrão prepara a base, gera tabelas descritivas e exporta os arquivos JSON usados pelo Stan, sem reamostrar os modelos.

Para reexecutar a amostragem Stan/cmdstanr:

```bash
Rscript scripts/exercises_dataset/exercises_dataset_cmdstanr.R --run_stan=1
```

Também é possível usar variável de ambiente:

```bash
RUN_STAN=true Rscript scripts/exercises_dataset/exercises_dataset_cmdstanr.R
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

Pacotes usados diretamente: `readr`, `dplyr`, `tidyr`, `stringr`, `purrr`, `tibble`, `digest`, `jsonlite`, `cmdstanr` e `posterior`.

## Saídas geradas

As saídas regeneráveis são salvas em:

- `outputs/exercises_dataset/tables/`
- `outputs/exercises_dataset/logs/`
- `outputs/exercises_dataset/data_stan/`
- `outputs/exercises_dataset/cmdstan_csv/`, apenas quando `--run_stan=1`

## Observação analítica

O modelo ordinal usa `difficulty` como resposta ordenada (`beginner`, `intermediate`, `advanced`) e avalia padrões associados a grupo corporal, categoria, número de passos, músculos secundários e, em modelo diagnóstico separado, grupo de equipamento. O efeito de equipamento deve ser interpretado como diagnóstico de composição do catálogo, não como efeito causal.
