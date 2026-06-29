# IOM Missing Migrants Project — modelo rota-mês

Script final reproduzível do estudo sobre mortos e desaparecidos registrados no **IOM Missing Migrants Project**, com modelo Bayesiano hierárquico de contagem NB2 por rota migratória e mês, usando offset de exposição em dias.

## Arquivos incluídos

- Script principal: `scripts/missing_migrants/missing_migrants_cmdstanr.R`
- Modelo Stan: `scripts/missing_migrants/modelo_nb_hierarquico_rota_mes_estavel.stan`
- CSV bruto: `data/raw/missing_migrants/Missing_Migrants_Global_Figures_allData.csv`
- Dados de modelagem congelados: `data/raw/missing_migrants/model_inputs/`
- Resultados auditáveis: `data/raw/missing_migrants/resultados_auditados/`

## Como executar

A partir da raiz do repositório:

```bash
Rscript scripts/missing_migrants/missing_migrants_cmdstanr.R
```

A execução padrão não reamostra o modelo Stan. Ela recria a base mínima, gera os dados Stan e recompõe tabelas/figuras finais a partir dos artefatos auditáveis versionados.

Para reexecutar a amostragem Stan/cmdstanr:

```bash
Rscript scripts/missing_migrants/missing_migrants_cmdstanr.R --run_stan=1
```

Também é possível usar variável de ambiente:

```bash
RUN_STAN=true Rscript scripts/missing_migrants/missing_migrants_cmdstanr.R
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

Pacotes usados diretamente: `readr`, `dplyr`, `tidyr`, `lubridate`, `stringr`, `jsonlite`, `ggplot2`, `tibble`, `cmdstanr` e `posterior`.

## Saídas geradas

As saídas regeneráveis são salvas em:

- `outputs/missing_migrants/tables/`
- `outputs/missing_migrants/figures/`
- `outputs/missing_migrants/logs/`
- `outputs/missing_migrants/data_stan/`
- `outputs/missing_migrants/cmdstan_csv/`, apenas quando `--run_stan=1`

## Conclusão substantiva do estudo

O estudo deve ser comunicado como análise da carga registrada de mortos e desaparecidos, não como estimativa direta da mortalidade real total. A posterior indica forte concentração por rota e tendência média anual positiva da carga registrada, mas a interpretação deve preservar as ressalvas de diagnóstico: R-hat ligeiramente acima do alvo estrito em poucos parâmetros e falhas de PPC em zeros e dispersão.
