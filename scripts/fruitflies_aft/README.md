# FruitFlies — replicacao AFT log-normal (cmdstanr)

Este exemplo integra ao repositório uma **replicação Bayesiana do estudo FruitFlies** com **R + Stan via cmdstanr**, usando um modelo **AFT log-normal** parametrizado em `log(y)` para maior estabilidade numérica.

O script:

- carrega o dataset `FruitFlies` do pacote **Stat2Data**
- prepara a matriz de desenho com **tratamento + thorax padronizado**
- ajusta um modelo AFT log-normal em **Stan**
- imprime diagnósticos HMC/NUTS e resumos posteriores no console
- produz um **posterior predictive check** e um gráfico de médias ajustadas
- pode salvar artefatos opcionais em `outputs/`

## Arquivos

- Script: `scripts/fruitflies_aft/fruitflies_aft_lognormal_cmdstanr.R`
- Entrada: `Stat2Data::FruitFlies`
- Saídas opcionais:
  - `outputs/figures/fruitflies_aft/fruitflies_aft_plots.pdf`
  - `outputs/tables/fruitflies_aft/fruitflies_aft_report.txt`

## Como rodar

A partir do **root** do repositório:

```bash
Rscript scripts/fruitflies_aft/fruitflies_aft_lognormal_cmdstanr.R
```

Salvando gráficos e relatório:

```bash
Rscript scripts/fruitflies_aft/fruitflies_aft_lognormal_cmdstanr.R --save_plots=1 --save_report=1
```

## Opções principais

- `--seed=42`
- `--chains=4`
- `--parallel_chains=4`
- `--iter_warmup=1000`
- `--iter_sampling=2000`
- `--adapt_delta=0.99`
- `--max_treedepth=12`
- `--refresh=250`
- `--save_plots=0`
- `--save_report=0`

## Dependências

Instale dependências R:

```bash
Rscript scripts/_setup/install_deps.R --all
```

Instale o CmdStan:

```bash
Rscript scripts/_setup/install_cmdstan.R
```

## Observações

- Sem `setwd()`.
- O script deve ser executado a partir da raiz do repositório.
- Por padrão, ele é **console-only**.
- A variável `sleep` é lida, mas não entra no modelo principal por ser potencialmente **pós-tratamento**.
- A modelagem em `log(y)` mantém a interpretação principal do AFT log-normal e melhora a estabilidade numérica.
