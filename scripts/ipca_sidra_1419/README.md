# IPCA / SIDRA 1419 -> Stan

Script Bayesiano em **Stan** via **cmdstanr** para reconstruir e analisar o IPCA mensal a partir de consultas online ao **SIDRA/IBGE**.

O script:

- baixa a serie headline pela tabela `1737`
- baixa os 9 grupos do IPCA pela tabela `7060`
- valida cobertura, pesos e consistencia do acumulado em 12 meses
- ajusta um modelo hierarquico temporal em Stan
- responde probabilisticamente perguntas alinhadas a manchetes sobre desaceleracao do IPCA

## Arquivos

- Script: `scripts/ipca_sidra_1419/ipca_sidra_1419_cmdstanr.R`
- Entrada: consulta online ao SIDRA (nao exige arquivo local)
- Saidas opcionais:
  - `outputs/tables/ipca_sidra_1419/ipca_sidra_1419_report.txt`
  - `outputs/tables/ipca_sidra_1419/*.csv`

## Como rodar

A partir do **root** do repositorio:

```bash
Rscript scripts/ipca_sidra_1419/ipca_sidra_1419_cmdstanr.R
```

Com janela explicita:

```bash
Rscript scripts/ipca_sidra_1419/ipca_sidra_1419_cmdstanr.R --start_period=202001 --end_period=202602
```

Com exportacao de artefatos:

```bash
Rscript scripts/ipca_sidra_1419/ipca_sidra_1419_cmdstanr.R --save_report=1 --save_tables=1
```

## Opcoes principais

- `--start_period=202001`
- `--end_period=202602`
- `--seed_model=20260318`
- `--chains=4`
- `--parallel_chains=4`
- `--iter_warmup=1000`
- `--iter_sampling=1000`
- `--adapt_delta=0.95`
- `--max_treedepth=12`
- `--init_mode=0`
- `--refresh=200`
- `--save_report=1`
- `--save_tables=1`

## Dependencias

Instale dependencias R:

```bash
Rscript scripts/_setup/install_deps.R --all
```

Instale o CmdStan (uma vez por maquina):

```bash
Rscript scripts/_setup/install_cmdstan.R
```

## Observacoes

- Sem `setwd()`.
- O script deve ser rodado a partir da raiz do repo.
- Por padrao, ele imprime o relatorio no console e **nao salva** artefatos finais.
- A tabela `7060` e usada a partir de `2020-01`, entao `--start_period` deve ser `202001` ou posterior.
- Se algum pacote estiver ausente, o script orienta a instalacao; opcionalmente, aceita `--auto-install`.
