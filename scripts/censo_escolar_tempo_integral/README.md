# Censo Escolar 2021-2025 -> tempo integral na rede publica

Script Bayesiano em **Stan** via **cmdstanr** para consolidar as matriculas presenciais em tempo integral da rede publica a partir dos **microdados oficiais do Censo Escolar**.

O script:

- baixa os ZIPs oficiais do INEP quando necessario
- valida as colunas esperadas com base no dicionario oficial dentro de cada ZIP
- usa as contagens oficiais por escola e etapa
- agrega por `ano x UF x dependencia administrativa x etapa`
- ajusta um modelo binomial multinivel em Stan
- replica a estatistica observada nacional e produz um resumo posterior para o Brasil por ano

## Arquivos

- Script: `scripts/censo_escolar_tempo_integral/censo_escolar_tempo_integral_cmdstanr.R`
- Entrada: downloads oficiais do INEP (sem necessidade de versionar os ZIPs no git)
- Cache local padrao: `data/raw/censo_escolar/`
- Saidas opcionais:
  - `outputs/tables/censo_escolar_tempo_integral/censo_escolar_tempo_integral_report.txt`
  - `outputs/tables/censo_escolar_tempo_integral/*.csv`

## Fonte oficial

- Pagina de microdados do Censo Escolar no INEP:
  - `https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-escolar`

Os links diretos atualmente mapeados no script sao:

- 2021: `https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2021.zip`
- 2022: `https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2022.zip`
- 2023: `https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2023.zip`
- 2024: `https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2024.zip`
- 2025: `https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2025_.zip`

## Como rodar

A partir do **root** do repositorio:

```bash
Rscript scripts/censo_escolar_tempo_integral/censo_escolar_tempo_integral_cmdstanr.R
```

Com anos explicitos:

```bash
Rscript scripts/censo_escolar_tempo_integral/censo_escolar_tempo_integral_cmdstanr.R --years=2021:2025
```

Forcando novo download dos ZIPs:

```bash
Rscript scripts/censo_escolar_tempo_integral/censo_escolar_tempo_integral_cmdstanr.R --force_download=1
```

Salvando relatorio e tabelas:

```bash
Rscript scripts/censo_escolar_tempo_integral/censo_escolar_tempo_integral_cmdstanr.R --save_report=1 --save_tables=1
```

## Opcoes principais

- `--years=2021:2025`
- `--seed=42`
- `--chains=4`
- `--parallel_chains=4`
- `--iter_warmup=1000`
- `--iter_sampling=1000`
- `--adapt_delta=0.95`
- `--max_treedepth=12`
- `--refresh=100`
- `--cleanup_temp=1`
- `--keep_base_ana=0`
- `--download_dir=data/raw/censo_escolar`
- `--force_download=0`
- `--download_timeout=7200`
- `--save_report=0`
- `--save_tables=0`

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
- O cache local em `data/raw/censo_escolar/` e **ignorado no git**.
- Por padrao, o script **nao salva** artefatos finais.
- Por padrao, `base_ana_all` **nao e mantida em memoria** para reduzir uso de RAM; use `--keep_base_ana=1` se voce precisar desse objeto.
- O ano de 2025 usa o link oficial atualmente exposto pelo INEP com sufixo `_2025_.zip`.
