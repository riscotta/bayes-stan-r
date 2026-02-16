# Hidrômetro — degradação / erro de medição (hierárquico)

Modelo Bayesiano em Stan (via **cmdstanr**) para estimar o **erro percentual de medição** em função de:

- **tipo/modelo** de hidrômetro (intercepto hierárquico)
- **idade** (anos)
- **log(volume acumulado)**
- **região/setor** (efeito hierárquico)

O script **simula** um dataset sintético (para validar o pipeline) e ajusta o modelo com HMC/NUTS.

## Como rodar

Do **root** do repositório:

```bash
Rscript scripts/hidrometro_degradacao/hidrometro_degradacao_cmdstanr.R
```

## Saídas

Por padrão é *console-only* (não grava arquivos).

Opcionalmente, você pode salvar:

- PDF com gráficos em `outputs/figures/` via `--save_plots=1`
- TXT com o resumo em `outputs/tables/` via `--save_report=1`

Exemplo:

```bash
Rscript scripts/hidrometro_degradacao/hidrometro_degradacao_cmdstanr.R \
  --save_plots=1 \
  --save_report=1
```

## Opções principais

- `--N=1800` (nº de aferições)
- `--K_model=8` (nº de modelos/tipos)
- `--K_region=12` (nº de regiões)
- `--age_max_years=16`
- `--ppc_draws=250` (subamostra de draws para PPC/PIT)

E parâmetros do sampler:

- `--chains=4`
- `--iter_warmup=1000`
- `--iter_sampling=1000`
- `--adapt_delta=0.95`
- `--max_treedepth=12`
