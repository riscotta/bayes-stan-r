# Média vs Ruído (μ) — “o ruído se cancela” com N

Este exemplo mostra duas ideias que se reforçam:

1) **Lei dos Grandes N / média amostral**: conforme **N cresce**, a distribuição de \(\bar{y}\) concentra em torno do **efeito subjacente** \(\mu\) (o ruído aleatório tende a se cancelar).

2) **Bayes com Stan**: o **posterior de \(\mu\)** também concentra com N (intervalos credíveis ficam mais estreitos, aproximadamente como \(N^{-1/2}\)).

## Como rodar

Do root do repositório:

```bash
Rscript scripts/media_ruido_mu/media_ruido_mu_cmdstanr.R
```

Por padrão, quando rodado via `Rscript` (não-interativo), ele salva:

- `outputs/figures/media_ruido_mu_plots.pdf` (plots)
- `outputs/tables/media_ruido_mu_sim_summary.csv` (resumo do bloco A)
- `outputs/tables/media_ruido_mu_posterior_summary.csv` (resumo do bloco B)

## Parâmetros via CLI (opcional)

Você pode ajustar alguns parâmetros diretamente na linha de comando:

```bash
Rscript scripts/media_ruido_mu/media_ruido_mu_cmdstanr.R \
  --mu_true=2 --sigma_true=5 --Ns=5,10,20,50,100 --R_rep=200 \
  --iter_warmup=500 --iter_sampling=500 --adapt_delta=0.95
```

Dica: se você só quer “ver a ideia” rápido, reduza `--Ns` e `--iter_sampling`.
