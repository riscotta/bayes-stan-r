# Deck — "Quantas partidas para chegar em r vitórias?"

Este exemplo simula dados (*fake*) de várias "sessões" por deck, onde cada sessão termina quando o jogador atinge **r vitórias**. O dado observado por sessão é:

- `losses`: número de derrotas antes de completar `r` vitórias
- `N = r + losses`: número total de partidas na sessão

## Modelo

Por deck `d`:

- `p[d] ~ Beta(a0, b0)`
- `losses ~ NegBin(r, p[d])`  (falhas antes de `r` sucessos)

Como esse modelo é **conjugado**, dá pra validar o Stan comparando com o posterior analítico:

- `p[d] | dados ~ Beta(a0 + r*S_d, b0 + sum(losses_d))`

O script imprime:

- recuperação de parâmetros (`p_true` dentro do IC 95%)
- checagem analítica vs Stan (`delta_mean`/`delta_q50` ≈ 0)
- diagnósticos HMC (divergências / treedepth / Rhat / ESS / E-BFMI)
- PPC numérico (média de `N` por deck)

## Como rodar

Sempre a partir do **root** do repositório (sem `setwd()`):

```bash
Rscript scripts/deck_15wins/deck_15wins_negbin_beta_cmdstanr.R
```

### Opções úteis

```bash
Rscript scripts/deck_15wins/deck_15wins_negbin_beta_cmdstanr.R \
  --r=15 --S_per_deck=12 --a0=2 --b0=2 \
  --p_true=0.48,0.52,0.55,0.60 \
  --chains=4 --iter_warmup=1000 --iter_sampling=1000 \
  --adapt_delta=0.99 --max_treedepth=15
```

> Dica: se aparecer divergência, suba `--adapt_delta`. Se estourar treedepth, suba `--max_treedepth`.
