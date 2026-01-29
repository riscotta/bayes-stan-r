# Therapeutic Touch — Análise Bayesiana (cmdstanr)

## Como rodar
1. Instale CmdStan (uma vez):
   - R: cmdstanr::install_cmdstan()

2. (Opcional) Reprodutibilidade:
   - renv::restore()

3. Rode o script:
   - Rscript scripts/therapeutic_touch/therapeutic_touch.R

## Entradas
- data/raw/TherapeuticTouchData.csv com colunas:
  - y (0/1)
  - s (id do sujeito)

## Saídas
- outputs/therapeutic_touch/relatorio_final.txt
- outputs/therapeutic_touch/*.png
- outputs/therapeutic_touch/ranking_sujeitos.csv
