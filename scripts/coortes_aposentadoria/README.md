# Coortes de Aposentadoria (Brasil) — 30 anos (retornos reais)

**Objetivo:** mostrar como a **sequência de retornos** (e o *ano de aposentadoria*) afeta a riqueza final, mantendo:

- mesma pessoa
- mesmo aporte mensal **real**
- mesma janela de contribuição: **30 anos (360 meses)**

**Fontes (mensal):**

- Ações: Ibovespa (`^BVSP`, Yahoo Finance, `Adj Close`)
- Renda fixa: Selic mensal (BCB/SGS **4390**)
- Inflação: IPCA (BCB/SGS **433**) → converte tudo para **retorno real**

## Como rodar

A partir do **root** do repo:

```bash
Rscript scripts/coortes_aposentadoria/coortes_aposentadoria_console_only_v2.R
```

## Opções principais (formato `--chave=valor`)

- `--from_date=1990-01-01`
- `--to_date=2026-02-20` *(padrão: `Sys.Date()`)*
- `--work_years=30`
- `--contrib_real=1`
- `--ticker_eq=^BVSP`
- `--show_plots=0|1` *(padrão: 0; não salva em disco)*

Diagnóstico de *sequence of returns* no fim da janela:

- `--do_sequence_diagnostics=0|1` *(padrão: 1)*
- `--end_years=5`
- `--seq_strategies=eq_100,tdf_step`

## Saída

- **Console-only**: imprime o resumo por estratégia (p05…p95, média, sd, melhor/pior coorte etc.)
- Top/Bottom 10 coortes para duas estratégias (ações 100% e glide por degraus)
- (Opcional) gráficos na tela se `--show_plots=1`
