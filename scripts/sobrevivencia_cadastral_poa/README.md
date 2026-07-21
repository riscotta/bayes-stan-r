# Sobrevivência cadastral de estabelecimentos de alimentação fora do lar em Porto Alegre

Estudo reproduzível sobre a permanência cadastral de estabelecimentos de alimentação fora do lar em Porto Alegre, usando análise de sobrevivência em tempo discreto e modelos Bayesianos implementados em Stan.

## Pergunta

Qual é a sobrevivência cadastral dos estabelecimentos nos primeiros cinco anos e como o risco de baixa varia entre coortes, subclasses, natureza jurídica e matriz/filial?

## Arquivos incluídos

- Script principal: `scripts/sobrevivencia_cadastral_poa/sobrevivencia_cadastral_poa_cmdstanr.R`
- Modelos Stan:
  - `scripts/sobrevivencia_cadastral_poa/modelo_M0_rw2_centrado_qr.stan`
  - `scripts/sobrevivencia_cadastral_poa/modelo_M1_rw2_centrado_qr.stan`
- Dados agregados para Stan: `data/raw/sobrevivencia_cadastral_poa/model_inputs/`
- Resultados auditados: `data/raw/sobrevivencia_cadastral_poa/resultados_auditados/`

A base individual com CNPJ e endereço não é publicada. O repositório contém insumos agregados suficientes para reamostrar os modelos sem expor registros identificáveis.

## Como executar

A partir da raiz do repositório:

```bash
Rscript scripts/sobrevivencia_cadastral_poa/sobrevivencia_cadastral_poa_cmdstanr.R
```

A execução padrão copia as tabelas auditadas para `outputs/sobrevivencia_cadastral_poa/` e recria duas figuras centrais a partir das tabelas congeladas.

Para reexecutar M0 e M1 com CmdStan:

```bash
Rscript scripts/sobrevivencia_cadastral_poa/sobrevivencia_cadastral_poa_cmdstanr.R --run_stan=1
```

Configuração congelada: quatro cadeias, 1.500 iterações de warmup, 1.000 amostras por cadeia, `adapt_delta = 0.95`, `max_treedepth = 13`, métrica `dense_e` e seed `20260721`.

## Dependências

Execução padrão: `readr`, `dplyr`, `ggplot2`, `scales` e `jsonlite`.

Reamostragem: `cmdstanr`, `posterior` e CmdStan configurado.

## Resultados principais

- Amostra analítica: 19.134 estabelecimentos e 7.110 baixas cadastrais observadas em até 60 meses.
- Sobrevivência observada: 86,6% em 12 meses, 77,3% em 24, 70,1% em 36 e 57,9% em 60 meses.
- O modelo M1, com efeitos que variam no tempo, superou o M0 na comparação interna: `ΔELPD-LOO = 284,9`, erro-padrão 23,6.
- O M1 apresentou zero divergências, zero saturações de treedepth, R-hat máximo 1,0099 e E-BFMI mínimo 0,409.
- A coorte de abertura foi o principal eixo de heterogeneidade; subclasses e filiais mudaram de posição relativa ao longo do tempo.

## Limites de interpretação

`Baixa cadastral` é um desfecho administrativo e não equivale necessariamente ao encerramento econômico real. As conclusões são descritivas, associativas e preditivas dentro da amostra. O desenho não identifica efeitos causais, não valida previsão de coortes futuras e não modela a dependência entre unidades do mesmo CNPJ básico.
