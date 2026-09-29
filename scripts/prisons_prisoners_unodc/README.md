# UNODC Prisons and Prisoners — heterogeneidade entre países, prisão sem sentença e contraste por sexo

Estudo Bayesiano reproduzível com dados do **UNODC Prisons and Prisoners**. O trabalho separa três perguntas: heterogeneidade de `held_rate`, nível de `unsentenced_pct` e contraste Female−Male na proporção de pessoas presas sem sentença.

## Execução

A partir da raiz do repositório:

```bash
Rscript scripts/prisons_prisoners_unodc/prisons_prisoners_unodc_cmdstanr.R
```

A execução padrão é rápida: recompõe os resultados finais a partir dos artefatos auditados versionados em `data/raw/prisons_prisoners_unodc/resultados_auditados/`.

Para reexecutar a amostragem Stan/cmdstanr:

```bash
Rscript scripts/prisons_prisoners_unodc/prisons_prisoners_unodc_cmdstanr.R --run_stan=1
```

A reamostragem usa os insumos Stan históricos versionados em `model_inputs/` e as especificações finais executadas no estudo:

- **M1 REV5**: hurdle para zero + Student-t em `log(held_rate)` na parte positiva;
- **M2 REV4.1**: zero-one-inflated Beta para `unsentenced_pct` Total;
- **M3 REV4.1**: zero-one-inflated Beta pareado para o contraste Female−Male.

## Dados versionados

`data/raw/prisons_prisoners_unodc/` contém:

- `model_inputs/base_analitica_nucleo_nacional.csv`: base analítica nacional usada no estudo;
- `model_inputs/stan_data_M1.json`, `stan_data_M2.json`, `stan_data_M3.json`: entradas congeladas dos modelos;
- `resultados_auditados/`: artefatos mínimos para reproduzir a síntese final sem reamostrar MCMC.

A proveniência e a identificação da fonte UNODC estão documentadas no README da pasta de dados.

## Resultados centrais do estudo

- Heterogeneidade entre países em `held_rate`: `exp(sigma_country) ≈ 1,94` por 1 DP do efeito de país.
- `unsentenced_pct`: média observada de aproximadamente 32,1%; o M2 reproduz a média agregada melhor do que a mediana.
- Contraste Female−Male: +3,72 p.p. no painel pareado e +1,79 p.p. quando a composição temporal é fixada.
- O PSIS-LOO agrupado por país não validou a tarefa de transportar a predição para países novos.
- O desenho é observacional; as conclusões são descritivas/associativas, não causais.

## Integridade das especificações finais

Os três arquivos Stan versionados nesta pasta correspondem às especificações finais preservadas nos pacotes auditados do projeto. Seus hashes SHA-256 são:

```text
M1 REV5  9fd045704b9d8aadc559df21032659e86c638b28ee9beb90b351fa64124500a6
M2       3b11e59fb5ea327f0d13946d5048b4cd258a909e984496a9cf4fb117ba2dccc7
M3       c8ddb41988a4b4948161b6486a539e48be17df5709dac5ace5601035599991a4
```

Os insumos Stan congelados também são versionados. Assim, a reprodução não depende dos objetos de sessão R nem dos CSVs brutos completos do CmdStan usados na execução histórica.
