# Kidney — survival lognormal com censura à direita (cmdstanr)

Este exemplo ajusta um **modelo Bayesiano lognormal** para tempo‑até‑evento com **censura à direita**, incluindo:

- Efeitos fixos: `age * sex + disease`
- Efeitos aleatórios por paciente: `(1 + age | patient)` com correlação
- *Prior predictive* (sanity check de escala)
- Diagnósticos HMC/NUTS (divergências, treedepth, Rhat, ESS, E‑BFMI via CmdStan)
- PPC numérico simples e auditável

## Como rodar

Do root do repositório:

```bash
Rscript scripts/kidney_survival/kidney_lognormal_survival_cmdstanr.R
```

Flags úteis:

- `--keep-tmp` → não apaga o diretório temporário (útil para depurar em Windows)

## Dados

O script usa o dataset **`kidney`** do pacote **`survival`** (76 observações, 38 pacientes, 2 eventos por paciente), originalmente discutido em McGilchrist & Aisbett (1991).

> Observação: aqui o indicador de evento é `status` (1=evento, 0=censurado). O script converte para `cens_right` (1=censurado à direita).

## Dependências

- R packages: `cmdstanr`, `posterior`, `survival`
- CmdStan instalado e configurado para o `cmdstanr`

Veja:
- `scripts/_setup/install_deps.R`
- `scripts/_setup/install_cmdstan.R`
