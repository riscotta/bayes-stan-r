# Dados — UNODC Prisons and Prisoners

Arquivos versionados para permitir reprodução imediata do estudo após o clone do repositório.

## Fonte

UNODC, **Prisons and Prisoners**, versão utilizada no projeto com metadado interno de 11/06/2025 (v2).

A camada nacional reportada/coletada foi usada no núcleo inferencial. As estimativas regionais da UNODC foram mantidas separadamente como contexto e verificação.

## Arquivos

- `model_inputs/base_analitica_nucleo_nacional.csv` — base analítica nacional com 7.353 combinações país–ano–sexo.
- `model_inputs/stan_data_M1.json` — entrada congelada de M1 (2.878 observações; 224 países; 2005–2022).
- `model_inputs/stan_data_M2.json` — entrada congelada de M2 (2.162 observações; 193 países; 2005–2022).
- `model_inputs/stan_data_M3.json` — entrada congelada de M3 (1.144 observações; 572 pares; 106 países; 2016–2022).
- `resultados_auditados/` — evidências tabulares necessárias à reprodução documental da síntese final.

## Integridade

SHA-256 dos dados principais versionados:

```text
base nacional      0256390a48d17a4b6445e5d6c7df2aa6ade1c3b3699b72737a92e34f5a00ac5d
stan_data_M1       fa71dd36b0a7002e502673c273be166ac22a726aeb62c56c284eb40a4b6dd8d9
stan_data_M2       09102dde039ad14ded1e4399d26fd50dc149a6db90bb01b66b4d9097d755afa0
stan_data_M3       ae5f0a22dd5544a0398ec192095bade88e0326d5c5f89d61de62e84f2f57692c
```

Os termos oficiais da fonte devem ser consultados antes de redistribuições fora deste contexto de reprodução analítica.