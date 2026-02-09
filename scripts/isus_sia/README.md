# ISUS / SIA — Monte Carlo robusto (cluster bootstrap por CNES)

Este exemplo roda uma simulação **Monte Carlo robusta** (cluster bootstrap por **CNES**) para apoiar **precificação** (ex.: SIGTAP), lidando com **caudas pesadas** e **procedimentos caros/raros**.

- **Script:** `scripts/isus_sia/MC_patched.R`
- **Dados (raw):** `data/raw/isus_sia/ISUS_SIA_PARS.csv` (separador `;`)
- **Saída:** **apenas console** (não grava arquivos)

## Como rodar

A partir do **root** do repositório:

```bash
Rscript scripts/isus_sia/MC_patched.R
```

Opcionalmente, você pode apontar o CSV explicitamente:

```bash
Rscript scripts/isus_sia/MC_patched.R --csv_path=caminho/para/ISUS_SIA_PARS.csv
```

## Requisitos

- R recente
- Pacote: `data.table`

## Observações importantes

1. **Compliance / LGPD:** antes de versionar este CSV publicamente, confirme que ele **não contém identificadores pessoais** (ou qualquer informação sensível) e que você tem permissão para compartilhamento.
2. **Tamanho do arquivo:** se o CSV crescer (ou se você quiser evitar “inchar” o Git), considere **Git LFS** ou um script de download/ingestão em `scripts/_setup/` + ignorar o arquivo no `.gitignore`.
