# ISUS / SIA — Monte Carlo robusto (cluster bootstrap por CNES)

Este exemplo roda uma simulação **Monte Carlo robusta** (cluster bootstrap por **CNES**) para apoiar **precificação** (ex.: SIGTAP), lidando com **caudas pesadas** e **procedimentos caros/raros**.

- **Script:** `scripts/isus_sia/mc_isus_sia.R`
- **Dados (raw):** `data/raw/isus_sia/ISUS_SIA_PARS.zip` *(contém `ISUS_SIA_PARS.csv`, separador `;`)*
- **Saída:** **apenas console** (não grava arquivos)

## Como rodar

A partir do **root** do repositório:

```bash
Rscript scripts/isus_sia/mc_isus_sia.R
```

Você pode indicar um CSV com `--csv_path` ou um ZIP com `--zip_path`:

```bash
Rscript scripts/isus_sia/mc_isus_sia.R --csv_path=caminho/para/ISUS_SIA_PARS.csv

Rscript scripts/isus_sia/mc_isus_sia.R --zip_path=caminho/para/ISUS_SIA_PARS.zip
```

A busca pela entrada segue esta ordem:

1. CSV indicado por `--csv_path` ou, sem essa opção, CSV padrão do projeto.
2. CSV de fallback do ambiente, se existir.
3. ZIP indicado por `--zip_path` ou, sem essa opção, ZIP padrão do projeto. Se esse ZIP estiver ausente, o script ainda tenta o ZIP de fallback do ambiente.

Para usar o ZIP indicado, confirme que os dois caminhos prioritários de CSV estão ausentes e que o ZIP contém `ISUS_SIA_PARS.csv` na raiz do arquivo. Um CSV encontrado antes nessa ordem tem precedência sobre o ZIP, mesmo com `--zip_path` explícito.

## Requisitos

- R recente
- Pacote: `data.table`

## Observações importantes

1. **Compliance / LGPD:** antes de versionar este CSV publicamente, confirme que ele **não contém identificadores pessoais** (ou qualquer informação sensível) e que você tem permissão para compartilhamento.
2. **Tamanho do arquivo:** se o CSV crescer (ou se você quiser evitar “inchar” o Git), considere **Git LFS** ou um script de download/ingestão em `scripts/_setup/` + ignorar o arquivo no `.gitignore`.
