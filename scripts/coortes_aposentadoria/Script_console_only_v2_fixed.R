#!/usr/bin/env Rscript

# Wrapper de compatibilidade.
# Mantém o nome original do arquivo que você vinha usando,
# mas delega a execução para o script no padrão do repo.
#
# Rode a partir do ROOT do repo:
#   Rscript scripts/coortes_aposentadoria/Script_console_only_v2_fixed.R
#
# (As mesmas opções --chave=valor são aceitas.)

source(file.path("scripts", "coortes_aposentadoria", "coortes_aposentadoria_console_only_v2.R"),
       local = new.env())
