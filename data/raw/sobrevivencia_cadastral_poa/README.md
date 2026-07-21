# Dados — sobrevivência cadastral em Porto Alegre

Esta pasta contém apenas insumos agregados e resultados auditáveis do estudo.

## Fontes originais

- Cadastro Nacional da Pessoa Jurídica da Receita Federal, recorte de estabelecimentos em Porto Alegre.
- Dados de alvarás de Porto Alegre usados nas etapas de inventário e auditoria.

A data de referência cadastral do estudo é 14 de junho de 2026. A população analítica inclui estabelecimentos de alimentação fora do lar com coortes de abertura entre 2000 e 2025.

## Conteúdo versionado

- `model_inputs/`: matrizes agregadas, exposições e eventos usados por M0 e M1, em JSON compactado (`.json.gz`).
- `resultados_auditados/`: sínteses posteriores, diagnósticos, LOO, PPC, sensibilidade e conclusões.
- `metadados/`: configuração da amostragem e ambiente da execução original.

## Conteúdo não publicado

A base individual possui CNPJ, endereço e outros campos cadastrais. Ela não é versionada para reduzir exposição desnecessária de registros identificáveis e evitar a redistribuição de arquivos grandes. Os dados agregados publicados preservam a reprodução dos modelos e das conclusões centrais.

## Escopo

Os resultados medem sobrevivência cadastral. Não devem ser interpretados como mortalidade econômica real, efeito causal de características empresariais ou previsão prospectiva validada.
