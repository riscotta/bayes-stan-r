# Mortalidade de 0–74 anos — São Paulo (2006–2023)

Estudo exploratório de 96 distritos e 18 anos (2006–2023) com recorte de 0–74 anos, organizado por sexo e grupos quinquenais. A origem é a integração provisória APVP/PRO-AIM (contagens de óbitos) × SEADE (população). **A equivalência dos limites distritais históricos não foi homologada.** Os resultados não são estatísticas oficiais nem inferências causais.

## O que está neste repositório

- `mortalidade_sp_0_74.R`: execução padrão que confere as tabelas E09 congeladas, gera resumos e dois gráficos em `outputs/mortalidade_sp_0_74/`. **Não recalcula os indicadores desde a base celular.**
- `E09_analise_completa.R`: código R original adaptado para recalcular as taxas, decomposição, análise de sensibilidade e quatro gráficos a partir da base E04 provisória e das saídas R E07/E08. **Código não reexecutado nesta publicação**.
- `data/raw/mortalidade_sp_0_74/resultados_auditados/`: tabelas agregadas auditadas (medidas descritivas anuais, decomposição e validação NB2). Não contém dados individualizados nem a base celular completa.
- `reports/mortalidade_sp_0_74/relatorio_cientifico.md`: interpretação e limites.

A **padronização** é feita em 30 estratos (15 faixas etárias × dois sexos), usando pesos internos da população de 2019. A **decomposição simétrica** da taxa bruta separa algebricamente mudanças das taxas específicas e da composição populacional, sem atribuição causal.

## Execução padrão (sem dados originais)

Da raiz do repositório:

```bash
Rscript --vanilla scripts/mortalidade_sp_0_74/mortalidade_sp_0_74.R
```

Requer apenas R base. Escreve tabelas/figuras regeneráveis sob `outputs/mortalidade_sp_0_74/`. Esta execução **confere e apresenta artefatos agregados já publicados**; não valida a integração original.

## Recálculo da E09 a partir da base derivada (dependente de arquivos externos)

Coloque, **sem publicar no Git**, em `data/raw/mortalidade_sp_0_74/`:
1. `base_analitica_celular_PROVISORIA.csv` (51.840 células, derivada E04);
2. `E07_R_validacao_anual.csv` (resultados de validação dos ajustes originais em R);
3. `E08_incerteza_cobertura_bootstrap_distrito.csv` (bootstrap distrital da E08; também há uma cópia agregada congelada em `resultados_auditados/`).

Execute:

```bash
Rscript --vanilla scripts/mortalidade_sp_0_74/E09_analise_completa.R
```

Os scripts leem os arquivos sem sobrescrevê-los. A saída regenerável usa `outputs/mortalidade_sp_0_74/tables/` e `figures/`. Não confunda a reexecução da E09 com a reconstrução desde os três CSVs primários, nem com a homologação territorial.

## Evidências e escopo

Verificado no estudo anterior: integridade da base derivada, reprodução da E08 (10 tabelas) e da E09 (8 tabelas, quatro gráficos) em Python, e resultados R de validação E07 recebidos. **Não verificado nesta publicação:** execução do código R adaptado do repositório, paridade completa R/Python e refazimento da E04 desde dados primários.

Resultados de referência (óbitos/100 mil): 2019 taxa bruta/padronizada 383,89/383,89; 2021 559,07/536,57; 2023 394,67/364,64. NB2 com treinamento até 2019: 61.166 observados em 2021 frente a 42.279,93 previstos. O desvio preditivo **não estima mortes causalmente atribuíveis** à pandemia.

## Proveniência e redistribuição

As fontes primárias APVP/SEADE, correspondência nominal dos 96 distritos e documentação sobre limites históricos não acompanham o repositório. Não foi verificada uma licença de redistribuição para as tabelas originais/derivadas completas. Publicamos **somente indicadores agregados e autorais**, com identificação explícita da natureza provisória e sem presumir que a licença MIT do código cobre os dados.

Reconstituir E04 exige as fontes `obitos_APVP_2006_2023_0a74.csv`, `populacao_SEADE_2006_2023_0a74_LONG.csv` e `crosswalk_distritos_PROVISORIO.csv`, além de documentação territorial histórica. Não usar `codigo_seade = 80000 + codigo_apvp` como regra de junção: falha em 60 distritos na correspondência original.

O modelo estatístico usa Poisson e binomial negativa em **R (MASS)**; não há modelo Stan neste estudo.
