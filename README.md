# Bayes, R e Stan — estudos aplicados

[![R CI](https://github.com/riscotta/bayes-stan-r/actions/workflows/ci.yml/badge.svg)](https://github.com/riscotta/bayes-stan-r/actions/workflows/ci.yml)
[![Licença MIT](https://img.shields.io/badge/c%C3%B3digo-MIT-blue)](LICENSE)

Ajustar um modelo é apenas parte de uma análise estatística. Este repositório reúne estudos em **R e Stan** com atenção à especificação, às verificações computacionais, à capacidade preditiva e aos limites do que pode ser concluído.

A unidade de organização é o **estudo**, não um pacote R. Cada pasta em §scripts/§ tem entrada e documentação próprias. Alguns estudos refazem o ajuste a partir dos dados disponíveis; outros reconstroem tabelas a partir de artefatos auditados, e alguns dependem de insumos que não podem ser redistribuídos. **Esses níveis de reprodução não são equivalentes.**

## Por onde começar

| Interesse | Estudo | O que observar |
| --- | --- | --- |
| Inferência hierárquica | [Baseball em três níveis](scripts/hierarchical/README.md) | *Pooling* parcial e heterogeneidade por posição/jogador |
| Modelagem e interpretação | [Mortalidade 0–74 em São Paulo](scripts/mortalidade_sp_0_74/README.md) | Padronização por idade e sexo, decomposição e limites territoriais; modelo ajustado em R, não em Stan |
| Avaliação preditiva | [Raça declarada e escore F5 em cães](scripts/caes_raca_f5/README.md) | Ganho pequeno de RMSE e calibração condicional insuficiente |
| Diagnóstico de modelo | [Missing Migrants, IOM](scripts/missing_migrants/README.md) | Modelo NB2 hierárquico, limitações do registro e falhas de PPC |

**[Catálogo completo de scripts, dados e comandos](scripts/README.md)**. Inclui modelos ordinais, sobrevivência, Poisson/NB2, séries temporais, regressão logística, simulações e estudos de saúde, risco, economia e educação.

## Execução

Use a raiz do repositório como diretório de trabalho; não é necessário chamar §setwd()§.

§§§bash
git clone https://github.com/riscotta/bayes-stan-r.git
cd bayes-stan-r

# Restaurar os pacotes R declarados no renv.lock
Rscript -e "if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv'); renv::restore(prompt = FALSE)"

# Exemplo: uma análise concreta
Rscript scripts/therapeutic_touch/therapeutic_touch.R
§§§

Ambiente de referência: **R 4.5.1**, §cmdstanr§ **0.9.0**, §rstan§ **2.32.7** e **CmdStan 2.40.0**. O §renv.lock§ controla pacotes R, não o compilador CmdStan. Para executar estudos com §cmdstanr§:

§§§bash
Rscript scripts/_setup/install_cmdstan.R
§§§

Alternativa para instalar dependências fora do §renv§: §Rscript scripts/_setup/install_deps.R§. Os detalhes estão em [config/environment.yml](config/environment.yml) e [scripts/_setup/README.md](scripts/_setup/README.md).

O comando padrão **não necessariamente reamostra Stan**. Em estudos que disponibilizam §--run_stan=1§, esse parâmetro aciona uma nova amostragem e pode exigir dados externos, compilador e tempo computacional. Consulte o README local antes de interpretá-lo como reprodução integral.

## Organização

| Caminho | Finalidade |
| --- | --- |
| [scripts/](scripts/README.md) | Código R/Stan e instruções por estudo |
| [data/raw/](data/raw/README.md) | Fontes recebidas e insumos históricos selecionados |
| [data/DATA_PROVENANCE.md](data/DATA_PROVENANCE.md) | Origens, licenças e permissões ainda pendentes |
| [outputs/](outputs/README.md) | Arquivos regeneráveis, agrupados em §outputs/<estudo>/<tipo>/§ |
| [reports/](reports/README.md) | Relatórios e cadernos de interpretação |
| [tests/](tests/README.md) | Contratos estruturais e verificações numéricas |

Resultados históricos auditados publicados dentro de §data/raw/<estudo>/resultados_auditados/§ são **exceções de preservação**, não dados brutos nem arquivos produzidos pela execução padrão. Não reorganize esses caminhos sem ajustar código, manifestos, documentação e testes.

## Resultados representativos e ressalvas

**Cães / F5.** Nos 2.084 cães analisados, adicionar raça declarada produziu redução interna de RMSE de **0,85% a 1,41%**, dependendo da família de distribuição. A melhora foi pequena, não foi confirmada em coorte externa e **não** identifica causalidade genética nem risco individual de mordida. [Análise e ressalvas](reports/caes_raca_f5/relatorio_cientifico.md).

**Mortalidade em São Paulo, 2006–2023.** A taxa bruta e a taxa padronizada por idade/sexo levam a conclusões diferentes em alguns intervalos. Os dados distritais usam correspondência territorial provisória; a execução pública **reconstitui indicadores agregados**, não a integração completa dos dados primários. [Relatório científico](reports/mortalidade_sp_0_74/relatorio_cientifico.md) e [caderno E01–E10](reports/mortalidade_sp_0_74/caderno_E01_E10.md).

**Missing Migrants.** A modelagem trata a carga **registrada** de mortos e desaparecidos, não a mortalidade real total. Há ressalvas de §R-hat§ e checagens preditivas dos zeros e da dispersão. [Escopo do estudo](scripts/missing_migrants/README.md).

## O que é validado automaticamente?

O [workflow R CI](.github/workflows/ci.yml) verifica a sintaxe dos scripts R, executa a suíte §testthat§ e uma reconstituição pública dos resultados agregados de mortalidade de São Paulo. Há também uma verificação local de caminhos Markdown e uma rotina separada para URLs externos.

**Passar na CI não comprova a validade estatística de todos os ajustes**, não instala todos os pacotes do §renv.lock§ e não reexecuta todas as cadeias MCMC. Veja [tests/README.md](tests/README.md) para saber o que efetivamente é coberto.

§§§bash
Rscript tests/run_tests.R
§§§

## Dados, licença e contribuição

O código e a documentação autoral estão sob [MIT](LICENSE). **Isso não concede licença sobre dados de terceiros.** O [inventário de proveniência](data/DATA_PROVENANCE.csv) registra arquivos cujas fontes ou permissões de redistribuição ainda não foram homologadas, inclusive algumas bases já versionadas. Consulte esses registros antes de reutilizar qualquer dataset. Dados individualizados não publicados não são necessários para navegar nos exemplos que usam apenas agregados.

Para propor mudanças, consulte [CONTRIBUTING.md](CONTRIBUTING.md). Para citar o código, veja [CITATION.cff](CITATION.cff). O histórico de mudanças editoriais e de infraestrutura está em [CHANGELOG.md](CHANGELOG.md).

Este repositório é um portfólio de estudos e trabalho em evolução; os relatórios devem ser lidos com suas respectivas limitações, e não como validações gerais para decisões operacionais.
