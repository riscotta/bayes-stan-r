# T07.1 — Inventário de caminhos legados, escritas e chamadas de shell

**Tarefa literal:** “Enumerar scripts que usam caminhos legados, diretórios locais, gravações implícitas ou comandos de shell.”

**Base inspecionada:** `main` em `4716ef3eadebe1a5ad8d8ec2e7802ecceb6a7902` (16/11/2026). **Estado:** execução documental em branch, **QA independente pendente** até revisão sobre o HEAD exato. Escopo da Etapa 07: integrar scripts históricos sem alterar os originais. **Nenhuma rotina inferencial ou dado institucional foi executado** nesta tarefa; não há novos wrappers.

## Cobertura e classificação

O [inventário CSV](inventario_scripts_T07_1.csv) enumera **59/59 scripts executáveis** em `scripts/` com extensões `.R`, `.py` ou `.ps1`, sem excluir os dois PowerShell históricos. Os indícios são **categorias não exclusivas** extraídas por leitura estática e triagem manual:

| Categoria | Scripts | O que efetivamente indica |
|---|---:|---|
| `LEITURA` | 49 | Chamada ou passagem de caminho de leitura, fonte, importação ou teste de existência; não necessariamente arquivo ausente |
| `ESCRITA` | 41 | Diretório, arquivo, saída de modelo ou exportação potencialmente gravável; não implica sobrescrita comprovada |
| `SHELL` | 4 | Chamada de processo externo ou PowerShell para Rscript; observar ambiente, dependências e efeitos |
| `LEGADO` | 9 | Caminhos de entrada/saída históricos que merecem migração/isolamento; não implicam falha atual |
| `ABSOLUTO` | 1 | Uso literal de um caminho absoluto dependente do ambiente |
| `SEM_SINAL_REGEX` | 5 | Não houve correspondência relevante na triagem lexical; **não significa ausência de efeitos** |

As categorias se sobrepõem e não podem ser somadas para inferir o número de scripts afetados. Os números são **triagem estática com falso positivo e falso negativo possíveis**, e não resultados de execução. A coluna `linha_exemplo` localiza um trecho concreto; quando há várias categorias, não necessariamente comprova todas elas na mesma linha. O CSV apresenta um registro por script, inclusive os sem correspondência lexical.

**Escopo excluído da contagem:** código Stan `.stan` (não controla os caminhos de sistema da mesma forma), testes fora de `scripts/`, arquivos de configuração, workflows e arquivos de dados/documentação. Isso não afirma inexistência de riscos nesses arquivos. Em particular, o instalador `scripts/_setup/install_cmdstan.R` está marcado `SEM_SINAL_REGEX`, mas invoca `cmdstanr::install_cmdstan` na linha 39; **a ausência de correspondência não torna esse script sem efeito**.

## Casos prioritários com evidência de código

| Script / localização | Evidência e implicação | Prioridade de inspeção |
|---|---|---|
| `scripts/isus_sia/mc_isus_sia.R:94–98` | Fallback literal `/mnt/data/ISUS_SIA_PARS.csv` ou `.zip`; depende da montagem local, e fonte está sujeita ao gate institucional | **Alta**, portabilidade e dado potencialmente restrito |
| `scripts/pnadc_estudo_i/estudo_I/e04/construir_base_e04.py:19–21` | Entrada `dados/04_series_csv/...` e `SAIDA` sob `e04/saida`; criação automática e posterior gravação fora de `outputs/<estudo>/<tipo>/` | **Alta**, entrada local e gravação histórica |
| `scripts/pnadc_estudo_i/estudo_I/e05/explorar_e05.py:19–24` | Consome `e04/saida` e produz `e05/saida`; encadeamento por caminhos históricos | **Alta**, dependência entre etapas |
| `scripts/pnadc_estudo_i/estudo_I/e08/diagnostico_e081.py:8,45` | Lê base E04 local e cria pasta E08 histórica | **Alta**, preservação e não sobrescrita |
| `scripts/pnadc_estudo_i/estudo_I/e09/consolidar_e09.py:5–7,46` | Exporta `contrastes_e09_consolidado.csv` dentro do próprio diretório `e09` | **Alta**, escrita na árvore histórica |
| `scripts/caes_raca_f5/original/E08_MP_Entregaveis/02_codigo/EXECUTAR_E08_MP.ps1:11–22` | Cria `07_retornos/...` e aciona `Start-Process` para Rscript; o runner pode produzir arquivos e ZIPs | **Alta**, execução e artefatos fora da convenção |
| `scripts/caes_raca_f5/original/E08_MP_Entregaveis/02_codigo/executar_E08_MP.R:16–18` | Usa `07_retornos` e `04_dados_imutaveis` relativos à estrutura original; cria diretório de saída | **Alta**, custódia e pré-condições |
| `scripts/pnadc_estudo_i/estudo_I/e07/run.ps1:9,56` | Descobre Rscript.exe no Windows, respeita `RSCRIPT_EXE` e executa `& $Rscript` com modos preflight/smoke/completo/resumo | **Média/alta**, dependência de PowerShell/R |
| `scripts/mortalidade_sp_0_74/reproduzir_E10.R:18` | Emprega `system2` ao delegar a Rscript; modo `completo` depende de entrada E04 local e não é auditoria N2 | **Média/alta**, executar somente após gate |
| `scripts/censo_escolar_tempo_integral/censo_escolar_tempo_integral_cmdstanr.R:241–302,347` | Gerencia cache/ZIP, pode apagar o destino com `force_download` e invoca `system2`; URLs/termos e hash externos não homologados | **Alta**, acesso remoto e substituição de cache |

**Contraponto:** vários scripts direcionam suas saídas a `outputs/<estudo>/<tipo>/` (por exemplo, wrapper Cães F5, importações e relatórios), mas essa convenção no código **não certifica isolamento de todas as escritas internas**. `LEGADO` é um marcador de migração, não um diagnóstico de defeito inferencial.

## Falso positivo, falso negativo e prioridade

A string `"saida"` usada como **valor de dado** no script Sherlock não é um caminho legado; não recebe a classificação `LEGADO`. Documentação inicial, comentários e textos de ajuda podem conter palavras como `source`, `output` e `Rscript` sem que sejam ações executáveis. A leitura estática não resolve funções indiretas, `do.call`, funções auxiliares, parametrização, `here::here`, aliases, callbacks, caminhos construídos dinamicamente, escritas internas de bibliotecas nem condições de execução. Por isso `SEM_SINAL_REGEX` **não é sinônimo de seguro**, e `ESCRITA` **não é sinônimo de sobrescrita existente**.

Antes de reexecutar qualquer script histórico, identificar explicitamente caminho de **entrada**, origem/licença/vintage/hash dos insumos quando exigidos, ambiente, permissões, diretório e consequências de **escrita**, preservação de arquivos originais, modo operacional T06.3 e critério de comparação T06.4. Respeitar `outputs/README.md`. A próxima tarefa **T07.2** deverá formalizar a imutabilidade dos originais com manifesto de hash; T07.3 tratará wrappers em diretório separado.

## Verificação reprodutível

Da raiz do checkout da branch:

```bash
python -m unittest discover -s tests/python -p 'test_inventario_t07_1.py' -v
```

O teste garante (i) cobertura exata dos 59 scripts dentro do escopo, (ii) caminhos sem duplicações e categorias válidas, (iii) linhas existentes e casos adversariais prioritários, (iv) marcadores reais de shell e caminho absoluto, (v) não classificação de um falso positivo de `saida`, e (vi) mutações adversariais de integridade do CSV. **Não executa nenhum dos scripts de pesquisa**, não baixa dados, não testa equivalência numérica, não produz N3/N4 e não certifica validade estatística. A CI no SHA final e a revisão por outro chat são gates posteriores.

## Dependências e travas

Reconciliar com [manifesto T06.1](../t06_1/manifesto_estudos_T06_1.json), [modos T06.3](../t06_3/README.md), [inventário T06.5](../t06_5/README.md) e [convenção de outputs](../../outputs/README.md). A [issue #14 P0](https://github.com/riscotta/bayes-stan-r/issues/14) de direitos, custódia e dados, mais as issues [#15](https://github.com/riscotta/bayes-stan-r/issues/15) e [#16](https://github.com/riscotta/bayes-stan-r/issues/16), permanecem abertas. Nenhuma permissão institucional ou direito de redistribuição é inferido da existência dos arquivos. `N3/N4` não foram atestados.

**Estado de encerramento desta tarefa:** até parecer técnico-científico independente favorável no SHA exato, checks verdes e squash autorizado, marcar somente **T07.1 Exec. ☒ / QA ☐ / Etapa 07 ☐**. Não iniciar T07.2 automaticamente no mesmo ciclo.
