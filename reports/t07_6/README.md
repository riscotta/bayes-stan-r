# T07.6 — Inventário de comparabilidade das saídas adaptadas (execução parcial)

**Objetivo literal:** “Comparar cada saída adaptada com referência histórica, sem prometer igualdade numérica exata sem critério.”

**Snapshot inicial:** `main` `77930248d5fdad81b999bc5741bbb930f4f97858`, 18/11/2026. **Natureza da entrega:** inventário, critérios prévios e gate automatizado documental, **sem comparação histórica material executada**. T07.6 permanece **parcial/pendente de referências autenticadas e de QA independente**; não marcar execução total, QA ou encerramento da Etapa 07.

## 1. Objetos encontrados e distinção de papéis

| Estudo/rota | Artefato do adaptador | Referência histórica pareada encontrada? | Resultado atual |
|---|---|---|---|
| PNAD E10 N2 | `outputs/pnadc_estudo_i/logs/<id>/stdout.txt` | **Não**. Original imprime auditoria em stdout, mas não há snapshot histórico correspondente autenticado na árvore | `NAO_COMPARAVEL` |
| PNAD E10 N2 | `outputs/pnadc_estudo_i/logs/<id>/stderr.txt` | **Não**. Conteúdo de diagnóstico depende de execução/ambiente | `NAO_COMPARAVEL` |
| PNAD E10 N2 | `outputs/pnadc_estudo_i/logs/<id>/report.json` | **Não**. Metadado introduzido pelo wrapper, inclui `id` e `timestamp_utc` novos | `NAO_COMPARAVEL` |
| Mortalidade SP | Nenhum output **do adaptador**: `executar` bloqueado | Não aplicável sem adaptador autorizado e referência individual | `BLOQUEADO_POR_CONTRATO` |
| Cães F5 | Nenhum output **do adaptador**: `executar` bloqueado | Não aplicável sem adaptador autorizado e referência individual | `BLOQUEADO_POR_CONTRATO` |

**Não confundir entrada e saída.** `data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv` é a **entrada pública congelada** auditada pelo script; Git blob `096b9e2d42c8fe1231ac675dc48cfc61dfa294d2`, SHA-256 documentado `8bd38ce75917d981431f4c2a9c09ba73648ff1151fd6ac717068637b682fe120`. Seu hash estabelece identidade técnica do CSV observado, **não** equivalência de `stdout.txt` ou reconstrução histórica E04–E09. Nenhum resultado versionado na pasta `outputs/` é pareável com os três logs: os arquivos `.gitkeep` ali são placeholders. `reports/t06_4/matriz_custodia_T06_4.csv` apresenta exemplos de resultados históricos de estudos, mas esses CSVs não são saídas do adaptador T07.3. Os hashes Git não autenticam por si mesmos a custódia científica.

**Disponibilidade de bytes:** os três caminhos PNAD são templates sob `outputs/.../logs/<id>/`, não artefatos efetivamente capturados e versionados neste PR. Portanto, nenhum SHA-256 de saída, horário de execução concreto ou igualdade de conteúdo é registrado como observado. O código do wrapper prevê esses artefatos e a CI antiga executa a auditoria pública N2, sem produzir um par de referência histórica custodial.

## 2. Critérios de comparação fixados antes de comparar

1. **Elegibilidade e custódia:** mesmo estudo, produto, estimando, população, período, filtros e transformação; autorização de uso; referência independente preexistente, caminho, versão, produtor, data, SHA-256 dos bytes e pin/cadeia de custódia confiável. Evidência do mesmo código ou do mesmo Git não basta para autenticação externa.
2. **Esquema:** mesma unidade, nomes e tipos de colunas, chaves sem duplicatas, conjunto de registros, regra de ordenação, representações de ausente (`NA`, branco, `null`), codificação e separador decimal. Distinguir divergência de arredondamento de alteração de universo e estimando.
3. **Determinísticos:** comparação exata como padrão, `abs=0, rel=0`; desvio somente após protocolo pré-aprovado com justificativa matemática e responsável por QA. Seguir a política T06.4: `|novo−ref| ≤ abs + rel×|ref|`, com limiar não zero obrigatoriamente `PENDENTE_QA`, não PASS automático.
4. **Resumo estocástico:** proibir igualdade ponto a ponto de cadeias. Fixar desenho, seed, priors, likelihood, dados, versão CmdStan/R, MCSE, ESS, R-hat, divergências e critérios por estimando antes de comparar. Nenhuma tolerância numérica específica foi homologada para T07.6.
5. **Logs e metadados:** `stdout.txt` pode ter comparação de bytes apenas se houver original homólogo autenticado e semântica idêntica; `stderr.txt` é diagnóstico, não estimativa; `report.json` compara estrutura/semântica com contrato do próprio wrapper, nunca igualdade histórica com objeto que não existia. Campos `timestamp_utc`/`id` variam deliberadamente.
6. **Gate de decisão:** sem par validado ou sem autorização, resultado **`NAO_COMPARAVEL`**. Ausência não implica divergência, igualdade ou falha do estudo. Só declarar `DIVERGENCIA` depois de comparar dados reais adequados. Para referenciais públicos autorizados, reutilizar o conferidor T06.4 com catálogo e pin externos auditados; o novo verificador T07.6 não o substitui.

## 3. Implementação verificável

- `inventario_pares_T07_6.json`: snapshot rastreável por estudo, entrada, referência de entrada, saída prevista, tipo, critérios e impedimentos.
- `tools/verificar_pares_t07_6.py`: valida o inventário contra o contrato atual do runner e recusa `PASS` inventado, referência inexistente, reutilização da entrada como saída, relaxamento de tolerância ou ativação implícita das rotas R/Stan. **Não abre bases, não executa código científico e não lê logs.**
- `tests/python/test_inventario_t07_6.py`: 14 testes, incluindo mutações adversariais puramente sintéticas do registro. A CI Python executa os testes e o verificador. Os gates prévios T07.1–T07.5 e R/Stan não são alterados.

Comandos:
```sh
python -m unittest discover -s tests/python -p 'test_inventario_t07_6.py' -v
python tools/verificar_pares_t07_6.py
```

## 4. Controle operacional público PNAD — sem referência histórica independente

A rota pública `scripts/pnadc_estudo_i/auditar_e10.py` é somente leitura e emite três linhas determinísticas. Há, portanto, um confronto adicional **executável e mais forte do que o inventário documental**: a CI primeiro executa o adaptador N2 com ID `ci-<GITHUB_RUN_ID>` e, em seguida, `tools/comparar_operacional_pnad_t07_6.py` executa diretamente **o mesmo script original** usando `python -I`, o mesmo CSV público congelado, o mesmo interpretador e ambiente mínimo. O comparador exige integridade do original conforme manifesto T07.2, SHA-256 de entrada/código/manifesto, `report.json` com contrato `T07.3/v1`, estado de sucesso e caminho canônico, e realiza cotejo **byte a byte** de `stdout.txt` e `stderr.txt`. Faz pós-checagem de hashes e preservação dos artefatos. Qualquer divergência ou ausência bloqueia; não aceita tolerância silenciosa.

O teste `tests/python/test_comparacao_operacional_t07_6.py` contém **12 cenários sintéticos** com execução direta real de minisscript sentinela, incluindo adulteração de stdout/stderr, SHA, entrada, original, metadados e symlink. O inventário adiciona **3 mutações** vedando elevar esse confronto a baseline histórica. São, portanto, 29 testes T07.6 (17 de inventário, 12 operacionais) além dos gates já existentes. Não guardar `stdout.txt` ou um novo CSV no Git; saída dinâmica permanece em `outputs/` no checkout da CI.

**Limite epistemológico:** o controle direto é feito com **o mesmo código e os mesmos dados** durante o mesmo workflow. Sua identidade de stdout/stderr demonstra apenas que o adaptador não mudou o texto produzido nessa execução controlada. Não demonstra que a saída seja igual a um resultado histórico **preexistente e independentemente custodiado**, nem valida a integridade da cadeia E04→E09, inferência, licenciamento, generalização, N3/N4 ou segurança como sandbox. `report.json` foi criado pelo adaptador e só permite conferência de contrato, sem par histórico. `stdout` direto e adaptado dependem da mesma lógica de auditoria e, portanto, não são oráculos científicos independentes.

Comandos da verificação operacional, a executar **apenas após o adaptador autorizado no mesmo checkout efêmero**:

```bash
python -m unittest discover -s tests/python -p 'test_comparacao_operacional_t07_6.py' -v
python tools/comparar_operacional_pnad_t07_6.py --id "ci-${GITHUB_RUN_ID}"
```

**Resultado:** a condição `IGUALDADE_OPERACIONAL_N2` só pode ser anotada após a CI passar **no SHA final**; permanece `NAO_COMPARAVEL` para a finalidade literal de equivalência histórica. A ausência de baseline externa não é contornada com execução repetida do mesmo código.

## 4. Limitações e próximos gates

**Ainda não executado:** comparação de bytes contra baseline histórica independente, comparação de CSVs históricos pareados, verificação de esquema de pares reais, confronto numérico, validação externa de pin, E04–E09, execução de R/Stan legado, sampling, N3/N4 ou homologação científica/institucional. Restrições: P0 [issue #14](https://github.com/riscotta/bayes-stan-r/issues/14) aberta, [#15](https://github.com/riscotta/bayes-stan-r/issues/15), [#16](https://github.com/riscotta/bayes-stan-r/issues/16); runner não é sandbox, TOCTOU residual e persistência de logs dependente do armazenamento.

**Para tornar T07.6 integralmente executada:** fornecer ou localizar, sob permissão e custódia independentes, pares históricos realmente homólogos (ou decidir formalmente que a comparação é não aplicável aos metadados exclusivos do wrapper). Pré-registrar o protocolo por par, executar comparação segura, registrar hashes/proveniência/diferenças e obter QA independente no HEAD. A ausência atual é um bloqueio da **comparação material**, não uma justificativa para fabricar uma referência. O próprio executor não aprova sua entrega, não faz squash e não inicia T07.7.
