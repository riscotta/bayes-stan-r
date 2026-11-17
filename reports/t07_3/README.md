# T07.3 — Adaptadores canônicos sem modificar os originais

**Tarefa:** criar wrappers/adaptadores canônicos em diretório separado, com parâmetros e saídas padronizados.
**Base do trabalho:** `1ceb1b35a7696a2a1d071cc6bd8da4f4f2dd43c2` (PR #59 T07.2).

## Interface

Executar da raiz do repositório, com Python 3.12.8:

```bash
python adapters/t07_3/runner.py --estudo pnadc_estudo_i --modo auditoria_rapida_N2 --acao planejar --id exemplo1
python adapters/t07_3/runner.py --estudo pnadc_estudo_i --modo auditoria_rapida_N2 --acao executar --id exemplo1
```

`--estudo`: `pnadc_estudo_i`, `mortalidade_sp_0_74`, `caes_raca_f5`.
`--modo`: estritamente `auditoria_rapida_N2`; qualquer outro modo é recusado.
`--acao`: `planejar` devolve JSON apenas em stdout, sem gravar; `executar` só é permitido para `pnadc_estudo_i`.
`--id`: identificador determinístico de 1–64 caracteres minúsculos [a-z0-9_-], sem separadores. Uma execução com ID repetido é recusada, inclusive após falha parcial.

**Saída da execução permitida:** `outputs/pnadc_estudo_i/logs/<id>/stdout.txt`, `stderr.txt`, `report.json`, incluindo estado, código de retorno, comando, SHA-256 do script e entrada e timestamp UTC. Em falhas posteriores à criação do diretório, preserva `report.json` com estado de erro. A falta de capacidade para criar o registro antes da execução é bloqueante.

## Matriz de adapters

| Estudo | Original | Interface de planejamento | Execução | Justificativa |
|---|---|---|---|---|
| PNAD Estudo I | `scripts/pnadc_estudo_i/auditar_e10.py` | Sim | **N2 público somente** | Auditoria de CSV congelado por hash, biblioteca padrão, sem Stan/ETL |
| Mortalidade SP | `scripts/mortalidade_sp_0_74/reproduzir_E10.R` | Sim | **Bloqueado** | Original em modo auditoria cria/grava outputs fixos; T07.4 deve tratar isolamento e sobrescrita |
| Cães F5 | `scripts/caes_raca_f5/caes_raca_f5_cmdstanr.R` | Sim | **Bloqueado** | Original grava tabelas/logs fixos e tem rota opcional de 30 ajustes Stan |

**Segurança:** O runner não altera scripts/, os 79 arquivos da baseline, configurações/modelos, insumos, parâmetros de inferência, dados institucionais ou resultados históricos. Para execução, exige código original regular com SHA-256 igual ao manifesto T07.2 e entrada pública local existente; recusa symlink de script, de saída, IDs fora do contrato e destinos existentes. Invoca o Python histórico com `-I`, `cwd` na raiz, ambiente mínimo, sem shell e com timeout de 30 segundos. Não permite escolha arbitrária de código, comando, diretório de destino ou entrada. O `--check-baseline` completo continua obrigatório na CI; hash sozinho não demonstra segurança comportamental.

**Limitações:** A auditoria da PNAD usa `assert` internos do original, executados sem `-O`. A execução produz **auditoria descritiva** da síntese congelada, não estimativas novas, equivalência numérica geral, calibração, teste independente N3/N4 nem prova de não haver qualquer efeito colateral no sistema. O wrapper é específico, não uma sandbox de execução hostil. O suporte real aos demais históricos dependerá de T07.4; testes de contrato/smoke ampliados (T07.5), comparação de referência (T07.6) e migração integral (T07.7) continuam pendentes.

## Testes e gates

```bash
python -m unittest discover -s tests/python -p 'test_adaptadores_t07_3.py' -v
python tools/verificar_originais_t07_2.py --check-baseline
```

Os testes de contrato criam **fixtures não científicas** em diretórios temporários para verificar recusa de path traversal, IDs malformados, execução repetida, symlink, código adulterado, entrada faltante e retorno de falha. Na CI, o gate T07.2 continua verificando os 79 códigos e uma invocação direta do runner testa a rota pública PNAD real. CI verde não equivale a QA independente. Manter a issue #14/P0 e as issues #15/#16 abertas.
