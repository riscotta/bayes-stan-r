# T07.5 — Contratos e smoke tests dos adaptadores canônicos

**Tarefa literal:** “Adicionar testes de contrato e smoke test para os novos wrappers.”

**Escopo:** testes adicionais para `adapters/t07_3/runner.py`, sem modificar o runner, os 79 códigos originais, o manifesto T07.2, insumos, modelos, priors, likelihoods ou resultados científicos. O runner permanece com contrato `T07.3/v1`; nenhum caminho R/Stan foi habilitado.

## Matriz de aceitação

| Contrato | Evidência automatizada |
|---|---|
| `--estudo`, `--modo`, `--acao`, `--id` obrigatórios, conjuntos fechados | Help, parâmetros ausentes, estudo/modo inválidos, IDs canônicos e malformados |
| `planejar` para PNAD, Mortalidade SP e Cães F5 | JSON da interface: versão `T07.3/v1`, caminhos, modo, autorização, justificativa e nenhum arquivo criado |
| Execução autorizada limitada a PNAD E10 N2 | Smoke subprocesso CLI real `python -I`, com mini-script e CSV de **fixture sintética** em raiz temporária |
| Registros canônicos | `outputs/pnadc_estudo_i/logs/<id>/{stdout.txt,stderr.txt,report.json}`; estado `EXECUTED_N2_CHECK`, hashes, comando, timestamp, retorno |
| Recusa de R/Stan históricos | `executar` em `mortalidade_sp_0_74` e `caes_raca_f5` retorna 2 sem criação de `outputs/` |
| Entradas e originais íntegros | Hashes iguais antes/depois no smoke; falha pré-reserva por manifesto adulterado/ausente e entrada ausente |
| Não sobrescrita e falhas | ID repetido conserva bytes; diretório preexistente, symlink de saída e retorno não zero são tratados sem sobrescrever artefatos existentes |
| Convenção de saídas | Estudo e tipo de output selecionados de allowlists; rejeição de caminhos não canônicos |

## Execução

Na raiz do repositório:

```bash
python -m unittest discover -s tests/python -p 'test_contratos_t07_5.py' -v
python -m unittest discover -s tests/python -p 'test_adaptadores_t07_3.py' -v
python -m unittest discover -s tests/python -p 'test_saida_t07_4.py' -v
```

A CI Python 3.12.8 executa os 14 contratos T07.5 antes da execução pública PNAD N2 já existente. Os jobs Stan (apenas sintaxe representativa) e R (sintaxe, testes e auditorias públicas já autorizadas) permanecem como gates existentes; não introduzimos sampling Stan ou novas execuções históricas no job Python.

Os testes de T07.5 **não utilizam CSV ou dados históricos do repositório**: copiam somente o `runner.py` para um checkout temporário, geram um script de impressão de sentinela, um CSV sintético e um manifesto de hash correspondente. A execução é feita por processo real da CLI em `-I` com raiz isolada na fixture, não por mock do subprocesso. A fixture não fornece evidência científica sobre a PNAD.

## Limites e aceite

O runner T07.3/T07.4 não é sandbox: mesmo com Python `-I`, o processo mantém privilégios do usuário, e validações de caminho/hash têm janelas TOCTOU. O smoke só verifica o contrato em fixture controlada; não prova ausência de efeitos colaterais em código hostil, segurança externa, N3/N4, reamostragem, equivalência de saídas históricas (T07.6) ou migração completa (T07.7). Falha de armazenamento pode impedir `report.json`.

T07.5 **Execução** pode ser marcada apenas após testes e evidências de CI no SHA final. **QA** fica pendente até parecer técnico independente de outro chat no SHA correto; a PR deve permanecer sem merge até aquele parecer e os checks exigidos. A Etapa 07 continua aberta. A issue #14/P0 (proveniência/licenças) e as issues #15/#16 permanecem abertas.
