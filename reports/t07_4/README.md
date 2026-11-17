# T07.4 — Não sobrescrita e convenção de saídas canônicas

**Tarefa literal:** “Garantir que entradas não sejam sobrescritas e que saídas regeneráveis permaneçam em `outputs/<estudo>/<tipo>/`.”

**Escopo da alteração:** contrato operacional do adaptador canônico T07.3, restrito à auditoria pública PNAD E10 N2. O código científico original não é modificado. As rotas de Mortalidade SP e Cães F5 permanecem somente em `planejar`; **nenhum R/Stan histórico é habilitado**. Não se altera modelo, prior, likelihood, estimando, dados, resultados históricos ou manifesto T07.2.

## Contrato implementado

- Os tipos permitidos seguem `outputs/README.md`: `figures`, `tables`, `models`, `logs`, `cmdstan_csv` e `data_stan`. Estudos permitidos são identificados por `STUDIES` do adaptador. IDs de execução são validados para evitar separadores de caminho, traversal e nomes não canônicos.
- A saída do único fluxo executável é `outputs/pnadc_estudo_i/logs/<id>/`. A ação `planejar` não cria diretórios nem arquivos. O adaptador reserva o diretório de uma execução nova com `mkdir(exist_ok=False)`; IDs antigos, mesmo com execução fracassada, não podem ser reutilizados.
- `stdout.txt`, `stderr.txt` e `report.json` são criados com `O_CREAT|O_EXCL` (e `O_NOFOLLOW` quando suportado). Arquivo pré-existente, inclusive symlink, não é substituído. Arquivos históricos fora do novo diretório não são escritos pelo código do adaptador.
- Fonte histórica, manifesto de integridade T07.2 e insumo N2 são inspecionados como arquivos regulares, sem symlinks em seus ancestrais. Fonte é comparada ao SHA-256 do manifesto. Código e insumo são novamente verificados depois do subprocesso; divergência gera estado `FAILED_INTEGRITY` e retorno de erro, com evidência preservada se o armazenamento estiver disponível.
- `report.json` persiste tanto sucesso quanto erro quando o sistema de arquivos permite; uma falha de gravação é explicitamente sinalizada. **Não há reescrita ou recuperação automática de entrada alterada.**

## Matriz de rotas e efeitos

| Rota | Origem e efeitos relevantes | Tratamento T07.4 |
|---|---|---|
| PNAD E10 N2 | `scripts/pnadc_estudo_i/auditar_e10.py`: lê CSV congelado e imprime auditoria descritiva | Executável com hash do código, controles de entrada e logs exclusivos no padrão canônico |
| PNAD E04–E09 | `estudo_I/e04`, `e05`, `e07`, `e08`, `e09`: gravações históricas, incluindo E07 `gravar_csv` → `write.table` | **Não executar pelo adaptador**; nenhuma migração automática; preservar histórico e entradas |
| Mortalidade SP | `reproduzir_E10.R`, com saídas históricas/fixas em certas rotas e dependências em R | `planejar` apenas; execução rejeitada antes de reservar qualquer saída |
| Cães F5 | `caes_raca_f5_cmdstanr.R` e `original/E08_MP_Entregaveis`: escrita em diretórios históricos, cópias e eventual Stan | `planejar` apenas; execução rejeitada antes de reservar qualquer saída |
| Outros scripts catalogados | Inventário T07.1: 51/59 com indícios de escrita e 9 com caminhos legados; há `file.copy(overwrite=TRUE)`, `unlink`, instalações e caches | **Fora do adaptador T07.4**; nenhuma certificação global de ausência de efeitos, nem autorização para execução |

## Provas e limitações

Executar na raiz do checkout:

```bash
python -m unittest discover -s tests/python -p 'test_adaptadores_t07_3.py' -v
python -m unittest discover -s tests/python -p 'test_saida_t07_4.py' -v
```

Os testes usam somente diretórios temporários e dados sintéticos, e incluem IDs repetidos, tipos inválidos, caminhos de travessia, artefato já presente, symlink para sentinela, falha de armazenamento, alteração simulada de fonte/insumo, ancestral de manifesto redirecionado, planejamento sem escrita e rotas R/Stan bloqueadas. A CI exige ambos os conjuntos antes da execução pública N2 já existente. A verificação completa do manifesto T07.2 e a CI Python/Stan/R seguem obrigatórias.

**Limites:** não há sandbox de sistema operacional nem isolamento para código hostil. Checagens de caminho e hash têm janelas TOCTOU; um subprocesso com privilégios de escrita pode atingir caminhos externos, e a verificação posterior **detecta** mas não previne ou reverte uma mutação. A garantia operacional de não sobrescrita aplica-se às **escritas realizadas pelo próprio adaptador** via criação exclusiva e à execução revisada do único script PNAD de leitura. Não estender a garantia a scripts históricos não executados. Falha de disco pode impedir `report.json`. Não há validação científica N3/N4, reamostragem ou prova de equivalência histórica. A issue [#14/P0](https://github.com/riscotta/bayes-stan-r/issues/14) de proveniência/licenças e as issues #15/#16 continuam abertas.

**Gate:** T07.4 só pode receber QA ☒ após revisão independente favorável no SHA final, CI no mesmo SHA e integração autorizada. Etapa 07 depende de T07.5–T07.7 e permanece aberta.
