# Gate de validade científica — dados publicados em 28/10/2026

**Natureza:** verificação automática dos diagnósticos e indicadores **já publicados**. Não compila, reestima nem valida todos os 17 programas Stan do repositório. A aprovação da CI não deve ser apresentada como validação de todos os modelos.

O comando abaixo gera o resumo com valores calculados diretamente dos arquivos auditados:

```bash
Rscript --vanilla scripts/_setup/report_scientific_gates.R
```

## Limites que permanecem relevantes

| Estudo | Evidência numérica nos CSVs | Consequência de interpretação |
| --- | --- | --- |
| IOM — Missing Migrants | R-hat máximo 1,0144, quatro quantidades acima de 1,01; ESS bulk mínimo ~382; zero divergências | A boa geometria HMC em divergências não substitui convergência uniforme nem PPC; os dados publicados apontam inadequação na reprodução de zeros e dispersão |
| UNODC — M1 | 45 efeitos de país e dois efeitos regionais com R-hat acima de 1,01 | Não apresentar os efeitos locais mal diagnosticados como resultados estáveis |
| UNODC — M1/M2/M3 | PSIS-LOO agrupado por país apresenta Pareto-k alto em todos os modelos | Não afirmar desempenho de predição para países novos a partir dessa avaliação |
| UNODC — comparação | M1/M2/M3 com 2.878, 2.162 e 1.144 observações, respectivamente, na tabela de LOO por observação | **Não comparar ELPD entre universos amostrais distintos** como se fossem a mesma tarefa de predição |
| UNODC — SBC | 20 réplicas por M2 e M3; máximo de R-hat da réplica 1,0587 (M2) e 1,0977 (M3) | Um exercício limitado de recuperação de parâmetros não comprova calibração bayesiana geral |
| Cães F5 | Ganho interno de RMSE na faixa aproximada de 0,85–1,41% | Não interpretar pequena melhora no conjunto disponível como validação externa ou causalidade |

As linhas quantitativas são avaliadas por `tests/testthat/test-scientific-published-diagnostics.R`. O workflow R CI produz um resumo automático; alertas metodológicos existentes **são exibidos como ressalvas**, não ocultados para obter um indicador verde.

## Próximos testes realmente funcionais

1. Selecionar dois modelos Stan representativos com dados sintéticos pequenos.
2. Compilar com versão registrada do CmdStan e verificar dimensões, prior predictive, posterior predictive e recuperação de parâmetros.
3. Conferir R-hat, ESS e divergências após amostragem real; comparar posterior com saídas congeladas e tolerâncias justificadas.
4. Adicionar testes de validação externa apenas quando houver bases independentes e autorização apropriada para executá-las.
5. Documentar a função de perda/tarefa preditiva e a unidade de LOO antes de comparar modelos.

**Não automatizar inferências decisórias só porque uma cadeia concluiu sem divergências.**
