# Testes e validação

O repositório reúne estudos independentes, não um pacote R. Executar a partir da raiz:

```bash
Rscript tests/run_tests.R
```

## Cobertura efetiva

- **CI padrão:** o workflow [R CI](../.github/workflows/ci.yml) usa Python 3.12.8, Stan 2.40.0 (sintaxe representativa e geração de C++, sem compilação nativa ou sampling nesse job) e R 4.5.1. O job R depende dos dois anteriores e faz `parse()` dos scripts R e dos testes.
- **Contratos de estudos e dados:** `tests/testthat/` verifica arquivos, colunas, referências e convenções.
- **Integridade de links relativos:** `tests/testthat/test-markdown-links.R` examina destinos Markdown nos documentos do repositório, ignorando blocos de código e URLs HTTP. Não resolve redirecionamentos externos ou links mencionados apenas como texto.
- **Invariantes numéricos publicados:** testes confrontam identidades de taxas e decomposição do estudo de mortalidade SP, e verificam ganhos/folds/intervalos do estudo de cães F5. São verificações de *agregados já preservados*, não reestimações.
- **Auditoria pública de São Paulo:** a CI executa o pipeline `--modo=auditar`, que recompõe e confere indicadores congelados. Não recalcula dados primários.
- **Cães F5:** o piloto T06.6 verifica quatro agregados públicos, 30/30 cotejos com tolerância documentada. Não reamostra Stan. Ver [T06.6](../reports/t06_6/README.md).
- **Stan científico separado:** os workflows [T05.4](../.github/workflows/stan-smoke-t05-4.yml) e [T05.5](../.github/workflows/stan-t05-5.yml) tratam de compilação nativa e verificações em dois modelos sintéticos sob seus protocolos. Não são o job sintático da CI padrão nem validação de todos os estudos.
- **Links externos:** o workflow [External links](../.github/workflows/external-links.yml) roda semanalmente ou manualmente, sem impedir a CI dos PRs. Apenas 404/410 confirmados são falhas; bloqueios HTTP, timeout e rate limits são inconclusivos.

## Fora da cobertura automática

- Instalação integral do `renv.lock` e compilação nativa/amostragem de todos os modelos Stan na CI padrão.
- Recuperação de parâmetros com simulações, SBC, PPC completos e validação preditiva em novos dados para todos os estudos.
- Reconstituição a partir de bases privadas, externas ou cuja redistribuição não está confirmada.
- Verificação das licenças de terceiros.

Não chamar a aprovação de um teste estrutural de “reprodução integral”. Para uma publicação científica, executar as rotinas estatísticas apropriadas por estudo, preservar sementes, dados de entrada, versões, diagnósticos e comparação de resultados.

- **Limites científicos publicados:** `test-scientific-published-diagnostics.R` verifica concordância de resumos IOM e alertas sobre PSIS-LOO, R-hat e SBC do estudo UNODC. O workflow publica [um relatório de ressalvas](../reports/SCIENTIFIC_VALIDITY_GATE_2026-10-28.md). **Não reexecuta Stan.**

- **PNAD Contínua, Estudo I:** teste estrutural R em `tests/testthat/test-pnadc-estudo-i.R`; job Python obrigatório da CI executa os testes públicos N2, o adaptador autorizado, a comparação byte a byte de stdout/stderr contra chamada direta contemporânea e `.github/scripts/executar_auditoria_n2.py`. Também é possível executar `python scripts/pnadc_estudo_i/auditar_e10.py` para auditar a síntese congelada. Não refaz E04–E09, Stan ou comparação com baseline histórica independente. Ver [T07.6](../reports/t07_6/README.md) e [T10.2](../reports/t10_2/README.md).
