# Testes e validação

O repositório reúne estudos independentes, não um pacote R. Executar a partir da raiz:

§§§bash
Rscript tests/run_tests.R
§§§

## Cobertura efetiva

- **Sintaxe R:** o workflow [R CI](../.github/workflows/ci.yml) faz §parse()§ dos scripts R e dos testes.
- **Contratos de estudos e dados:** §tests/testthat/§ verifica arquivos, colunas, referências e convenções.
- **Integridade de links relativos:** §tests/testthat/test-markdown-links.R§ examina destinos Markdown nos documentos do repositório, ignorando blocos de código e URLs HTTP. Não resolve redirecionamentos externos ou links mencionados apenas como texto.
- **Invariantes numéricos publicados:** testes confrontam identidades de taxas e decomposição do estudo de mortalidade SP, e verificam ganhos/folds/intervalos do estudo de cães F5. São verificações de *agregados já preservados*, não reestimações.
- **Auditoria pública de São Paulo:** a CI executa o pipeline §--modo=auditar§, que recompõe e confere indicadores congelados. Não recalcula dados primários.
- **Links externos:** o workflow [External links](../.github/workflows/external-links.yml) roda semanalmente ou manualmente, sem impedir a CI dos PRs. Apenas 404/410 confirmados são falhas; bloqueios HTTP, timeout e rate limits são inconclusivos.

## Fora da cobertura automática

- Instalação integral do §renv.lock§ e compilação/amostragem dos modelos Stan.
- Recuperação de parâmetros com simulações, SBC, PPC completos e validação preditiva em novos dados para todos os estudos.
- Reconstituição a partir de bases privadas, externas ou cuja redistribuição não está confirmada.
- Verificação das licenças de terceiros.

Não chamar a aprovação de um teste estrutural de “reprodução integral”. Para uma publicação científica, executar as rotinas estatísticas apropriadas por estudo, preservar sementes, dados de entrada, versões, diagnósticos e comparação de resultados.
