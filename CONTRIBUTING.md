# Contribuição e critérios de revisão

Este repositório reúne estudos, não um pacote R. Contribuições devem preservar os scripts reproduzíveis e **não alterar estimativas científicas sem justificativa, comparação e documentação**.

## Fluxo recomendado

1. Abra uma issue com o problema, os arquivos envolvidos e uma proposta de validação.
2. Trabalhe em uma branch; evite commits diretos em §main§.
3. Execute §Rscript tests/run_tests.R§ a partir da raiz. A pipeline também verificará sintaxe R e a reconstituição pública do estudo de mortalidade SP.
4. Documente qualquer dependência externa, contrato de dados, semente aleatória, versão de R/Stan, modo de execução e limite de reprodutibilidade.
5. No PR, separe **mudança técnica**, **mudança nos resultados** e **risco residual**.

Não publique dados pessoais, bases institucionais de distribuição incerta, credenciais nem arquivos produzidos por cadeias MCMC sem revisão de segurança, licença e tamanho.

## Convenções

- §scripts/<estudo>/§: script principal e README próprio; dados em §data/raw/<estudo>/§ quando permitido.
- §outputs/<estudo>/<tipo>/§: resultados regeneráveis, normalmente ignorados no Git.
- §data/processed/§: dados transformados; §data/raw/§: originais ou exceções históricas justificadas no README.
- §data/DATA_PROVENANCE.csv§: atualização obrigatória para novas fontes ou mudança de permissões.
- Evite §setwd()§, caminhos absolutos locais e instalação automática de pacotes no meio das análises.
- Se uma mudança exige dados privados ou externos, preserve um teste público mínimo e declare explicitamente o que ele **não verifica**.

## Critérios de aceitação

- Links internos e caminhos de execução válidos.
- Contratos de entrada/saída documentados.
- Testes adicionados ou justificativa para sua ausência.
- Nenhuma alteração de resultado sem comparação quantitativa e diagnóstico.
- Nenhuma declaração de reprodutibilidade integral baseada apenas no parse da sintaxe.

## Histórico

Use mensagens de commit como §fix:§, §docs:§, §test:§ e §refactor:§ descrevendo precisamente o que mudou. Informe no PR se modelos foram **reamostrados** ou apenas **auditados a partir de agregados existentes**.
