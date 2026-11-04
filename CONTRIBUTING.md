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

## Contrato editorial obrigatório para estudos (T02.1–T02.7)

A partir das novas contribuições documentais e de mudanças substantivas em estudos, classifique a unidade como **Laboratório de modelos** ou **Estudo aplicado** segundo a [taxonomia](scripts/STUDY_TAXONOMY_T02_1.md), sem renomear identificadores históricos. A pasta `scripts/_setup/` é infraestrutura, não um estudo. Uma demonstração simples não precisa receber artificialmente dez relatórios históricos; registre a justificativa de etapas que realmente não se aplicam.

1. **Metadados:** forneça uma ficha por unidade com os **oito campos** `Pergunta`, `Objeto`, `Dados`, `Método`, `Estado`, `Resultados`, `Limites` e `Responsável`, conforme o [contrato T02.2](scripts/STUDY_METADATA_CONTRACT_T02_2.md). Identifique fontes, versão, acesso e o que foi efetivamente executado; não presuma um responsável a partir da autoria Git. Campos sem prova devem conter `PENDENTE_DE_VERIFICACAO`, ou `INDISPONIVEL_NO_HISTORICO` somente após busca documentada; `NAO_APLICAVEL` exige fundamento explícito.
2. **Planejamento E00:** para um estudo novo, use o [modelo T02.3](scripts/STUDY_E00_TEMPLATE_T02_3.md) com perguntas, estimandos, hipóteses concorrentes, critérios de decisão/revisão/não decisão, riscos, fontes e responsáveis comprovados. `FECHADA` requer critério atendido, evidência, autoridade e motivo; ausências mantêm a decisão `ABERTA`. Não descrever uma reconstrução posterior como pré-registro histórico.
3. **Formulação e execução E06→E07:** quando houver E06/E07 substantivos, preencha a [matriz T02.4](scripts/STUDY_E06_E07_MATRIX_T02_4.md) para cada equação, dado/índice, parâmetro, prior, likelihood, transformação/restrição e generated quantity **pertinente**, explicitando localizadores, versões, dimensões e divergências. Uma classe ausente exige análise e justificativa. Para código só em R, mapear a implementação R em vez de inventar Stan. Não afirmar correspondência completa a partir de nomes ou README.
4. **Evidência e afirmações E08→E09:** use a [matriz T02.5](scripts/STUDY_E08_E09_MATRIX_T02_5.md), uma linha por afirmação material, com enunciado literal, estimando, evidência, diagnósticos, sensibilidade, hipótese, limite e localização verificável. Diferencie posterior marginal, contraste conjunto identificado, aproximação e cenário hipotético; `CI=success` não comprova inferência ou licença. Mantenha `ABERTA` ou `DIVERGENCIA` quando não houver reconciliação.
5. **Estado de cada etapa E00–E10:** utilize exatamente `APROVADO`, `RESSALVA`, `PENDENTE`, `NAO_APLICAVEL` ou `INDISPONIVEL_NO_HISTORICO` segundo as condições do [contrato T02.6](scripts/STUDY_STAGE_STATUS_T02_6.md). O estado global depende de evidência e QA **daquela etapa**, não dos estados locais das matrizes ou da CI, nem das caixas Exec./QA do plano de correção. Preserve transições e reaberturas com data, SHA, fundamento e parecer independente.

**Legado:** preserve fontes, código, resultados e decisões históricos. A aplicação retroativa é `RECONSTRUCAO_EDITORIAL` com data, conjunto de fontes efetivamente inspecionadas, lacunas e sem fingir execução original. A adoção editorial não homologa origens/redistribuição/privacidade; datasets sujeitos à [Issue #14](https://github.com/riscotta/bayes-stan-r/issues/14) não devem ser publicados adicionalmente sem autorização competente. Mudança em modelo, priors, likelihood, amostra, inferência ou resultado exige revisão científica explícita, comparação e novo gate.

**Gate de PR:** anexe diff, SHA-base e HEAD, comandos e resultados de testes, evidências do piloto quando aplicável, riscos, limitações e separação entre `implementado`, `testado`, `revisado por QA` e `cientificamente validado`. A CI verde é requisito técnico quando exigido, **não** QA independente. Solicite revisor distinto para o SHA final; se a revisão for inconclusiva, mantenha PR aberto e sem merge. Para novas pesquisas, admita explicitamente `NAO_DECIDIR` quando hipóteses, dados, identificação ou diagnósticos não permitirem decidir.

**Exemplo de aplicação controlada:** [piloto documental de mortalidade SP 0–74](scripts/STUDY_PILOT_T02_7_MORTALIDADE_SP.md). Ele demonstra preenchimento e lacunas **sem** executar novo ajuste, preencher toda a cadeia E00–E10 ou certificar a validade científica; não use o piloto como aprovação automática de outros estudos.
