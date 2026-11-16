# T06.6 — piloto de auditoria pública Cães F5 (N2)

**Tarefa literal:** “Executar piloto em estudo com insumos públicos e comparar resultados com registro verificável.”

**Delimitação prévia:** auditoria rápida N2 das quatro tabelas agregadas públicas do estudo caes_raca_f5. Não se afirma reprodução Stan N3, reprodução desde fonte N4, validação científica, licença institucional geral ou calibração externa. Não se modifica modelos, priors, likelihood, estimandos ou resultados congelados.

## 1. Fonte, vintage e direitos

Origem: Morrill et al. (2022), Associated data files and scripts for 'Ancestry-inclusive dog genomics challenges popular breed stereotypes', Dryad, publicado em 05/05/2022, DOI https://doi.org/10.5061/dryad.g4f4qrfr0.
Fonte oficial e política Dryad CC0 1.0: https://datadryad.org/dataset/doi%3A10.5061/dryad.g4f4qrfr0 e https://datadryad.org/terms.

A declaração CC0 se aplica aos dados publicados no Dryad, mas não transfere automaticamente direitos para artefatos derivados ou fontes terceiras. O pacote original Dryad não foi baixado nem autenticado byte a byte neste ciclo. A issue #14 P0 permanece aberta.

Entradas efetivas: os quatro arquivos CSV já versionados em data/raw/caes_raca_f5/resultados_auditados, conforme SHA Git fixados no protocolo JSON. São tabelas agregadas derivadas; não são os microdados da fonte. O piloto não acessa base E04 por cão, nem os 120 CSV de fits CmdStan históricos. Não reconstrói os 2.084 escores individuais. A cadeia fonte original → E04 não foi homologada.

## 2. Protocolo pré-especificado e custódia

Referência Git fixa: main 77ec0e1cf4ef4bf3feb9721ede8221846c6b0bf6. Antes da operação, exigir Git blob SHA de cada CSV exatamente igual ao declarado e registrar SHA-256 de seus bytes. O job de CI usa checkout efêmero; nenhuma entrada ou saída histórica é alterada.

Comando R efetivo, sem reamostragem:
    Rscript --vanilla scripts/caes_raca_f5/caes_raca_f5_cmdstanr.R --run_stan=0

O orquestrador .github/scripts/t06_6_piloto_f5.py executa o R como subprocesso e, independentemente, recalcula por Python Decimal os resultados derivados a partir das quatro fontes. Comparação pré-definida: três famílias t5, normal e skew; dez métricas numéricas por família (30 cotejos), colunas e ordem exatas; tolerância absoluta 5e-13 e relativa zero, somente para serialização numérica. É proibido aumentar a tolerância após observar os dados.

Invariantes adicionais: 7 linhas métricas, 30 linhas modelos/folds, 15 contrastes por fold; 2.084 cães; um fold desfavorável (fold 1) e quatro favoráveis por família; cobertura+caudas=1 (tolerância 1e-12); intervalo bootstrap positivo. Falhas em integridade, dados, R ou comparação => FALHOU, nunca PASS presumido.

As saídas do wrapper vão a outputs/caes_raca_f5/tables e seu resumo em logs, **no checkout efêmero** após verificação de ausência do CSV de saída preexistente. O registro de execução, stdout/stderr e cópia da tabela são escritos somente em outputs/_reexecucoes_t06_4/caes_raca_f5/t06_6_<run>_<attempt>, sem substituir referências versionadas. A CI publica artifact t06-6-caes-f5-n2 com retenção configurada em 30 dias; a política de retenção definitiva exige governança própria.

## 3. Ambiente e execução controlada

CI: Ubuntu 24.04; Python 3.12.8 para testes; R 4.5.1 no check obrigatório, Rscript efetivo gravado no registro. Sem cmdstanr/CmdStan nem nova amostragem; seed não aplicável. Arquivos globais renv.lock e config/environment.yml não são instalados integralmente por este piloto e não comprovam N3.

Executar a partir da raiz, após obter Rscript e Git:
    python -m unittest discover -s tests/python -p 'test_piloto_t06_6.py' -v
    python .github/scripts/t06_6_piloto_f5.py

Testes adversariais: mutação RMSE, não finito, família repetida, falta de coluna, linha duplicada, fold retirado, IC invertido, tolerância mínima válida. Logs e JSON incluem input SHA-256, Git SHA, comando, saída, R version, métricas e status. COMPARACAO_N2_CONFORME_PENDENTE_QA é apenas resultado operacional, não parecer científico ou QA independente.


**Evidência em falhas:** após o subprocesso R, o executor copia a tabela gerada (se existir) e calcula seu SHA-256 **antes** de validar as 30 métricas; mesmo em divergência numérica ou código de saída R não zero, o CSV original de tentativa acompanha registro de erro e logs no artifact. Falha de produção não é tratada como ausência de divergência. Esta salvaguarda corrige o achado automatizado do PR #56 no SHA anterior sem alterar tolerâncias, fontes ou estimandos.

## 4. Critérios de encerramento e limites

Após run real: conferir check obrigatório, logs, SHA da execução PR, 30 comparações, valores e hashes, e obter parecer independente em outro chat sobre o HEAD exato. Sem QA favorável, não fazer squash e não marcar T06.6 QA. O executor não revisa sua própria implementação.

Este piloto verifica coerência de agregados históricos; não reexecuta Stan, não estima posteriores, não prova capacidade preditiva fora da amostra histórica, não demonstra origem/reconstrução completa dos dados e não resolve issue #14 P0. Issues #15 e #16 e a Etapa 06 continuam abertas. T06.7 é tarefa posterior.
