# T06.4 — Política de preservação, reconciliação e variação numérica

**Tarefa literal:** “Definir políticas para resultados históricos: preservação, reconciliação e tolerância a variação numérica.” **Base de partida:** main 34f34e83c4918cbb885d51647d5d0b2b785796bb, 12/11/2026. **Natureza:** contrato documental e comparador técnico de arquivos já disponíveis; sem novas execuções de estudos, acesso a dados institucionais, reamostragem, alterações de priors/likelihoods/estimandos, substituição de referências ou validação científica.

A política é geral para os **33 estudos** do [manifesto T06.1](../t06_1/manifesto_estudos_T06_1.json), independentemente de evidência individual de N2/N3/N4. Os cinco estados da [T06.2](../t06_2/estados_reprodutibilidade_T06_2.json) e os três modos da [T06.3](../t06_3/modos_operacionais_T06_3.json) permanecem independentes. Não aferido não significa impossível; proximidade numérica não autoriza promoção de nível.

## 1. Congelamento, proveniência e retenção

1. **Original e referência:** resultados históricos e insumos versionados são somente leitura. Não substituir, limpar, normalizar in-place, reformatar, recalcular, renomear ou adicionar metadados dentro da referência. Registrar commit Git do código, caminho relativo canônico, hash Git da referência catalogada (quando houver) e SHA-256 calculado dos bytes do artefato efetivamente utilizado. Um Git blob SHA de T06.1 **não é** SHA-256 e não garante validade material. Se o hash de referência divergir, BLOQUEADO/DIVERGÊNCIA, sem gerar nova referência para acomodar o resultado.
2. **Saídas novas:** somente sob "outputs/_reexecucoes_t06_4/<estudo>/<execucao_id>/", com IDs únicos e sem redirecionar scripts legados automaticamente. O comparador não executa nem altera scripts; antes de rodar script que escreve em caminhos legados (PNAD E04/saida e outputs/mortalidade, por exemplo), usar ambiente isolado autorizado e inspecionar todos os destinos. Pasta existente não é prova de execução.
3. **Trilha mínima:** estudo e versão, modo, commit de código e configuração, comando efetivo, sistema/linguagem/pacotes/CmdStan, origem/vintage da entrada, direitos, SHA-256 da entrada, seed ou declaração explícita de não aplicabilidade, ID/horário/logs, diagnósticos, caminhos e hashes de saída, regra pré-especificada e parecer QA. Não publicar dados protegidos ou rastros identificáveis sem avaliação institucional. Falta de material essencial é BLOQUEADO/NÃO AFERIDO, nunca PASS.
4. **Retenção:** manter referências e manifestos históricos; conservar logs, configurações, hashes, resumos e decisões de QA por execução. Descarte de cópias e retenção de dados potencialmente pessoais dependem da política institucional homologada, não de prazo inventado aqui. Não transferir dados institucionais ao GitHub.

## 2. Critérios por tipo de artefato

| Tipo | Alinhamento e critério inicial | Variação e decisão |
|---|---|---|
| Binário, código, manifesto e imagem com identidade exigida | SHA-256 de bytes **exato** | Zero tolerância; diferença de serialização é alteração do arquivo e requer análise |
| CSV determinístico de agregados/contagens | Conjunto idêntico de chaves/colunas, sem duplicatas, decimal finito, mesmo estimando e unidade; padrão abs=0 e rel=0 | Arredondamento, localidade ou flexibilidade numérica só sob protocolo pré-aprovado |
| Resumo Stan/MCMC, bootstrap, simulação | Alinhar estimando, população, unidade, desenho, código, prior, likelihood, seed, ambiente e método | MCSE, ESS, R-hat, divergências, quantis, reamostragem e pareamento demandam calibração por estudo; dentro de limiar permanece PENDENTE_QA |
| Figuras e tabelas regeneradas de agregados | Comparar definições e tabelas de origem; exigir bytes exatos só para identidade | Mudança de renderizador não dispensa reconciliação quantitativa |

Quando pré-aprovado, o comparador usa: **|novo − referência| ≤ a + r × |referência|**, sendo a ≥ 0 (unidade do estimando), r ≥ 0 (adimensional). O padrão é a=r=0. Esta regra assimétrica em torno da referência é apenas operacional; não calibra limiares científicos. Um limite não nulo exige justificativa matemática, protocolo, responsável de QA e evidência de calibração anteriores ao julgamento. Para erro Monte Carlo, variância da diferença depende de variâncias/covariâncias e da dependência entre execuções; não usar um epsilon universal. Quantis, amostragem correlacionada e bootstrap pareado exigem critérios próprios. Sem critério justificável, registrar PENDENTE_QA/BLOQUEADO.

## 3. Gates por modo

- **Auditoria rápida/N1–N2:** referências públicas/agregadas já permitidas, sem acesso a insumos protegidos. CONFERENCIA_TECNICA não certifica N2 por estudo. T06.2 documenta N2 descritivo limitado somente em **PNAD E09 e mortalidade SP (2/33)**; outros 31 não aferidos.
- **Reexecução de modelo/N3:** exigir E04/equivalente íntegra, autorização, ambiente e seed, logs de ajuste, diagnósticos, estimandos, comparações pré-especificadas e QA independente. Sem direitos homologados, o contrato técnico bloqueia o modo. Nenhum N3 certificado.
- **Reprodução desde dados/N4:** adicionalmente cadeia primária, versão, licenças, transformações, QA das junções, reconstrução dos resultados e validação independente. Nenhum N4 ou validade científica certificados.

**P0:** [issue #14](https://github.com/riscotta/bayes-stan-r/issues/14) aberta (proveniência e redistribuição sem homologação). [#15](https://github.com/riscotta/bayes-stan-r/issues/15) e [#16](https://github.com/riscotta/bayes-stan-r/issues/16) permanecem abertas. Nenhum teste permite liberar dados nem fazer release.

## 4. Riscos específicos

- **PNAD Estudo I:** auditoria E09 confere projeção pública congelada por checksum; não recompõe aquisição SIDRA, E04–E08 ou cadeias marginais E07. Correlações temporais rho são cenários, não estimativas; jamais declarar posterior conjunta.
- **Mortalidade SP 0–74:** modo auditar usa agregados; conferir exige E04 local e não reestima; completo reajusta oito modelos R a partir de E04. Bootstrap histórico NumPy versus R pode variar. Limiar requer protocolo separado; equivalência territorial não homologada.
- **Cães F5:** modo padrão confere tabelas; --run_stan=1 exige E04 local e nova amostragem, não N4. CmdStan histórico 2.37.0 difere da referência global 2.40.0. Cinco folds e bootstrap pareado de 2.000 replicações não são observações independentes.

## 5. Conferidor técnico: contrato e exemplos

O script [reconciliar_resultados_t06_4.py](../../.github/scripts/reconciliar_resultados_t06_4.py) usa somente Python padrão, lê uma referência e um candidato já existentes, verifica metadados mínimos, isolamento canônico de caminho (inclusive symlinks e hardlinks), SHA-256 de ambos, chaves e decimais de CSV e declarações de tolerâncias. Exige **catálogo preexistente com digest externo fixado e confiável** para comparar referências. **Não roda modelos, não grava saídas, não autentica assinaturas/autorizações e não certifica N2/N3/N4.**

O registro externo JSON exige: "schema":"t06_4_execucao_v1", "estudo", "execucao_id", "modo", "tipo" ("bytes_exatos", "csv_deterministico", "csv_estocastico_resumo"), "codigo_sha" (40 hex), "ambiente", "comando", "seed" (inteiro não negativo de 0 a 2^64−1, string de dígitos correspondente ou texto literal "NAO_APLICAVEL" quando realmente inaplicável; listas, booleanos, objetos e strings vazias são bloqueados), "entrada_sha256", "direitos" ("NAO_HOMOLOGADOS"/"HOMOLOGADOS"), "referencia" e "candidato" (ambos com "path" e "sha256"). Para CSV acrescentar "comparacao" = {"chaves":["id"],"colunas":{"valor":{"abs":"0","rel":"0"}}}. Limiar não zero ou resumo estocástico exigem em "comparacao.preaprovacao": "id_protocolo", "justificativa_matematica", "responsavel_qa" e "evidencia_calibracao"; o estocástico também demanda "oraculo_estimandos" e "diagnosticos_mcse_ess_rhat". **Esses campos não verificam autenticidade de assinatura ou qualidade de calibração**: revisão independente precisa confrontar as evidências citadas. Na implementação corrigida, limiares `abs` ou `rel` **não zero** permanecem necessariamente `PENDENTE_QA`, mesmo se a diferença estiver dentro do limite e a preaprovação textual estiver completa. Divergências fora do limite ainda produzem `DIVERGENCIA`.

### 5.1 Âncora de referência independente e caminhos canônicos

O comparador exige dois argumentos **fora do JSON da execução**: `--catalogo` (snapshot JSON previamente auditado) e `--catalogo-sha256-confiavel` (SHA-256 desse snapshot obtido e fixado antes da comparação, em versão controlada ou canal de custódia independente). O catálogo usa `schema=t06_4_catalogo_v1`, `git_commit` (40 hexadecimais) e `referencias` (lista de registros com `path`, `sha256` de bytes e `git_blob_sha` de objeto Git SHA-1). O comparador confirma digest do próprio catálogo, entrada única por caminho, SHA-256 da referência e Git blob do arquivo. **O SHA-1 do blob não substitui o SHA-256 dos bytes.**

**Gate de confiança externo:** o hash fixado não pode ser simplesmente recalculado do catálogo recém-entregue com o registro da execução: isso repetiria a falha B3. Quem executa deve verificar previamente a proveniência do snapshot, o commit declarado e a identidade do registro de custódia (Git confiável, assinatura ou processo independente), antes de fornecer o pin. O script não consulta nem autentica automaticamente o servidor Git, a assinatura do catálogo, o emissor do pin ou o caráter preexistente do commit; a ausência de custódia confiável impede utilizar `CONFERENCIA_TECNICA` como evidência de integridade histórica.

Para ambos os artefatos, são rejeitados links simbólicos no arquivo ou ancestrais sob a raiz, referências sob qualquer subárvore `outputs/_reexecucoes_t06_4`, hardlinks de referência/candidato e candidatos fora de `outputs/_reexecucoes_t06_4/<estudo>/<execucao_id>/`. A leitura não altera os arquivos. Os testes usam catálogo e hashes **inteiramente sintéticos** e não homologam um catálogo real de nenhum dos 33 estudos.

    python .github/scripts/reconciliar_resultados_t06_4.py --raiz . --registro "$REGISTRO" --catalogo "$CATALOGO_AUDITADO" --catalogo-sha256-confiavel "$CATALOGO_SHA256_CONFIAVEL"
    python -m unittest discover -s tests/python -p 'test_resultados_t06_4.py' -v


### 5.2 Precisão aritmética, validação de metadados e limites explícitos

A comparação de decimais finitos usa razões exatas (`fractions.Fraction` a partir de `Decimal`), não a precisão global de 28 dígitos do contexto Decimal. **Toda diferença que excede estritamente** `abs + rel × |referência|` retorna `DIVERGENCIA` antes de avaliar o gate de QA. Igualdade exata no limiar permanece dentro da tolerância, mas limiar não zero retorna **PENDENTE_QA**, nunca aprovação inferencial.

Para limitar custo e expoentes patológicos, cada número de entrada ou tolerância admite **até 512 algarismos no coeficiente** e **expoente decimal com módulo até 1000**. Acima desses limites, a execução retorna `BLOQUEADO`, não aproxima o número. A seed aceita somente inteiro não negativo de até 64 bits, a forma decimal em string ou `NAO_APLICAVEL`; o último requer justificativa externa de não aplicabilidade. Entradas JSON fora do contrato são rejeitadas de modo controlado, sem traceback. Esses limites operacionais não foram calibrados como critérios científicos.

### 5.3 Evidência externa de custódia: pendência B3

**Situação em 12/11/2026:** nenhum catálogo histórico real preexistente, registro independente de pin ou vínculo commit/path/blob, custodiados e auditados, foi apresentado neste ciclo. O catálogo e os commits nos testes são sintéticos; os hashes são válidos apenas como autoconsistência de fixture. Portanto, este PR **não comprova autenticidade histórica real**. Até existir a evidência, bloqueia-se o uso do comparador para reivindicar integridade histórica, N2/N3/N4 ou homologação de resultados reais, ainda que a saída técnica isolada diga `CONFERENCIA_TECNICA`.

Para uso real autorizado, um auditor independente deverá obter, de fonte preexistente confiável, (i) identidade e versão do catálogo, com data e custodiador; (ii) pin SHA-256 publicado ou registrado **antes** da execução, independente do responsável pela comparação; (iii) commit Git existente com origem rastreável; (iv) correspondência do caminho e Git blob do artefato no commit; (v) permissão de uso e eventual triagem de privacidade. Registrar as evidências externas no parecer e confrontá-las com os bytes locais. **Recalcular o pin do catálogo fornecido pelo executor não atende B3.** O script, por projeto, não autentica nem atesta essa cadeia.

**Gate P0 #14:** enquanto não houver homologação institucional externa verificável, os modos `reexecucao_modelo` e `reproducao_desde_dados` continuam **BLOQUEADOS**, inclusive quando `direitos=HOMOLOGADOS` aparece no registro. O valor textual não equivale a licença. A liberação futura requer mudança controlada com evidências específicas e novo QA.

Status: CONFERENCIA_TECNICA (exit 0, somente técnica, modo auditoria rápida, igualdade exata e catálogo externamente fixado), PENDENTE_QA (exit 3), DIVERGENCIA ou BLOQUEADO (exit 2). Não publicar PENDENTE_QA como PASS. Divergência exige preservar logs/hashes, classificar erro (entrada, ambiente, seed, modelo, serialização ou Monte Carlo), abrir registro, escalar ao responsável científico e obter nova autorização antes de reexecutar. Não alterar oráculo/limiar retroativamente para acomodar discrepância.

A [política estruturada](politica_resultados_T06_4.json) é validada contra T06.1–T06.3 em testes adversariais apenas sintéticos. Eles não executam os 33 estudos, não validam direitos nem inferência. **Etapa 06 aberta**: T06.5–T06.7 e QA independente de T06.4 permanecem pendentes.
