# T02.2 — Contrato mínimo de metadados de estudos

**Tarefa:** definir oito metadados obrigatórios: pergunta, objeto, dados, método, estado, resultados, limites e responsável.  
**Versão de referência:** `main` em `2c826ca11b9661f10cf0b76ab542ced65e8c8051` (03/11/2026).  
**Natureza:** contrato editorial preenchível, não reconstituição de registros históricos, validação científica, autorização de dados ou política de publicação já homologada. A formalização em `CONTRIBUTING.md` e o piloto de adoção pertencem à T02.7.

O contrato complementa a [taxonomia de 33 pastas](STUDY_TAXONOMY_T02_1.md). Cada **unidade de estudo** (12 Laboratórios de modelos; 21 Estudos aplicados) mantém seu identificador histórico; `scripts/_setup/` é infraestrutura e fica fora do escopo. Não é exigido preencher retroativamente 33 fichas na T02.2.

## Regras de preenchimento e evidência

1. **Presença:** os oito campos devem existir em qualquer ficha. Nenhum pode ser omitido ou deixado vazio. O conteúdo é específico da unidade, não uma avaliação genérica do repositório.
2. **Ausência de evidência:** usar explicitamente `PENDENTE_DE_VERIFICACAO` (informação possivelmente obtível, ainda não verificada) ou `INDISPONIVEL_NO_HISTORICO` (não localizada nas fontes históricas consultadas), acrescentando a fonte consultada e a diligência futura quando pertinente. Não substituir essas marcas por afirmações inventadas. `NAO_APLICAVEL` exige justificativa e só vale quando o atributo substantivo não se aplica — **não** elimina o campo obrigatório.
3. **Fontes:** indicar caminho relativo, artefato, comando ou URL auditável sempre que existir. Um README pode provar a descrição documental, não a execução independente de um modelo. Dados ou resultados ausentes do Git não são presumidos disponíveis.
4. **Classificação:** em **Laboratório de modelos**, a pergunta pode ser metodológica e o objeto pode ser uma simulação/estimando didático; explicitar quando os dados são sintéticos. Em **Estudo aplicado**, declarar fenômeno, unidade analítica, população/período, quando conhecidos, sem inventar limites amostrais.
5. **Estado:** descrever separadamente o que foi documentado, implementado, efetivamente executado, revisado por QA e cientificamente validado. `CI=success` não implica MCMC reproduzido, identificação, segurança de dados ou validação científica. Não antecipar a taxonomia formal por etapa E00–E10, reservada à T02.6.
6. **Responsabilidade:** distinguir autor original, mantenedor documental e revisor quando houver prova. Um usuário do GitHub, o nome da organização ou a autoria presumida não identificam automaticamente o responsável científico; use marca de pendência quando desconhecido.
7. **Não intervenção:** não modificar datasets, saídas históricas, estimativas, priors, likelihood, código científico, permissões ou arquivos originais para completar a ficha. Se faltar documento contemporâneo, sinalizar **reconstrução editorial**.
8. **Segurança:** a ficha descreve localização e condições de acesso sem transcrever linhas individuais ou divulgar dados institucionais. Pendências de privacidade e licenças (Issue #14 e T01.3) permanecem intactas.

## Dicionário dos oito campos

| Campo obrigatório | Conteúdo mínimo verificável | Laboratório de modelos | Estudo aplicado |
| --- | --- | --- | --- |
| **Pergunta** | Interrogação investigada ou objetivo declarativo específico; fonte da formulação | Questão técnica/comportamento do método | Pergunta substantiva sobre o fenômeno |
| **Objeto** | Unidade/entidade ou estimando e escopo, quando disponível | Técnica, parâmetro, simulação e unidade de geração | Fenômeno, população, recorte espacial e temporal disponíveis |
| **Dados** | Identificação de insumo, caminho/fonte, natureza sintética/real, proveniência e situação de acesso | Semente/geração ou dataset demonstrativo, sem assumir licença | Fonte, vintage, transformações, disponibilidade e eventuais restrições |
| **Método** | Família de modelo/algoritmo, script/modelo vinculados, pressupostos centrais conhecidos | Técnica demonstrada e comparação/diagnóstico | Método orientado ao estimando e pressupostos relevantes |
| **Estado** | Situação documental, execução, QA e validação com evidências separadas; desconhecidos explícitos | Não equiparar exemplo compilável a MCMC executado | Não equiparar agregados auditados a inferência reproduzida |
| **Resultados** | Localização de saídas e seu tipo, ou marca explícita de ausência; sem inferir execução | Artefato demonstrativo/saída esperada versus verificada | Estimativa/contraste documentado e origem computacional |
| **Limites** | Limitações metodológicas, cobertura, direitos/acesso e inferência; incerteza explícita | Condições sintéticas e generalização limitada | Identificação, amostragem, fonte e extrapolação possíveis |
| **Responsável** | Nome/função e prova da atribuição ou marcador de pendência | Autor de material e contato, apenas se comprovado | Responsável científico/de dados e revisor, apenas se comprovados |

## Modelo copiável (preenchimento obrigatório)

Identificador histórico da pasta: `scripts/<identificador>/`.  
Classe editorial (T02.1): `Laboratório de modelos` **ou** `Estudo aplicado`.  
Registro: `DOCUMENTACAO_ATUAL` ou `RECONSTRUCAO_EDITORIAL` (com data e fonte). Estes três itens de contexto não substituem os oito campos.

**Pergunta:** PENDENTE_DE_VERIFICACAO — fontes consultadas: ...; diligência: ...

**Objeto:** PENDENTE_DE_VERIFICACAO — unidade/estimando/recorte: ...; fonte: ...

**Dados:** PENDENTE_DE_VERIFICACAO — natureza, fonte, versão, caminho, acesso e licença: ...

**Método:** PENDENTE_DE_VERIFICACAO — script/modelo e pressupostos: ...

**Estado:** PENDENTE_DE_VERIFICACAO — documentação: ...; execução: ...; QA: ...; validade científica: ...; evidências: ...

**Resultados:** PENDENTE_DE_VERIFICACAO — tipo, local e distinção entre saída declarada e saída verificada: ...

**Limites:** PENDENTE_DE_VERIFICACAO — cobertura, identificação, extrapolação, acesso e riscos: ...

**Responsável:** PENDENTE_DE_VERIFICACAO — papel e evidência de atribuição: ...

## Exemplos documentais (não são homologação)

Os exemplos são **preenchimentos editoriais a partir dos READMEs no SHA-base**, não fichas oficiais completas, nem relatos de execução neste ciclo. Informação não demonstrada permanece pendente.

### Exemplo A — Laboratório de modelos: `deck_15wins`

**Pergunta:** Como comparar a inferência de probabilidades de vitória por deck no modelo binomial negativo com a posterior conjugada? Fonte: [README local](deck_15wins/README.md).

**Objeto:** Probabilidade `p[d]` por deck e número de derrotas até `r` vitórias, em sessões simuladas; fonte: README local.

**Dados:** Sintéticos, gerados pelo script; entradas como `--r`, `--S_per_deck` e `--p_true` descritas no README. Nenhuma amostra real alegada.

**Método:** Priors Beta, verossimilhança binomial negativa para derrotas e comparação da posterior analítica com Stan; [script](deck_15wins/deck_15wins_negbin_beta_cmdstanr.R).

**Estado:** Formulação e comando documentados no README. Execução independente, estado atual de QA científico e resultados numéricos neste ciclo: `PENDENTE_DE_VERIFICACAO`. A CI do repositório não prova validação deste modelo.

**Resultados:** O README descreve métricas impressas (recuperação de parâmetros, diferenças de quantis, HMC e PPC); execução e valores observados neste ciclo: `PENDENTE_DE_VERIFICACAO`.

**Limites:** Simulação didática: não demonstra desempenho generalizável em partidas reais; diagnósticos e aproximações precisam de execução para avaliação. Direitos de quaisquer dados reais: `NAO_APLICAVEL` (exemplo descrito como sintético).

**Responsável:** Atribuição científica individual: `PENDENTE_DE_VERIFICACAO`; não presumida a partir do proprietário do repositório.

### Exemplo B — Estudo aplicado: `pnadc_estudo_i`

**Pergunta:** “O desemprego brasileiro caiu para 5,2%. Mas o mercado de trabalho melhorou em todas as dimensões?” Formulação literal no [README local](pnadc_estudo_i/README.md); o valor pertence à pergunta histórica, não a uma atualização aqui.

**Objeto:** Brasil, 13 indicadores substantivos em cinco eixos; contrastes 202509→202609 e 202606→202609; fonte: README local.

**Dados:** Agregados da PNAD Contínua/IBGE; síntese descritiva versionada em [contrastes E09](../data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv). Snapshot e entradas históricas E04–E08 não estão integralmente no Git; licença da vintage original não confirmada no README.

**Método:** Treze modelos marginais Stan históricos E07 e cenários aproximados E08/E09 condicionais a correlações temporais hipotéticas; [código e limites](pnadc_estudo_i/README.md).

**Estado:** Identidade dos 15 fontes históricos e verificações de síntese descritas no README; auditoria pública `python scripts/pnadc_estudo_i/auditar_e10.py` declarada ali. Neste ciclo T02.2 o comando **não foi executado**; reamostragem Stan e reprodução completa E04–E09: **não demonstradas**.

**Resultados:** Tabela histórica dos 26 contrastes disponível em [CSV](../data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv). Interpretar como síntese descritiva auditável, **não** como posterior conjunta identificada.

**Limites:** Falta de covariâncias observadas, sobreposição de trimestres móveis, arredondamento, desenho amostral complexo, acesso incompleto a insumos e vintage/licença não confirmada; não inferir `P(todos os eixos melhorarem | dados)` a partir dos cenários.

**Responsável:** Responsável científico individual e validação institucional: `PENDENTE_DE_VERIFICACAO`; não inferidos da autoria dos arquivos.

## Verificação e fronteira de escopo

**Aceitação documental T02.2:** localizar o arquivo e o vínculo no [catálogo](README.md); contar os oito campos exatos do modelo; conferir a descrição, marcadores e fontes; verificar que os dois exemplos estão em categorias diferentes; executar os testes estruturais `tests/testthat/test-study-metadata-contract.R` (na CI do PR). Falhas devem ser corrigidas antes do parecer independente.

**Não concluído por este contrato:** preenchimento de 33 fichas históricas; modelo E00 (T02.3); matrizes E06→E07 e E08→E09 (T02.4–T02.5); estados formais E00–E10 (T02.6); alteração de `CONTRIBUTING.md` ou piloto (T02.7); revisão científica/QA de datasets. A etapa 02 permanece aberta até QA de cada tarefa.

**Rastreabilidade:** [T02.1 taxonomia](STUDY_TAXONOMY_T02_1.md) · [Issue #14 — dados](https://github.com/riscotta/bayes-stan-r/issues/14) · [README PNAD](pnadc_estudo_i/README.md) · [README Deck](deck_15wins/README.md).
