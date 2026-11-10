# T04.7 — Exemplos didáticos ancorados na PNAD Contínua (Estudo I)

**Tarefa literal:** “Revisar exemplos didáticos: preferir valores e variáveis da PNAD; usar analogias apenas como complemento.”  
**Natureza:** revisão **editorial** de materiais já versionados; não é novo estudo, nova inferência ou reconstrução contemporânea das etapas históricas.  
**Base verificada antes da edição:** `main` `bee679d573de72e2fffca60404dc11fd501b7a7e` em 10/11/2026, após o squash do PR #42; CI de push/main #42821483002 concluída com sucesso.  
**Fonte numérica exclusiva dos exemplos observados:** [projeção pública E09](../../data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv), blob `096b9e2d42c8fe1231ac675dc48cfc61dfa294d2`. A origem primária IBGE, vintage completa, CVs e saídas E04–E09 não foram revalidados nesta tarefa.

## 1. Hierarquia obrigatória da explicação

1. **Agregado publicado e identificável:** especificar `IndicadorId`, localidade Brasil, período inicial/final, unidade, `ValorOrigem`, `ValorDestino`, `Delta` e campo `SituacaoOficialIBGE` quando estiverem no CSV; não equiparar `Delta` ou situação oficial a uma probabilidade.
2. **Expressão de cenário condicional:** somente após o exemplo observado, explicitar a hipótese adicional, dependências e insumos faltantes. `rho=-0,5;0;0,5;0,9` são valores postulados pelo código E08.2, nunca correlações inferidas da PNAD.
3. **Oráculo matemático sintético:** conservar números inventados apenas quando essenciais para verificar fórmula, caso de fronteira ou teste automatizado; rotular `SINTETICO_NAO_PNAD` e não os apresentar como estimativa, teste oficial ou evidência de inferência.

## 2. Três leituras verificáveis — recorte recente (202606→202609)

| Indicador / unidade | `ValorOrigem` | `ValorDestino` | `Delta` do CSV | `SituacaoOficialIBGE` do CSV | Leitura permitida |
| --- | ---: | ---: | ---: | --- | --- |
| `desocupacao_pct`, % / p.p. | 5,4 | 5,2 | −0,2 p.p. | `caiu` | Queda descritiva de taxa publicada; não calcular chance de queda |
| `desalentados_mil`, mil pessoas | 2.288 | 2.459 | +171 mil | `cresceu` | Aumento descritivo mesmo com queda da desocupação; dimensões diferentes |
| `rendimento_real_habitual_rs`, R$ | 3.753 | 3.790 | +37 R$ | `estavel` | Delta positivo **não** equivale automaticamente a aumento significativo |
 
Essas observações são linhas públicas da **projeção histórica**, não recoleta SIDRA nem reprodução de E09 integral. `SituacaoOficialIBGE` é a classificação **transcrita** no CSV; não foi recalculada ou confrontada nesta T04.7 com o Quadro Sintético primário.

Como exemplo adicional de distinção entre delta e classificação, o `nivel_ocupacao_pct` **anual 202509→202609** passa de **58,7% para 59,0%**, `Delta=+0,3` p.p., com situação `estavel` na projeção; para a janela **recente**, **58,8%→59,0%**, `Delta=+0,2` p.p., situação `cresceu`. O sinal isolado de um delta arredondado não substitui a classificação informada.

## 3. Aplicação nas explicações já existentes

| Documento | Problema editorial identificado | Correção T04.7 | Risco que continua |
| --- | --- | --- | --- |
| [T03.2 — derivação](derivacao_contrastes_gaussianos_T03_2.md) §4 | Caso `10→13`, com `s_0=2`, `s_1=1`, aparecia como exemplo central | Abrir com `desocupacao_pct` recente `5,4→5,2`, `d=-0,2` p.p.; deixar a tabela `10→13` como verificação algébrica explicitamente sintética | CVs e covariâncias não disponíveis; probabilidade PNAD **não calculada** |
| [T03.4 — suporte e arredondamento](auditoria_suporte_cv_arredondamento_T03_4.md) §§2/4 | Taxas `0,2` com `s=1`, `5,2→5,3` e limite P14 inventado podem ser confundidos com realidade do estudo | Primeiro observar taxa `5,4→5,2` e, **condicionando** a regra ilustrativa de arredondamento ao décimo, obter apenas `[-0,30;-0,10]` p.p.; outros números seguem expressamente sintéticos | Intervalo de arredondamento **não** é IC, significância oficial ou auditoria do `ValorOriginal` |
| [T03.5 — alternativas](comparacao_alternativas_inferenciais_T03_5.md) §3 | Demonstrava não identificação e Fréchet exclusivamente por valores inventados | Antepor taxa e períodos observados; nomear os cinco proxies PNAD e manter `s_0,s_1,rho` simbólicos; sintéticos só para exibir efeito matemático | Não há posterior temporal ou conjunta identificada |
| [T03.6 — rótulos](rotulos_inferenciais_T03_6.md) §2 | Legenda entre colchetes sem indicador/períodos reais | Usar `desocupacao_pct`, janela recente, `5,4→5,2`, `caiu`; legenda de probabilidade só como modelo **a preencher com fonte verificada** | CSV público não contém `ProbSentido`, CV, draws ou `rho` estimado |
| [T03.7 — invariantes](invariantes_sinteticos_T03_7.md) §1 | Exemplo `p_i=0,9` na linha E08.4 sem mencionar os proxies reais | Nomear os cinco `IndicadorId` do código e rebaixar `p_i=0,9` a oráculo sintético | Fréchet para cinco proxies não significa probabilidade de melhoria dos cinco eixos |

Os **testes matemáticos históricos** preservam números sintéticos (`10,13,2,1`; `p_i=0,9` etc.) para assegurar invariantes. Alterar seus oráculos para simular dados PNAD, sem insumos e validação, seria falsificar a origem da evidência; a revisão editorial não o faz.

## 4. Exemplo de leitura metodológica sem inventar inferência

Para `desocupacao_pct` recente, `y_0=5,4`, `y_1=5,2`, `d=-0,2` p.p., `k=-1` (menor é favorável). **Somente num cenário que fornecesse escalas justificadas** `s_0,s_1>0`, a família hipotética de E08.2 seria:

```text
Var(Delta | rho) = s_0^2 + s_1^2 - 2*rho*s_0*s_1
P_cenario(movimento favoravel | rho) =
  Phi(0,2 / sqrt(s_0^2+s_1^2-2*rho*s_0*s_1))
```

Isso **não** calcula a probabilidade de melhora para a PNAD: o CSV público não oferece CVs `202606`/`202609`, covariância temporal, likelihood conjunta nem posterior de `Delta`. `rho=0` selecionado por E09 representa **hipótese**, não independência empírica. Os contrastes anual e recente compartilham destino `202609` e não são independentes por construção. O E07 ajusta os níveis marginais apenas em `202609`; `y_rep` é preditiva marginal, não diferença.

O mesmo cuidado vale para cinco proxies de E08.4: `desocupacao_pct`, `nivel_ocupacao_pct`, `subutilizacao_pct`, `informalidade_pct`, `rendimento_real_habitual_rs`. Os limites `max(0,sum(p)-4)` e `min(p)` só se aplicam a **cinco probabilidades condicionais fornecidas**, não observadas no CSV de 26 linhas; sem dependência interindicadores identificada, não existe probabilidade conjunta pontual. `p_i=0,9` é exemplo sintético, não probabilidade PNAD.

## 5. Integridade e pendências

**Não alterado:** CSVs, resultados E04–E09, arquivos em `scripts/pnadc_estudo_i/estudo_I/`, três modelos Stan, priors, likelihoods, estimandos, parâmetros, inferências e decisões históricas. Apenas textos de leitura, índice e teste editorial. Nenhum dado ou CV foi fabricado; não se realizou Stan/MCMC, PPC, inferência conjunta, consulta SIDRA ou validação de classificação pelo IBGE.

**Ressalvas T04.6 conservadas:** **R1:** `auditar_e10.py` ainda usa `assert`, e `python3 -O` desativa suas validações; **R2:** há dependência operacional indireta `E08.3 → E08.4 → E09` não integralmente explicitada no E10 original; **R3:** o nível N2 via Python continua opcional na CI e foi pulado no runner da T04.6. Não declarar essas ressalvas sanadas pela T04.7.

**Governança:** Etapa 01/P0, T01.3–T01.7 e [Issue #14](https://github.com/riscotta/bayes-stan-r/issues/14) abertas; [Issues #15](https://github.com/riscotta/bayes-stan-r/issues/15) e [#16](https://github.com/riscotta/bayes-stan-r/issues/16) abertas. As seis decisões `D00-01`–`D00-06` do [E00](e00_consolidado_T04_1.md) permanecem `ABERTA`. Níveis N3/N4 continuam bloqueados por evidência/insumos ausentes e governança. **T04.7: QA PENDENTE**; nenhuma autoaprovação nem autorização de merge. A Etapa 04 permanece aberta.

**Fontes:** [E00](e00_consolidado_T04_1.md), [T04.2](contratos_e01_e05_T04_2.md), [T04.3](e06_especificacao_modelos_T04_3.md), [T04.4](ponte_e06_e07_T04_4.md), [T04.5](ponte_e08_e09_T04_5.md), [T04.6](e10_matriz_evidencias_reproducao_T04_6.md), [T03.4](auditoria_suporte_cv_arredondamento_T03_4.md), [nota científica](nota_cientifica.md), [CSV E09](../../data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv).
