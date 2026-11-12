# T06.3 — Três modos operacionais distintos de reprodução

**Tarefa literal:** “Documentar modo auditoria rápida separado de modo reexecução de modelo e modo reprodução desde dados.”

**Corte:** 12/11/2026. **main-base:** f80343fdbb2d65ca7b726aefd919c611013c93df. **Natureza:** documentação/contratos, sem novos ajustes, sem dados institucionais acessados e sem reprodução desde fontes primárias. O [JSON T06.3](modos_operacionais_T06_3.json) cobre os **33 estudos** do [manifesto T06.1](../t06_1/manifesto_estudos_T06_1.json), sem promoção indevida dos [estados T06.2](../t06_2/estados_reprodutibilidade_T06_2.json). Ter um comando não comprova execução. Não aferido não significa impossível.

## 1. Modos, operações e critérios de parada

| Modo | Entrada e operação | Gate exigido | Evidência possível |
| --- | --- | --- | --- |
| **Auditoria rápida** | Inventário/identidade Git e conferência não destrutiva de agregados congelados; nenhum ajuste ou download de dados originais. | Caminhos, hashes e identidades numéricas documentadas conferidos, log e commit identificáveis. | N1 documental ou N2 descritivo **somente quando há evidência por estudo**. |
| **Reexecução de modelo** | Base analítica E04/equivalente já construída, dependências, seeds e configurações; novo ajuste e diagnóstico de estimandos. | Permissão, integridade da entrada, versão/ambiente e execução com logs/draws, diagnósticos/oráculos, tolerâncias justificadas e QA independente. | N3 **somente após execução comprovada**, nunca por código, sintaxe ou CI de auditoria. |
| **Reprodução desde dados** | Fonte primária identificada, licenciada e versionada; importação, E04, modelos e síntese em cadeia. | Proveniência e direitos homologados, cadeia integral testada, hashes, ambiente, saídas reconciliadas e QA independente. | N4 **somente após execução comprovada**, sem implicar validade científica. |

**Regra de parada:** ausência de licença/insumo, checksum incorreto, ambiente divergente, dependência não resolvida ou log insuficiente resulta em **BLOQUEADO/NÃO AFERIDO**, jamais PASS ou promoção para N3/N4. Não republicar insumos, não reamostrar automaticamente, não alterar priors/likelihood/estimandos e não sobrescrever resultados históricos.

## 2. Auditoria rápida — dois exemplos N2 com CI histórica

Executar da raiz do repositório, registrando o SHA do checkout.

**PNAD Contínua — Estudo I:** requer Python 3, somente biblioteca padrão. O script confere SHA-256 do CSV público congelado, 13 indicadores, 5 eixos e 26 contrastes, sem SIDRA, E04–E08 ou MCMC.

~~~bash
python scripts/pnadc_estudo_i/auditar_e10.py
~~~

**Mortalidade SP (0–74):** requer R base; consulta E09 agregada congelada e regenera tabelas/figuras em outputs/mortalidade_sp_0_74. Não calcula E04/E07.

~~~bash
Rscript --vanilla scripts/mortalidade_sp_0_74/reproduzir_E10.R --modo=auditar
~~~

Essas duas auditorias possuem prova histórica em [CI push/main #43252399861](https://github.com/riscotta/bayes-stan-r/actions/runs/43252399861). **Apenas estes 2/33 estudos** têm N2 limitado atestado no contrato T06.2; os demais 31 não foram aferidos por essa evidência. Os scripts testados nesta T06.3 são os testes Python de contrato; os comandos de auditoria pública são executados pela CI preexistente.

**Cães F5:** o comando público abaixo consulta tabelas auditadas e **não reamostra Stan**. Está documentado no README do estudo, mas esta tarefa **não afirma execução nova nem ateste N2 individual**:

~~~bash
Rscript scripts/caes_raca_f5/caes_raca_f5_cmdstanr.R
~~~

Teste documental T06.3, sem ler dados brutos e sem ajuste:

~~~bash
python -m unittest discover -s tests/python -p 'test_modos_t06_3.py' -v
~~~

## 3. Reexecução de modelo — comandos identificados, sem certificação N3

**Mortalidade SP:** o modo completo começa na base derivada E04, arquivo **local, não publicado** data/raw/mortalidade_sp_0_74/base_analitica_celular_PROVISORIA.csv. Precisa de R e MASS, reestima oito modelos R E07, recalcula E08/E09 e grava em outputs. O script verifica a presença da base e do pacote, **não** homologa SHA externo, permissão de uso ou limites territoriais; qualquer comparação dos ajustes é gate futuro.

~~~bash
Rscript --vanilla scripts/mortalidade_sp_0_74/reproduzir_E10.R --modo=completo
~~~

O comando abaixo **não reajusta modelo**: confere E08/E09 com base E04 e quatro CSVs locais E07. Portanto não é N3:

~~~bash
Rscript --vanilla scripts/mortalidade_sp_0_74/reproduzir_E10.R --modo=conferir
~~~

**Cães F5:** o wrapper possui rota opcional de amostragem, **não executada pela T06.3**, com base E04 original local, SHA-256 documentado no README, CmdStan e pacotes R adicionais. CmdStan histórico **2.37.0** e referência global **2.40.0** não são ambientes idênticos.

~~~bash
Rscript scripts/caes_raca_f5/caes_raca_f5_cmdstanr.R --run_stan=1
~~~

**PNAD E07:** runner preservado em scripts/pnadc_estudo_i/estudo_I/e07/run.ps1 (modos preflight, smoke, completo e resumo). A versão pública não contém entradas E04, resultados E07/E08, cadeias históricas nem coletor SIDRA original. **Nenhum comando N3 executável com insumos completos foi homologado para terceiros.** Sintaxe ou 52 cadeias históricas mencionadas no estudo não provam posterior conjunta identificada.

**Gate N3:** autorização das entradas; hash/vintage, ambiente, seeds, configuração de modelos; execução com logs e amostras; métricas e critérios de discrepância pré-especificados; revisão independente. A T06.3 **não satisfaz esse gate** para nenhum dos 33 estudos.

## 4. Reprodução desde dados — sem comando N4 verificado nesta tarefa

- **PNAD:** requer reconstruir aquisição SIDRA com versão, E04–E09, E07/Stan e saídas a partir dos dados originais. O coletor original e o snapshot integral não estão no clone; o script de auditoria E09 não substitui a cadeia.
- **Mortalidade SP:** requer fontes primárias APVP/PRO-AIM e SEADE, crosswalk temporal dos 96 distritos, E04 e depois E07–E10. O modo completo inicia em **E04 derivada**, não nos três insumos originais; a equivalência territorial não foi homologada.
- **Cães F5:** exige fonte original, transformações E04, termos de uso e validação da base. A flag --run_stan=1 parte da **base derivada**, não reconstrói a fonte primária.
- **Demais 30 estudos:** confrontar individualmente [T06.1](../t06_1/manifesto_estudos_T06_1.json), [T06.2](../t06_2/estados_reprodutibilidade_T06_2.json), o README e as entradas de proveniência. O contrato não afirma N3 ou N4 para nenhum.

**Nenhum comando ponta a ponta N4 é demonstrado nesta T06.3.** Isso não prova que outros estudos sejam impossíveis de reproduzir. Antes de tentar o N4, exigir direitos de acesso e redistribuição, origem e checksum, sequenciamento completo, ambiente/versions, saídas e revisão independente. Não publicar comandos fictícios ou tratar uma base já transformada como dado primário.

## 5. CI, preservação, evidência mínima e tarefas posteriores

A CI existente fixa Python **3.12.8** (auditoria N2), R **4.5.1** e usa Stan **2.40.0** para verificação de tradução representativa, **sem sampling**. renv.lock/config/environment.yml são referências globais, não prova de execução por estudo. Rotas históricas podem gravar fora da convenção outputs; inspecionar antes de rodar, preservar resultados e evitar acesso a dados institucionais.

Futuras evidências por modo devem indicar: estudo, modo, SHA de código, origem/direitos/vintage e hash de entrada sem expor bytes protegidos, ambiente e seed, comando, saída em pasta isolada, hash de saída, logs, comparação/oráculos, limitações, CI e parecer independente. Não inventar PASS pela ausência de erro.

O teste T06.3 deve ser executado no job Python e seus resultados conferidos no **HEAD do PR**. Ele verifica contratos/mutações sobre arquivos de código e documentação, **não** executa N3/N4 nem atesta ciência. QA independente em outro chat é obrigatório antes de eventual squash. Políticas de preservação/tolerâncias quantitativas pertencem à **T06.4**; lacunas externas, à **T06.5**; piloto real, à **T06.6**; reconciliação dos 33 READMEs, à **T06.7**.

**Governança:** [issue #14 P0](https://github.com/riscotta/bayes-stan-r/issues/14) e issues [#15](https://github.com/riscotta/bayes-stan-r/issues/15)/[#16](https://github.com/riscotta/bayes-stan-r/issues/16) seguem abertas. Etapa 06 **ABERTA**, sem homologação jurídica, inferencial ou científica nesta tarefa.
