# T05.2 | Auditoria Python pública PNAD N2 na CI

**Estado:** implementação técnica submetida a PR para QA independente; não é homologação científica nem encerramento da Etapa 05. Documento operacional de 10/11/2026; não reconstitui a execução histórica.

## Contrato e escolha do gate

- A CI adiciona o job **Python 3.12.8 — PNAD E10 public N2**, em ubuntu-24.04, com actions/setup-python@v6 e Python exatamente 3.12.8; o próprio job verifica a versão em execução.
- Dependências externas N2: **nenhuma**. A auditoria usa a biblioteca padrão Python (csv, hashlib, collections, pathlib, runpy, unittest). Não há pip install. Os cinco pacotes de scripts/pnadc_estudo_i/requirements-historicos.txt têm versões fixadas (pandas 2.2.3, numpy 2.3.5, scipy 1.17.0, matplotlib 3.10.8, arviz 1.1.0) e **não são dependências de N2**; não são instalados nem implicam N3/N4.
- Os testes adversariais precedem a auditoria real; qualquer erro produz saída diferente de zero. O job R já exigido pelo ruleset #26061035 utiliza needs: pnad_n2 e if: always(), com passo inicial que falha se N2 não retornar success. Assim, o check R obrigatório não pode ficar verde quando o job Python falha, sem alterar o ruleset.
- O teste R original da T04.6 ainda pode saltar Python dentro do contêiner R. O novo job Python é obrigatório e independente desse skip.

## Falhas adversariais e Python otimizado

O auditor histórico auditar_e10.py depende de assert; python -O e PYTHONOPTIMIZE podem eliminar todas as verificações. Seu blob Git histórico a8aa628b0cad4321811acd283819203c69b493f7 está referenciado pela matriz E10 e por testes R. A T05.2 o preserva **literalmente**; o novo executar_auditoria_n2.py bloqueia a execução se sys.flags.optimize != 0, com código 2, antes de chamar o auditor. Uma inconsistência ou exceção no caminho normal resulta em código 1. **Invocar diretamente o arquivo histórico com -O fora do gate segue inseguro**; a correção de CI não deve ser confundida com alteração retroativa do legado.

A suíte testa: saída positiva do CSV público imutável, reprovação por adulteração do SHA, Delta incompatível com os valores mesmo após recalcular o SHA, cardinalidade alterada com SHA recalculado, e rejeição com python -O e PYTHONOPTIMIZE=1. Alterações adversariais afetam exclusivamente cópias em diretórios temporários; não há alteração de dados versionados ou inspeção de bases institucionais.

## Comandos e escopo

- python3 -m unittest discover -s tests/python -p 'test_pnad_n2.py' -v
- python3 .github/scripts/executar_auditoria_n2.py
- python3 -O scripts/pnadc_estudo_i/executar_auditoria_n2.py (esperado: exit 2)

**N2, caso aprovado:** somente integridade SHA-256 da projeção pública E09 (8bd38ce75917d981431f4c2a9c09ba73648ff1151fd6ac717068637b682fe120), 13 indicadores, cinco eixos, 26 contrastes, períodos e deltas descritivos. **Não comprova:** extração SIDRA, E04–E08, 52 cadeias Stan/MCMC, PPC, valores CV, 104 probabilidades E08.2, posterior temporal conjunta, inferência, N3/N4, licenças, privacidade ou autorização de publicação. CSV e rótulos oficiais são os mesmos; nenhum estimando ou resultado científico é recalculado.

**Gates pendentes:** T05.2 QA não realizado até parecer independente no HEAD final; T05.3–T05.7 pendentes. T01.3–T01.7/Issue #14, Issues #15/#16, D00-01–D00-06 e ressalvas T04.4 R1–R4, T04.5 R1–R3, T04.6 R1–R3, T04.7 e T05.1 R1–R4 permanecem com seus estados anteriores.

**Correção de CI no PR:** o primeiro run no HEAD 029dd796 falhou no teste de inventário T05.1 porque o wrapper somava uma 80ª fonte em scripts/ sem integrar o inventário histórico fixado. Por isso o wrapper técnico foi deslocado exclusivamente para .github/scripts/, sem alterar relatórios T05.1 ou código científico; o HEAD novo exige novo CI e QA. A primeira execução Python N2 havia passado; o check R tinha falhado por deriva de universo, não por resultado científico.
