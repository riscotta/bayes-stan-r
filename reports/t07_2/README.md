# T07.2 — Integridade dos códigos originais por SHA-256

**Tarefa literal:** “Preservar o código original imutável e comprovar sua identidade por hash e manifesto.”

**Baseline fixo:** commit da `main` **`c0fab905ed1bd3ca61926885ca1eed32f8470269`** (17/11/2026, integração T07.1, PR #58). O manifesto foi construído lendo bytes dos blobs desse commit no GitHub, confrontando cada arquivo com seu identificador de blob e tamanho. Não se calculou hash de fontes baixadas de provedores externos e não se executou qualquer script científico.

## Contrato de escopo

- **Incluídos:** todos os caminhos regulares sob `scripts/` terminados exatamente em `.R`, `.py`, `.ps1` ou `.stan`, recursivamente; **79 arquivos** (59 scripts executáveis catalogados na T07.1, mais 20 fontes/modelos Stan). Incluem os scripts sob `scripts/_setup/` e `scripts/caes_raca_f5/original/`.
- **Excluídos explicitamente:** README/Markdown, JSON de aprovação/configuração, dados, resultados, imagens, CSV, arquivos de projeto, testes e workflows fora de `scripts/`, além de outras extensões não listadas. A exclusão não constitui declaração de segurança, imutabilidade ou ausência de valor histórico desses itens. A issue #14/P0 continua bloqueando afirmações sobre proveniência/licença e redistribuição de dados.
- **Caminhos:** relativos à raiz Git, separados por `/`, sem `.`, `..`, barra invertida ou duplicatas; ordenação lexicográfica; sem links simbólicos no diretório `scripts/`. Novos arquivos com extensão incluída são **reprovados** se não constam da baseline. Arquivos removidos também reprovam.
- **Bytes:** hash SHA-256 do conteúdo binário bruto, sem normalizar fim de linha, encoding ou espaços. Colunas: `path,sha256,bytes,git_blob_sha1`. O identificador SHA-1 do blob Git é auxiliar e **não substitui** SHA-256, pois SHA-1 tem limitações criptográficas conhecidas.
- **Âncora externa:** `--check-baseline` lê a árvore e os 79 blobs do commit fixado com `git ls-tree` e `git cat-file`, confere escopo, tamanho, SHA-256 e SHA-1. O GitHub Actions busca explicitamente o commit necessário na clone rasa. O modo padrão também confronta todos os arquivos existentes em `scripts/` com o manifesto.

## Verificação

Na raiz do repositório:

```bash
python -m unittest discover -s tests/python -p 'test_originais_t07_2.py' -v
python tools/verificar_originais_t07_2.py
git fetch --no-tags --depth=1 origin c0fab905ed1bd3ca61926885ca1eed32f8470269
python tools/verificar_originais_t07_2.py --check-baseline
```

Todos os comandos de verificação são **somente leitura em `scripts/`**. O `git fetch` atualiza somente objetos do banco local do Git; não modifica código-fonte nem roda modelos. A CI da PR executa nove cenários unitários, incluindo modificação, remoção, inclusão não inventariada, hash adulterado, hash malformado, caminho não canônico e duplicata, além de conferir o manifesto completo contra a árvore atual e o commit-base.

## Regras de atualização e limites

Não regenerar o manifesto automaticamente em CI, nem aceitar um novo baseline apenas porque um script mudou. Alterações futuras no código original requerem autorização científica/governança específica, uma nova versão do manifesto com justificativa, evidência da alteração e QA independente sobre o novo SHA. O teste impede mudanças acidentais **dentro do escopo definido**, mas não aplica travamento físico ao Git nem valida semântica, resultados históricos, equivalência numérica, direitos autorais, amostragem N3/N4 ou ausência de efeitos colaterais dos scripts. Check verde não significa aprovação científica.

Em ambientes Windows, desligar a transformação de final de linha no checkout (`core.autocrlf=false`) ou usar Git/CI em sistema que preserve os bytes originais, pois uma conversão CRLF altera corretamente o SHA-256 observado. A T07.2 não modifica nenhum arquivo em `scripts/`; o inventário T07.1 continua independente e suas ressalvas permanecem válidas. A Etapa 07 continua aberta até T07.3–T07.7 e respectivos gates.
