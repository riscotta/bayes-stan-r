# T08.3 — Verificação documental

Base auditada: `17181eca17b4bd4090a14e283dccbabd39b03c57`, árvore `5fe8661c483e0543a4c56af628c0d6a48de335d8`. Registro E1. Preview nativo, CI candidata e QA independente devem ser aferidos no PR.

## Inventário e limites

A árvore contém 622 entradas, 463 blobs e 115 arquivos Markdown. A triagem reteve 21 origens por risco e 14 por possível evidência histórica. As 80 origens elegíveis produziram 1.155 ocorrências no parser local GFM.

| Categoria | Ocorrências | Destinos distintos na categoria |
| --- | ---: | ---: |
| Existência relativa na árvore | 822 | 104 |
| Destino retido, só metadados | 174 | 18 |
| URL do repositório no GitHub, em fonte | 111 | 41 |
| URL externa, em fonte | 11 | 10 |
| Âncora em Markdown elegível | 33 | 33 |
| Fragmento em destino conferido por metadados | 4 | 4 |

Os totais de destinos por categoria podem se sobrepor. A auditoria histórica confirmou 109 ocorrências para 40 destinos do GitHub. A diferença de cobertura em duas ocorrências e as URLs externas seguem sem confirmação neste corte. As 33 âncoras do índice do catálogo apareceram com destinos no Preview nativo da base. O parser local cobre a sintaxe que reconhece e não comprova renderização de todo Markdown, direitos ou disponibilidade remota.

## Comandos e correções

197 ocorrências triadas por origem e posição. Foram cotejados parser/argumentos, contexto, pré-requisitos e efeitos observáveis nos entrypoints elegíveis. Estados estáticos incluem menção, exemplo, condição de ambiente e insumos, rota suspensa e valores não verificáveis. O registro por posição fica nas evidências de E1, com destinos protegidos redigidos. A resposta de serviços externos a parâmetros históricos e valores dinâmicos dependentes de custódia seguem inconclusivos. Nenhum comando auditado foi executado.

- Raiz: 46 substituições de delimitador U+00A7 por U+0060 em 16 linhas. Links e comandos preservados.
- ISUS/SIA: documentada a precedência do CSV padrão ou indicado e do CSV de fallback sobre um ZIP explícito, sob autorização pontual. A fonte e os comandos permanecem iguais.
- FN permanece aplicável. No RS Seguro, a unidade são contagens mensais de ocorrências com uma vítima por sexo, faixa e crime. A avaliação não infere perfil individual, anonimização ou direitos.

## Teste e CI

Suíte Python de biblioteca padrão, fixtures sintéticas e regressão sobre o README raiz. Teste local em checkout esparso da base, Python 3.12.14: 36/36 no candidato, exit 0. Com a versão original do README, houve uma falha esperada `DELIMITADOR_CORROMPIDO`, exit 1. As baselines positivas precedem os casos adversariais associados.

A minuta de CI acrescenta somente a invocação da suíte ao job Python existente, após T08.2. As verificações preexistentes incluem PNAD N2 público, sintaxe Stan, testes R e auditorias públicas/agrupadas, sem liberação de rotas suspensas. A nova suíte não segue links, executa comandos encontrados ou lê evidência protegida. A comprovação de CI candidata depende do HEAD e da árvore de checkout aferidos no PR.

## Complemento sintético de renderização

A amostra real fixada para o Preview abrange os READMEs raiz, de Mortalidade SP, hierárquico, RS Seguro, PNAD e ISUS/SIA, o catálogo e GSEQ, respeitada a elegibilidade de renderização. Este complemento usa somente conteúdo sintético.

<details>
<summary>Exemplo sintético expansível</summary>

Texto sintético com H<sub>2</sub>O e x<sup>2</sup>.

</details>

### Fórmulas de exemplo

Inline: $x^2 + y^2 = 1$.

$$
\sum_{i=1}^{3} i = 6
$$

- [Âncora interna da fixture](#fórmulas-de-exemplo)

```text
conteúdo literal, sem execução
```
