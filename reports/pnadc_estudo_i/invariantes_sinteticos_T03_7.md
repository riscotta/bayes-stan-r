# T03.7 — Invariantes e casos sintéticos: E07–E09 (PNAD Contínua, Estudo I)

**Tarefa literal:** “Adicionar testes de invariantes e casos sintéticos sem modificar os resultados históricos sem justificativa”. **Baseline:** `main` `1090943e9b64590fdd0d2ad95ccdda490b3282fc`, 09/11/2026, após squash PR #35. **Tipo:** testes automatizados e documentação, sem alteração de dados, código inferencial, parâmetros, priors, likelihood ou artefatos de resultados.

## 1. Contratos e evidências associadas

| Origem | Invariante/ensaio executável em R | O que não prova |
| --- | --- | --- |
| E08.2 | Matriz `Sigma(rho)` simétrica definida positiva para quatro `rho` postulados, identidade de variância da diferença, valores analíticos independentes para `y0=10,y1=13,s0=2,s1=1`, complemento entre direções e probabilidade 0,5 em `Delta=0` | Não estima `rho`, covariância de desenho nem posterior temporal |
| E07 Stan e E08.2 | Domínios de `theta` taxa [0,100], contagem [0,P14], renda >=0; escala `s=abs(y)*CV/100`, invariância de escala e fronteira `y=0` com `CV>0` | Não compila Stan, não faz PPC, nem valida adequação dos modelos à PNAD |
| E08.3 | Intervalos determinísticos de arredondamento; grade de 16 vértices com dois valores e dois CVs perturbados; probabilidades finitas entre 0 e 1 em caso sintético | Os 16 vértices não são extremos globais garantidos no espaço contínuo nem distribuição estatística de erros |
| E08.4 | Limites de Fréchet `max(0,sum(p)-4)` e `min(p)` para **cinco eventos proxy**; exemplo `p_i=0,9` => [0,5;0,9] | Não identifica dependência conjunta, nem a probabilidade de melhoria dos cinco eixos |
| Projeção pública E09 | Leitura sem escrita de 26 contrastes descritivos, 13 indicadores, 2 janelas, identidade de `Delta`, ausência de campos probabilísticos e seleção `rho=0` no fonte de consolidação | Não executa E09 integral e não reproduz os insumos históricos ausentes |

**Arquivo executável:** `tests/testthat/test-pnadc-invariantes-sinteticos-t03-7.R`, invocado pelo runner existente `Rscript tests/run_tests.R`; workflow `.github/workflows/ci.yml` executa a mesma suíte sobre R 4.5.1. Não altera CI nem suas dependências.

## 2. Hipóteses explícitas

Os casos gaussianos são **hipóteses matemáticas sintetizadas** com escalas estritamente positivas e `|rho|<1`; cada cenário fixa sua própria matriz, sem inferência sobre `rho`. O teste de `CV>0, y=0` evidencia que `s=0` permanece possível: é **fronteira de domínio**, não observação de erro na PNAD. O modelo Stan restringe o parâmetro `theta`, mas sua réplica `y_rep ~ Normal(theta,s)` não fica necessariamente restrita ao mesmo suporte. Não se deduz independência amostral de trimestres que não compartilham meses, dada a rotação 1-2(5).

As funções R usadas como oráculos recalculam **identidades sintéticas** sem importar ou executar os scripts Python originais E08.2–E08.4, cujos módulos acessam arquivos históricos em importação. Referências textuais aos fontes fornecem guardas de rastreabilidade, **não cobertura funcional Python**. Esses testes podem sofrer falsos positivos se a lógica Python mudar preservando os trechos textuais consultados. A execução dos testes da CI não substitui teste científico de inferência, recuperação de parâmetros, sensibilidade ou cobertura.

## 3. Aceitação, QA e rastreabilidade

- Revisar diff integral (somente teste, relatório e referência no README) e confirmar que `data/`, `estudo_I/` e `outputs/` não foram modificados.
- Conferir, no **SHA final do PR**, parsing R, suíte estrutural e logs do check obrigatório `R 4.5.1 — syntax and tests`. Caso qualquer teste falhe, **não presumir** execução bem-sucedida.
- QA técnico-científico **em outro chat**, distinto do executor, deve inspecionar o teste, recalcular casos sintéticos de forma independente, verificar falsos positivos e confrontar E07–E09 e os documentos T03.1–T03.6.
- Emitir parecer sobre o HEAD exato. Um parecer favorável, CI verde e verificação da proteção da `main` habilitam integração por squash autorizada; **este relatório não contém aprovação de QA**.
- Após integração, verificar commit de squash e CI `push/main`, atualizar o Word mestre e encerrar a Etapa 03 apenas se todos os gates da etapa estiverem cumpridos.

**Riscos não resolvidos:** E07 posterior apenas marginal no período 202609; E08/E09 contrastes condicionais a `rho` não identificado; ausência de covariâncias temporais/interindicadores, insumos históricos E04/E07/E08 completos e validação da cobertura pelo desenho amostral. Etapa 01/P0 T01.3–T01.7 e Issue #14 permanecem abertas; Issues #15/#16 também. Testes sintéticos não autorizam redistribuição de dados nem reclassificação de cenários exploratórios como posterior temporal.

**Fontes dentro do repositório:** [T03.2](derivacao_contrastes_gaussianos_T03_2.md) · [T03.4](auditoria_suporte_cv_arredondamento_T03_4.md) · [T03.5](comparacao_alternativas_inferenciais_T03_5.md) · [T03.6](rotulos_inferenciais_T03_6.md) · [Nota científica](nota_cientifica.md).
