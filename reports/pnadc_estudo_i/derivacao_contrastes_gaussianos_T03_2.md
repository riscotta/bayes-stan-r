# T03.2 — Derivação da aproximação gaussiana dos contrastes temporais (PNAD Contínua, Estudo I)

**Natureza:** formalização matemática e auditoria estática do método já presente em `e08/sensibilidade_e082.py`. **Não** constitui reamostragem, ajuste multivariado novo, estimativa de correlação, validação do desenho da PNAD ou revisão dos resultados históricos.  
**Linha de base consultada:** `main` `2dbbb0cac81a4bee302fd2ecaffd300b41ce219e` (06/11/2026).  
**Escopo:** os 26 pares indicador–janela definidos em E05/E08 (13 indicadores × anual 202509→202609 ou recente 202606→202609). Referência complementar: [inventário E07 T03.1](inventario_estimandos_e07_T03_1.md).

## 1. Quantidade, aproximação e condições

Para um indicador e dois períodos `0` (origem) e `1` (destino), `y_t` é a estimativa **agregada publicada** e `theta_t` seu valor latente. O código define a escala aproximada `s_t = |y_t| CV_t / 100`, com CV em porcentagem. Definimos o contraste `Delta = theta_1 - theta_0` e sua diferença central observada `d = y_1 - y_0`.

A hipótese **adicional**, não identificada pelos dados públicos disponíveis, é que o vetor `(theta_0, theta_1)` seja descrito, **para fins do cenário**, por uma distribuição normal bivariada centrada em `(y_0, y_1)`, com matriz de dispersão

```text
Sigma(rho) = [       s_0^2       rho * s_0 * s_1 ]
             [ rho * s_0 * s_1        s_1^2     ] .
```

A centralização e as variâncias `s_t^2` **não são** resultados dos ajustes Stan marginais E07: constituem uma aproximação operacional às incertezas dos agregados. Interpretá-la como distribuição **posterior** exigiria um modelo bivariado com likelihood, prior e desenho amostral explicitados, e uma justificativa para a correspondência entre `s_t` e desvio posterior — nada disso está identificado nesta implementação. Como distribuição *amostral* de erros, também seria necessário justificar a aproximação normal e as covariâncias de desenho. Não chamar automaticamente os intervalos abaixo de intervalos credíveis ou oficiais do IBGE.

**Pressupostos matemáticos mínimos:** `s_0 > 0`, `s_1 > 0` e `-1 < rho < 1` tornam a matriz de covariâncias definida positiva. São hipóteses sobre a dependência entre **dois períodos de um indicador**, não sobre a dependência entre indicadores/eixos. A PNAD usa desenho complexo e trimestres móveis: a matriz acima é uma **família de cenários**, não covariância observada, estimada ou identificada. Validade dos CVs, suporte e efeito do arredondamento permanecem para T03.4.

## 2. Derivação algébrica de Var(Delta | rho)

Escrevendo `a = (-1, 1)^T`, então `Delta = a^T (theta_0, theta_1)^T` e, pela identidade de variância de uma combinação linear,

```text
Var(Delta | rho, s_0, s_1)
  = a^T Sigma(rho) a
  = Var(theta_1) + Var(theta_0) - 2 Cov(theta_1, theta_0)
  = s_1^2 + s_0^2 - 2 rho s_0 s_1.
SD(Delta | rho) = sqrt(s_0^2 + s_1^2 - 2 rho s_0 s_1).
E(Delta | rho) = y_1 - y_0 = d.
```

Dessa hipótese decorre **apenas condicionalmente ao cenário** `Delta ~ Normal(d, SD(Delta | rho)^2)`. Para `SD > 0` e `Phi` a função de distribuição acumulada normal padrão:

```text
P_cenario(Delta > 0 | rho) = Phi(d / SD(Delta | rho)).
P_cenario(movimento favoravel | rho) = Phi(k * d / SD(Delta | rho)),
  com k = +1 quando maior é favorável; k = -1 quando menor é favorável.
Faixa normal central de 95% = [d - z_0.975 SD, d + z_0.975 SD],
  z_0.975 = 1.959963984540054.
```

Os nomes `IC95Inf` e `IC95Sup` são mantidos **como rótulos do código histórico**, mas não se atribui cobertura de desenho ou credibilidade bayesiana verificada a esses valores. São limites normais condicionais ao cenário. O sentido `menor` usa `1-Phi(d/SD)`, equivalente a `Phi(-d/SD)`.

## 3. Papel de rho e limites da sensibilidade

- `rho = 0`: independência **postulada** entre os dois termos da aproximação, e `Var(Delta)=s_0^2+s_1^2`. Não é um fato demonstrado para trimestres móveis.
- `rho > 0`: reduz a variância do contraste; `rho < 0`: aumenta. A identidade `d Var(Delta)/d rho = -2 s_0 s_1 < 0` vale com escalas fixas e positivas.
- O código testa exatamente `rho = -0.5, 0, 0.5, 0.9`; **nenhum** desses valores é inferido de dados, e o intervalo `[-0.5,0.9]` não é intervalo de confiança/credibilidade para rho.
- Para `rho` se aproximando de `+1`, `SD` tende a `|s_1-s_0|`; para `rho` se aproximando de `-1`, tende a `s_1+s_0`. Esses são **limites algébricos**; `rho=±1` gera matriz singular, não incluída nos quatro cenários. Se `s_0=s_1` e `rho→1`, a variância tende a zero: não se deve interpretar uma probabilidade computada com desvio zero como posterior regular.
- Quando `k*d>0`, reduzir o desvio aumenta a probabilidade condicional de movimento favorável; quando `k*d<0`, reduz essa probabilidade; quando `d=0` e `SD>0`, resulta `0.5`. Assim **não** é correto afirmar que rho positivo aumenta sempre a probabilidade de melhoria.
- Nem a matriz temporal de um único indicador, nem os seus limites em rho, definem uma distribuição conjunta para os 13 indicadores ou uma probabilidade de melhora simultânea nos cinco eixos.

## 4. Exemplo PNAD e conferência sintética complementar

**Primeiro, o dado publicado:** na projeção descritiva E09, `desocupacao_pct` no contraste **recente 202606→202609** passa de **5,4% para 5,2%**, logo `d=y_1-y_0=-0,2` ponto percentual e a direção favorável para desocupação é `k=-1`. Para esse par, a expressão pertinente é `Var(Delta | rho)=s_0²+s_1²-2rho*s_0*s_1` e, **apenas se fossem fornecidos e justificados** `s_0,s_1>0`, a probabilidade condicional seria `Phi(0,2/sqrt(s_0²+s_1²-2rho*s_0*s_1))`. Os CVs desse par e as covariâncias de desenho **não estão no CSV público**: não se calcula aqui `s_0`, `s_1`, desvio do contraste, probabilidade ou intervalo. O rótulo descritivo da projeção é `caiu`, não um resultado dessa fórmula.

**Complemento exclusivamente sintético para verificar a álgebra (não são dados da PNAD):** se `y_0=10`, `y_1=13`, `s_0=2`, `s_1=1`, então `d=3` e `Var(Delta)=5-4rho`.

| rho hipotético | Var(Delta) | SD(Delta) aprox. | P_cenario(Delta > 0) aprox. |
| ---: | ---: | ---: | ---: |
| -0.5 | 7.0 | 2.645751 | 0.871580 |
| 0 | 5.0 | 2.236068 | 0.910144 |
| 0.5 | 3.0 | 1.732051 | 0.958368 |
| 0.9 | 1.4 | 1.183216 | 0.994385 |

Este exemplo verifica somente a álgebra da aproximação condicional. Não quantifica o efeito real da sobreposição, dos pesos amostrais ou da inferência marginal de E07.

## 5. Correspondência com o código e fronteiras de evidência

| Código histórico `sensibilidade_e082.py` | Relação com a derivação | Estado |
| --- | --- | --- |
| `df['s'] = df.Valor.abs()*df.CVPercentual/100` | `s_t=|y_t| CV_t/100` | Fonte estática inspecionada; entradas locais não reexecutadas |
| `delta = y1-y0` | Diferença dos valores publicados | Descritiva, não contraste de draws Stan |
| `sd = np.sqrt(s0*s0+s1*s1-2*rho*s0*s1)` | `SD(Delta | rho)` | Identidade derivada acima |
| `norm.cdf(delta/sd)` e escolha `pdir` | Probabilidades de sinal, condicionais | Não posterior conjunta identificada |
| `IC95Inf`, `IC95Sup` | Faixa normal central condicional | Não comprova cobertura/credibilidade reais |
| `sd_min=abs(s1-s0)`, `sd_max=s1+s0` | Limites algébricos `rho→±1` | Extremos singulares não amostrados |

E07 ajusta separadamente `theta` **somente para 202609**; E08.2 consome resumo E07 apenas para checagem marginal, e não usa draws E07 para montar os 26 deltas. E09 consolida E08.2 sem novo ajuste. Os scripts históricos **não são modificados** por T03.2. A auditoria de rastreabilidade de probabilidades pertence a T03.3; a adequação de suporte/CV/arredondamento/sobreposição/covariâncias pertence a T03.4; modelos alternativos e revisão científica pertencem a T03.5–T03.6; testes de invariantes e cobertura mais ampla pertencem a T03.7.

## 6. Condição de QA desta tarefa e riscos não resolvidos

O QA independente deverá: (i) conferir derivação, sinais e fator `2rho s_0 s_1`; (ii) confrontar os quatro valores de rho, fórmulas, sinal `pdir` e limites com o código histórico; (iii) inspecionar teste sintético, diff e CI **no SHA exato**; (iv) confirmar ausência de mutação em dados, modelos e conclusões; (v) manter explícita a falta de covariâncias reais, de draws temporais conjuntos, de reprodução E04–E09 e de homologação P0 (Issue #14). **Autoinspeção e CI verde não substituem parecer independente.** Nenhuma das tarefas T03.3–T03.7 é declarada concluída aqui.
