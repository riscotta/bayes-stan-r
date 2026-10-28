# Matriz de evidências da publicação (28/10/2026)

**Estudo exploratório, dados territoriais provisórios; não são indicadores oficiais.**

| Etapa | Artefato | Status verificável |
| --- | --- | --- |
| E04 | Base derivada 51.840 células | Auditada em Python E10, mas **não versionada** |
| E07 | Poisson/NB2 quatro folds, 8 modelos | R executado historicamente; código publicado; **não reajustado nesta CI** |
| E08 | Diagnósticos e bootstrap 96 distritos | Reproduzido em Python E10; R integrado não reexecutado com E04 nesta CI |
| E09 | Oito tabelas e quatro gráficos | Reproduzidos em Python E10; **auditoria pública** verifica identidades |
| E10 | Caderno e scripts R | Registro histórico preservado; CI usa somente indicadores publicados |
| Proveniência | APVP/PRO-AIM, SEADE, crosswalk | **Fontes originais e limites distritais históricos não homologados** |

A diferença observada menos prevista pela NB2 **não** é estimativa causal do efeito da COVID-19.
O bootstrap de E08 expressa variabilidade da **cobertura das células**, não do total agregado previsto.
A licença do código não confere licença sobre insumos de terceiros.

O manifesto histórico abaixo refere-se **exclusivamente aos caminhos e bytes do ZIP E10 original**, e não aos arquivos modificados deste repositório. SHA-256 do ZIP E10 original: `a1fe9d9e0a2c9a8c2274a6d09508b5d93d203a689f309307dadb7eb2803215ba`.
