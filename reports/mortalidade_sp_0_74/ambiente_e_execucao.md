# Ambiente e verificação do estudo E07–E10

- **Execução histórica E07 (confirmada em registro recebido):** R 4.5.1 (Windows 11 x64, plataforma x86_64-w64-mingw32); MASS 7.3-65; fuso America/Sao_Paulo.
- **Nova CI:** imagem `rocker/r-ver:4.5.1` (Linux); sintaxe R, testes estruturais e modo público `auditar` que reproduz resumos/gráficos congelados.
- **Modo `conferir`:** R base e a base E04 + saídas R E07 locais. Os 8 CSVs E09 são recalculados e comparados.
- **Modo `completo`:** R/MASS, 8 ajustes Poisson/NB2 para 4 cortes temporais, diagnóstico E08 e E09, sem reconstituir dados desde APVP/SEADE.
- **Python histórico E10:** E08 e E09 efetivamente reexecutadas e auditadas a partir das entradas E07 preservadas. Não afirmar que a CI R testa os modes dependentes dos dados externos.
- **Limitações:** a base distrital histórica e a proveniência original não foram homologadas; nenhum coeficiente tem interpretação causal.
- **Integridade:** o manifesto em `MANIFESTO_E10_ORIGINAL_SHA256.csv` é uma fotografia do pacote E10 original, **não** um hash canônico do repositório GitHub adaptado.

O arquivo congelado `E07_R_validacao_anual.csv` apresenta 14 linhas (sete anos para Poisson e NB2). Os resumos publicados não incluem calibração distrito×ano ou sexo×idade×ano, necessários para executar E08 integralmente no modo `conferir`.
