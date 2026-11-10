# T05.3 — Validação obrigatória de fontes Stan representativas na CI

**Escopo literal:** Adicionar validação de sintaxe/compilação para modelos Stan representativos.
**Base:** main 4c660b8fbe236f51b4c2d8ea4156fe996a386f10 (10/11/2026).
**Implementação:** branch task/t05-3-stan-stanc3-sintaxe; QA independente e merge pendentes.

## Inventário e seleção

O inventário versionado reports/t05_1/inventario_scripts.csv registra **20 arquivos Stan em nove unidades**. Foram selecionados cinco (25% dos arquivos, cinco unidades diferentes), privilegiando famílias e estruturas distintas, sem selecionar por resultados de amostragem:

| Unidade | Fonte exata | Cobertura do gate |
|---|---|---|
| PNAD estudo I | scripts/pnadc_estudo_i/estudo_I/e07/stan/contagem_marginal.stan | Contagem, restrições, gerados |
| Cães F5 | scripts/caes_raca_f5/original/E08_MP_Entregaveis/02_codigo/stan/M1_skew_normal_E08_MP.stan | Skew-normal, parâmetros transformados |
| Exercises | scripts/exercises_dataset/modelo_ordinal_dificuldade_sem_equipamento.stan | Modelo ordinal e efeitos hierárquicos |
| World Bank | scripts/importacoes_world_bank/modelo_principal_hierarquico_student_t.stan | Regressão hierárquica Student-t |
| Porto Alegre | scripts/sobrevivencia_cadastral_poa/modelo_M0_rw2_centrado_qr.stan | Função do usuário, QR e risco de evento |

As outras 15 fontes e quatro unidades não são cobertas por esta seleção. O teste de manifesto confirma cardinalidade e associação ao inventário T05.1, mas não prova a compatibilidade dos demais programas Stan.

## Ferramenta, reprodução e custo

- Job próprio **Stan 2.40.0 — representative syntax**, Ubuntu 24.04; Python 3.12.8 para orquestração em biblioteca padrão, sem instalação de CmdStan, R/cmdstanr ou datasets externos.
- stanc3 **v2.40.0** (release estável), binário linux-stanc de stan-dev/stanc3, conferido por SHA-256 **29306a35092c7f6c1aa20df903ddb9dcd83af5995ba37630324a2e019d05e6f4** antes da execução.
- Comando por modelo: stanc --O0 --o=ARQUIVO_TEMPORARIO.hpp FONTE.stan. Exige exit 0 e saída C++ não vazia em pasta temporária.
- Cenário negativo: arquivo Stan temporário com falta deliberada de ponto e vírgula. O gate falha se o compilador **não** rejeitar a entrada inválida.
- Testes Python com executável falso exercitam controle de versão, ausência de fonte, teste negativo e inventário; o job executa também o binário real contra os cinco modelos.
- O job validate (contexto protegido **R 4.5.1 — syntax and tests**) depende de pnad_n2 e stan_syntax; se qualquer gate falha, o contexto obrigatório R falha explicitamente. O ruleset #26061035 não foi alterado.
- Timeout do job: dez minutos. O binário Linux demanda download de cerca de 5,7 MB; rede indisponível ou checksum divergente geram falha explícita, não skip.

## Critérios de aceite

1. Cinco fontes presentes no inventário versionado e no checkout.
2. Binário stanc3 com versão exata e checksum verificado.
3. Tradução Stan→C++ das cinco fontes com exit 0 e saída não vazia.
4. Sintaxe adversarial rejeitada com status não zero.
5. Cinco testes Python estruturais aprovados.
6. CI do PR verde no HEAD exato; sem mascarar falha Stan pelo check R obrigatório.
7. QA independente com revisão de diff, logs, escopo, modelos e riscos, antes de merge.

**Limites:** sucesso de stanc3 demonstra análise sintático-semântica e geração de código C++; **não** compila o C++ nativamente, não executa CmdStan, não ajusta MCMC, não valida matemática, priors, verossimilhança, identificabilidade, recuperação de parâmetros, PPC, SBC, predição ou inferência científica. A compilação C++ nativa fica pendente de trabalho específico, sem confundir esta checagem com T05.4. Nenhum modelo, dataset, script científico, auditor E09/E10 ou resultado histórico foi alterado.

**Pendências preservadas:** T05.2 R1 (caminho documental incorreto), R2 (alcance N2), T05.4–T05.7, Etapa 05, Issues #14–#16, P0 T01.3–T01.7 e D00-01–D00-06. Implementação aberta para QA independente.
