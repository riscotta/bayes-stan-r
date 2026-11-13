# T06.5 — Dependências externas, lacunas e caminhos históricos

**Tarefa literal:** “Registrar dependências ausentes dos estudos com dados externos, incluindo os caminhos históricos.”

**Corte documental:** 13/11/2026. **Base Git inspecionada:** `f5f782d19bebc0a09ef08831982d601569e33d95` (após T06.4). **Natureza:** inventário N1 documental; não executa modelos, não baixa insumos, não avalia privacidade dos dados institucionais e não homologa origem ou direitos.

## 1. Entrega e contagens reproduzíveis

O arquivo [dependencias_externas_T06_5.json](dependencias_externas_T06_5.json) cobre as **33 unidades da taxonomia T02.1** a partir do [manifesto T06.1](../t06_1/manifesto_estudos_T06_1.json), [catálogo de proveniência](../../data/DATA_PROVENANCE.csv) e READMEs existentes:

- **30/33 estudos** com ao menos uma dependência externa catalogada ou citada sem catálogo; os demais 3 (**deck_15wins**, **hidrometro_degradacao**, **media_ruido_mu**) não apresentam vínculo externo *nesse recorte documental*. Isso **não prova inexistência** de outras dependências de execução ou de histórico.
- **27 vínculos estudo–dataset** catalogados (25 estudos com catálogo) e **8 referências externas não catalogadas** distribuídas por 5 estudos (coortes de aposentadoria, FruitFlies, inhaler, IPCA e kidney).
- **36 referências de caminho, nome local, diretório legado ou URL** registradas por estudo. Não são 36 arquivos ausentes: algumas referências já estão no Git, outras são diretórios e outras dizem respeito a saídas históricas ou serviços online.

A mesma fonte pode abastecer **vários estudos**; por exemplo, `retractions_time_to_retraction` é compartilhada entre três estudos de retratações. Contar 27 vínculos não equivale a contar 27 datasets distintos. Fontes externas de pacotes R (por exemplo `brms`, `survival`, `Stat2Data`) não são automaticamente arquivos CSV ausentes ou passíveis de redistribuição.

## 2. Como ler o estado

Cada registro identifica `id`, README, dependência, `dataset_id` quando há catálogo, origem e URL *declaradas*, escopo catalogado, status histórico de licença/redistribuição, estado **restrito à árvore Git**, referência documental e a lacuna ainda a comprovar. As referências históricas separam literalmente **nome**, **arquivo relativo**, **cache**, **saída legada** e **URL**, quando cabível.

- `ARQUIVO_VERSIONADO_NESTE_SNAPSHOT` = objeto Git nesse caminho; não certifica custódia científica, integridade da extração, autorização ou versão original.
- `DIRETORIO_COM_ENTRADAS_VERSIONADAS_NESTE_SNAPSHOT` = há objetos Git abaixo do prefixo, que podem ser documentação, placeholders, agregados ou resultados, sem fonte bruta.
- `NAO_LOCALIZADO_NA_ARVORE_GIT_DO_SNAPSHOT` = não encontrado **naquela árvore**, jamais declaração de arquivo historicamente inexistente.
- `REFERENCIA_RELATIVA_OU_NOME_NAO_VERIFICADO_NO_GIT` = caminho legado relativo ou nome extraído do documento, sem reconciliação com um arquivo local.
- `URL_EXTERNA_NAO_TESTADA` = referência de serviço online, sem download, acesso ou confirmação de versão.

Os caminhos catalogados vêm da T06.1. Referências históricas foram cotejadas com READMEs e, quando indicado expressamente no JSON, com `DATA_PROVENANCE.csv`. **Apenas existência/ausência na árvore Git foi inspecionada; presença no computador do pesquisador e hashes externos não foram verificados.** Nomes isolados de arquivos da mortalidade SP não são transformados em caminhos absolutos presumidos.

## 3. Lacunas prioritárias registradas

| Estudo | Dependência / referência histórica | Falta comprovar |
|---|---|---|
| PNAD Contínua Estudo I | Snapshot SIDRA de 22.130 linhas, metadados, E04–E08 e 52 cadeias; saídas legadas `estudo_I/e04/saida`, `estudo_I/e05/saida`, `estudo_I/e08/` | Origem/vintage, manifesto de transporte, coletor original, disponibilização legítima, reconciliação de intermediários e cadeias |
| Mortalidade SP 0–74 | `base_analitica_celular_PROVISORIA.csv`, `obitos_APVP_2006_2023_0a74.csv`, `populacao_SEADE_2006_2023_0a74_LONG.csv`, `crosswalk_distritos_PROVISORIO.csv` e intermediários E07 | Fontes APVP/SEADE, correspondência territorial histórica, permissões, E04 e E07 sem reexecutar automaticamente |
| Cães F5 | `data/raw/caes_raca_f5/base_F5_principal_E04.csv` não versionada; referência do README a SHA-256 histórico | Custódia de E04, origem derivada, integridade e correspondência do ambiente CmdStan 2.37.0 |
| RS Seguro | Dois extratos versionados; camada opcional `data/raw/rs_seguro/rs_month_crime.csv` sem objeto no snapshot | Permissão institucional e vínculo comprovável entre extratos locais e fonte oficial |
| Censo Escolar | Cache `data/raw/censo_escolar/` com ZIPs INEP, não versionados | Versão efetivamente baixada, datas, hashes, termos e transformações |
| Retratações e Consumer Shopping | CSVs Kaggle locais referenciados nos READMEs | Versão exata da base, licença específica e integridade da cópia |
| ISUS/SIA, mortalidade (Excel), IOM, Sherlock e demais bases versionadas | Caminhos no catálogo/README | Custódia, permissões e classificações pertinentes; não presumir que o Git permita redistribuição irrestrita |

Os modos auditoria rápida, reexecução de modelo e reprodução desde dados permanecem **distintos** (T06.3). Um resultado agregado publicado ou um arquivo `model_inputs` versionado não prova reconstrução desde fonte primária.

## 4. Verificação e critérios de parada

Na raiz do checkout com histórico Git:

```bash
python -m unittest discover -s tests/python -p 'test_dependencias_t06_5.py' -v
```

Os testes usam **somente** metadados públicos do manifesto T06.1, o inventário T06.5 e `git ls-tree` no SHA-base. Reconciliam os 33 IDs, 27 vínculos, 8 lacunas, status de caminhos e vetos a falsa autorização; incluem mutações adversariais que removem dependências ou alteram permissões. **Não verificam dados brutos, disponibilidade externa, veracidade científica, nem N2/N3/N4.**

**Gate institucional preservado:** [issue #14 (P0)](https://github.com/riscotta/bayes-stan-r/issues/14), mais [#15](https://github.com/riscotta/bayes-stan-r/issues/15) e [#16](https://github.com/riscotta/bayes-stan-r/issues/16), abertas na data de corte. A governança de [#11](https://github.com/riscotta/bayes-stan-r/issues/11) e [#19](https://github.com/riscotta/bayes-stan-r/issues/19) esclarece as dependências históricas de mortalidade e PNAD. Antes de qualquer execução externa ou publicação de novos dados, exigir autorização, vintage, origem, checksum, ambiente, caminho de escrita isolado e QA específico. **Nunca baixar, acessar ou republicar dados institucionais nesta T06.5.**

A implementação T06.5 requer **QA independente sobre o SHA final**, checks obrigatórios e integração aprovada. **Etapa 06 permanece aberta; T06.6 e T06.7 não foram iniciadas** neste ciclo.
