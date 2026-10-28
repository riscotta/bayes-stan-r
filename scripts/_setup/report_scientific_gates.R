#!/usr/bin/env Rscript
# Auditoria de limites já publicados. Não compila nem reamostra modelos Stan.
# Somente R base e artefatos auditados existentes.
read <- function(path) utils::read.csv(path, stringsAsFactors = FALSE,
                                        check.names = FALSE, fileEncoding = "UTF-8-BOM")
iom_dir <- "data/raw/missing_migrants/resultados_auditados"
und_dir <- "data/raw/prisons_prisoners_unodc/resultados_auditados"
f5_dir <- "data/raw/caes_raca_f5/resultados_auditados"

mcmc <- read(file.path(iom_dir, "E08_resumo_diagnostico_mcmc.csv"))
e10 <- read(file.path(iom_dir, "E10_resultados_chave.csv"))
loo <- read(file.path(und_dir, "PSIS_LOO_resumo_M1_M2_M3.csv"))
rhat <- read(file.path(und_dir, "auditoria_rhat_M1_REV5_por_categoria.csv"))
f5 <- read(file.path(f5_dir, "metricas_full_E08_MP.csv"))
sbc2 <- read(file.path(und_dir, "sbc_status_M2.csv"))
sbc3 <- read(file.path(und_dir, "sbc_status_M3.csv"))

metric <- function(name) {
  v <- as.numeric(mcmc$valor[mcmc$metrica == name])
  if (length(v) != 1L || !is.finite(v)) stop("Métrica MMP inválida: ", name)
  v
}

mmp_rhat <- metric("rhat_max")
mmp_bad <- metric("n_rhat_gt_1_01")
mmp_ess <- metric("ess_bulk_min")
mmp_div <- metric("divergencias_total")
if (!is.finite(mmp_rhat) || !is.finite(mmp_bad)) stop("Métricas MMP inválidas")
ppc <- e10$valor[e10$indicador == "Falhas principais"]
if (length(ppc) != 1L) stop("Resumo PPC não encontrado")
ppc_warning <- grepl("n_zeros|desvio_padrao", ppc)
countries <- loo[loo$level == "country", ]
if (!setequal(countries$model, c("M1", "M2", "M3"))) {
  stop("Faltam linhas LOO por país para M1, M2 ou M3")
}
all_bad_country <- all(countries$n_k_gt_0_7 > 0)
n_by_model <- loo[loo$level == "observation", c("model", "n_units")]
different_samples <- length(unique(n_by_model$n_units)) > 1L

m1_local_bad <- sum(rhat$n_rhat_gt_101[rhat$categoria %in%
                                         c("country_effect", "region_base")])
f5_gains <- vapply(c("normal", "skew", "t5"), function(family) {
  x <- f5[f5$familia == family, ]
  v1 <- x$rmse[x$modelo == "M1"]
  v2 <- x$rmse[x$modelo == "M2"]
  if (length(v1) != 1L || length(v2) != 1L) stop("Métricas F5 ausentes")
  100 * (v1 - v2) / v1
}, numeric(1))

fmt <- function(x, digits = 3) format(round(x, digits), decimal.mark = ",",
                                      trim = TRUE, scientific = FALSE)
warning <- function(flag) if (flag) "RESSALVA" else "SEM ALERTA NESTE TESTE"

report <- c(
  "## Auditoria de limites científicos publicados",
  "",
  "Esta auditoria confere arquivos históricos. **Não executa Stan, não calcula novos pós-teriores e não prova validade externa.**",
  "",
  "| Estudo | Sinal publicado | Resultado do gate |",
  "| --- | --- | --- |",
  sprintf("| IOM / Missing Migrants | R-hat máximo %s; %d quantidades acima de 1,01; ESS bulk mín. %s; divergências %d | %s |",
          fmt(mmp_rhat, 4), as.integer(mmp_bad), fmt(mmp_ess, 1), as.integer(mmp_div),
          warning(mmp_rhat > 1.01 || mmp_ess < 400 || ppc_warning)),
  sprintf("| UNODC / M1 | Efeitos locais com R-hat acima de 1,01: %d | %s |",
          m1_local_bad, warning(m1_local_bad > 0)),
  sprintf("| UNODC / M1–M3 | Modelos com problemas Pareto-k na validação por país: %d/3 | %s |",
          sum(countries$n_k_gt_0_7 > 0), warning(all_bad_country)),
  sprintf("| UNODC / M1–M3 | Tamanhos de bases de LOO por observação: %s | %s |",
          paste(n_by_model$n_units[match(c("M1","M2","M3"), n_by_model$model)],
                collapse = ", "), warning(different_samples)),
  sprintf("| UNODC / SBC | Réplicas M2=%d; M3=%d; R-hat máximo M2=%s e M3=%s | Exploratória; não equivale a calibração completa |",
          nrow(sbc2), nrow(sbc3), fmt(max(sbc2$rhat_max), 4), fmt(max(sbc3$rhat_max), 4)),
  sprintf("| Cães F5 | Ganho RMSE interno M2 sobre M1: %s%% a %s%% | Sem validação externa por este gate |",
          fmt(min(f5_gains), 2), fmt(max(f5_gains), 2)),
  "",
  "**Interpretação:** não comparar ELPD-LOO entre modelos UNODC que usam universos distintos;",
  "não inferir predição confiável para um país novo a partir dos PSIS-LOO assinalados;",
  "não confundir divergências HMC iguais a zero com PPC satisfatório.",
  "",
  "Fonte: CSVs versionados em `data/raw/missing_migrants/resultados_auditados/`,",
  "`data/raw/prisons_prisoners_unodc/resultados_auditados/` e `data/raw/caes_raca_f5/resultados_auditados/`.",
  ""
)
out <- paste(report, collapse = "\n")
cat(out, "\n")
summary_path <- Sys.getenv("GITHUB_STEP_SUMMARY", "")
if (nzchar(summary_path)) cat(out, "\n", file = summary_path, append = TRUE)
