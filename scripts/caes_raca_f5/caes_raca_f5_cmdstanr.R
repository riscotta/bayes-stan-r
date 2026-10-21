#!/usr/bin/env Rscript
# Estudo dos Caes F5 / E10 — entrada do repositorio. Executar a partir da raiz.
# Padrao: audita agregados, sem Stan. --run_stan=1: reexecuta original E08_MP,
# apenas com a base E04 fornecida separadamente, R/cmdstanr e CmdStan instalados.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) && !all(args %in% c("--run_stan=0", "--run_stan=1"))) {
  stop("Uso: Rscript scripts/caes_raca_f5/caes_raca_f5_cmdstanr.R [--run_stan=1]")
}
run_stan <- "--run_stan=1" %in% args
study <- "caes_raca_f5"
root <- normalizePath(".", mustWork = TRUE)
scr <- file.path(root, "scripts", study)
dat <- file.path(root, "data", "raw", study, "resultados_auditados")
out <- file.path(root, "outputs", study)
tables <- file.path(out, "tables")
logs <- file.path(out, "logs")
dir.create(tables, recursive = TRUE, showWarnings = FALSE)
dir.create(logs, recursive = TRUE, showWarnings = FALSE)
need <- c("metricas_full_E08_MP.csv", "metricas_por_fold_auditadas.csv",
          "contrastes_por_fold_auditados.csv", "bootstrap_independente_2000.csv")
if (!all(file.exists(file.path(dat, need)))) stop("Faltam resultados auditados versionados")
read <- function(f) utils::read.csv(file.path(dat, f), stringsAsFactors = FALSE, check.names = FALSE)
m <- read(need[1]); f <- read(need[2]); cfold <- read(need[3]); b <- read(need[4])
stopifnot(nrow(m) == 7L, all(m$n_caes == 2084L),
          setequal(unique(m$familia), c("baseline", "normal", "t5", "skew")),
          nrow(f) == 30L, nrow(cfold) == 15L)
rows <- lapply(c("t5", "normal", "skew"), function(fam) {
  a <- m[m$familia == fam & m$modelo == "M1", ]
  z <- m[m$familia == fam & m$modelo == "M2", ]
  stopifnot(nrow(a) == 1L, nrow(z) == 1L, a$rmse > z$rmse)
  ff <- cfold[cfold$familia == fam, ]
  stopifnot(nrow(ff) == 5L, setequal(ff$fold, 1:5), sum(ff$n) == 2084L,
            sum(ff$delta_rmse > 0) == 4L, ff$delta_rmse[ff$fold == 1L] < 0)
  bb <- b[b$familia == fam & b$contraste == "delta_RMSE_M1_menos_M2", ]
  stopifnot(nrow(bb) == 1L, bb$LI95 > 0, bb$LS95 > bb$LI95)
  data.frame(familia=fam, caes=as.integer(a$n_caes), rmse_M1=a$rmse,
             rmse_M2=z$rmse, delta_rmse=a$rmse-z$rmse,
             ganho_pct=100*(a$rmse-z$rmse)/a$rmse,
             ci95_inf=bb$LI95,ci95_sup=bb$LS95,
             cobertura90_M2=z$cobertura90,
             abaixo_q05_M2=z$abaixo_q05, acima_q95_M2=z$acima_q95)
})
summary <- do.call(rbind, rows)
stopifnot(nrow(summary)==3L,
          all(abs(summary$cobertura90_M2+summary$abaixo_q05_M2+summary$acima_q95_M2-1)<1e-12),
          all(summary$ganho_pct > 0.8 & summary$ganho_pct < 1.5))
utils::write.csv(summary,file.path(tables,"comparacao_F5_E08_MP.csv"),row.names=FALSE,na="")
writeLines(c("E10 — Auditoria de tabelas publicada (NAO e nova estimacao Stan)",
             "Coorte: 2084 caes; 196 racas declaradas; 5 folds internos; 1 repeticao.",
             "M2 melhora RMSE interno em 0,85%–1,41%; fold 1 desfavoravel.",
             "CALIBRACAO_CONDICIONAL_GERAL=NAO_VALIDADA",
             "GENERALIZACAO_EXTERNA=NAO_TESTADA; CAUSALIDADE_GENETICA=NAO_INFERIDA"),
           file.path(logs,"resumo_e_limites.txt"))
print(summary, row.names = FALSE, digits = 6)
if (!run_stan) quit(save="no", status=0L)
# A base E04 por cao nao e redistribuida pelo Git (privacidade/proveniencia).
input <- file.path(root,"data","raw",study,"base_F5_principal_E04.csv")
if (!file.exists(input)) stop("--run_stan=1 exige data/raw/caes_raca_f5/base_F5_principal_E04.csv (externa; ver README)")
if (!requireNamespace("digest",quietly=TRUE)) stop("Instale digest para conferir SHA-256 antes de reamostrar")
expected <- "9db17e9d53edd88b7603591a89fae521f93d1a83b82a851a4156527f3408bad9"
if (!identical(digest::digest(input,algo="sha256",file=TRUE), expected)) stop("Base E04 nao coincide com a execucao historica; nao reamostrar")
old <- file.path(scr,"original","E08_MP_Entregaveis")
placement <- file.path(old,"04_dados_imutaveis")
dir.create(placement,recursive=TRUE,showWarnings=FALSE)
target <- file.path(placement,"base_F5_principal_E04.csv")
if (file.exists(target) &&
    !identical(digest::digest(target,algo="sha256",file=TRUE),expected)) stop("Base existente no layout original tem hash divergente")
if (!file.exists(target) && !file.copy(input,target)) stop("Falha ao preparar copia da base externa no layout original")
original <- file.path(old,"02_codigo","executar_E08_MP.R")
dest <- file.path(out,"full_local")
if (dir.exists(dest) && length(list.files(dest,all.files=TRUE,no..=TRUE))) {
  stop("A pasta de resultado ja contem arquivos; preserve-a antes de uma nova execucao: ",dest)
}
cat("ATENCAO: inicio de 30 ajustes Stan, 120 cadeias; exige revisao computacional posterior.\n")
res <- system2(file.path(R.home("bin"),"Rscript"),
               args=c("--vanilla",shQuote(normalizePath(original)),"--stage=full",
                      paste0("--output=",shQuote(normalizePath(dest,mustWork=FALSE)))))
if (!identical(as.integer(res),0L)) stop("Executor original falhou; nao descarte resultados parciais: ",dest)
cat("Nova execucao finalizada; auditoria e gate NAO sao aprovados automaticamente.\n")
