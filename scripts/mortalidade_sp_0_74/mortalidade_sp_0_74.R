# Mortalidade SP 0-74: reproduz sintese congelada ou aciona recálculo E09.
# Rscript --vanilla scripts/mortalidade_sp_0_74/mortalidade_sp_0_74.R
# --recalcular=1: exige base E04 e saídas locais E07/E08 (ver README).
args <- commandArgs(trailingOnly = TRUE)
if (length(setdiff(args, "--recalcular=1")) > 0L) stop("Argumentos: nenhum ou --recalcular=1")
cmd <- grep("^--file=", commandArgs(FALSE), value = TRUE)
stopifnot(length(cmd) == 1L)
script <- normalizePath(sub("^--file=", "", cmd), winslash = "/", mustWork = TRUE)
root <- normalizePath(file.path(dirname(script), "..", ".."), winslash = "/", mustWork = TRUE)
out <- file.path(root, "outputs", "mortalidade_sp_0_74")
dir.create(file.path(out, "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out, "figures"), recursive = TRUE, showWarnings = FALSE)
if ("--recalcular=1" %in% args) {
  source(file.path(dirname(script), "E09_analise_completa.R"), local = new.env(parent = globalenv()))
} else {
  audit <- file.path(root, "data", "raw", "mortalidade_sp_0_74", "resultados_auditados")
  read_audit <- function(n) {
    p <- file.path(audit, n)
    if (!file.exists(p)) stop("Arquivo auditado ausente: ", p)
    read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
  }
  annual <- read_audit("E09_taxas_anuais_e_sensibilidade.csv")
  dec <- read_audit("E09_decomposicao_descritiva_simetrica.csv")
  nb <- read_audit("E09_validacao_NB2_e_cobertura.csv")
  stopifnot(nrow(annual) == 18L, identical(as.integer(annual$ano), 2006:2023),
            nrow(dec) == 4L, nrow(nb) == 7L,
            all(c("taxa_bruta_100mil", "taxa_padronizada_2019_100mil") %in% names(annual)),
            all(is.finite(annual$taxa_bruta_100mil)),
            all(is.finite(annual$taxa_padronizada_2019_100mil)))
  val <- function(year, col) annual[[col]][match(year, annual$ano)]
  stopifnot(abs(val(2019, "taxa_bruta_100mil") - 383.890403995) < 1e-6,
            abs(val(2021, "taxa_padronizada_2019_100mil") - 536.570744552) < 1e-6,
            abs(val(2023, "taxa_padronizada_2019_100mil") - 364.637004485) < 1e-6,
            abs(sum(annual$obitos) - 778039) < 1e-6,
            abs(nb$previstos[nb$ano == 2021] - 42279.9283527) < 1e-5)
  stopifnot(max(abs(dec$parcela_mudanca_taxas_100mil +
                   dec$parcela_composicao_100mil -
                   dec$diferenca_bruta_100mil)) < 1e-7)
  write.csv(annual, file.path(out, "tables", "E09_taxas_anuais_e_sensibilidade.csv"), row.names = FALSE)
  write.csv(dec, file.path(out, "tables", "E09_decomposicao_descritiva_simetrica.csv"), row.names = FALSE)
  write.csv(nb, file.path(out, "tables", "E09_validacao_NB2_e_cobertura.csv"), row.names = FALSE)
  png(file.path(out, "figures", "taxa_bruta_vs_padronizada.png"), width = 1100, height = 650, res = 125)
  plot(annual$ano, annual$taxa_bruta_100mil, type = "o", pch = 16,
       xlab = "Ano", ylab = "Óbitos por 100 mil", main = "Mortalidade 0–74 em São Paulo",
       ylim = range(c(annual$taxa_bruta_100mil, annual$taxa_padronizada_2019_100mil)))
  lines(annual$ano, annual$taxa_padronizada_2019_100mil, type = "o", lty = 2, pch = 15)
  abline(v = 2019, lty = 3)
  legend("topleft", c("Taxa bruta", "Taxa padronizada (padrão 2019)"),
         lty = c(1, 2), pch = c(16, 15), bty = "n")
  dev.off()
  png(file.path(out, "figures", "observado_vs_previsto_nb2.png"), width = 1100, height = 650, res = 125)
  plot(nb$ano, nb$obitos_R, type = "o", pch = 16,
       xlab = "Ano", ylab = "Óbitos", main = "Validação preditiva NB2",
       ylim = range(c(nb$obitos_R, nb$previstos)))
  lines(nb$ano, nb$previstos, type = "o", lty = 2, pch = 15)
  abline(v = 2019.5, lty = 3)
  legend("topleft", c("Observado", "Previsto"), lty = c(1, 2), pch = c(16, 15), bty = "n")
  dev.off()
  cat("Tabelas congeladas conferidas e figuras reconstituídas. Não houve recálculo desde E04.\n")
}
