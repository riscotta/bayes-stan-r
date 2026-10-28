# Adaptado de E09_analise_completa.R (E09, 28/10/2026).
# NAO EXECUTADO nesta publicacao GitHub. Exige base derivada E04 e E07_R_validacao_anual.csv locais.
# E09 — Sao Paulo, mortalidade de 0 a 74 anos, analise descritiva PROVISORIA.
# Execucao: Rscript --vanilla scripts/E09_analise_completa.R
# Entrada: dados/ (CSV derivado E04, validacao anual R E07, bootstrap E08)
# Dependencias: R base. Nao reajusta modelos nem produz inferencia causal.
all_args <- commandArgs(trailingOnly=FALSE)
arg <- grep('^--file=', all_args, value=TRUE)
if (length(arg) != 1L) stop('Execute por Rscript --vanilla scripts/E09_analise_completa.R')
script <- normalizePath(sub('^--file=', '', arg), winslash='/', mustWork=TRUE)
root <- normalizePath(file.path(dirname(script),'..','..'),winslash='/',mustWork=TRUE)
infile <- function(n) file.path(root,'data','raw','mortalidade_sp_0_74',n)
val_path <- Sys.getenv('E10_E07_ANUAL',infile('E07_R_validacao_anual.csv'))
boot_path <- Sys.getenv('E10_E08_BOOTSTRAP',file.path(root,'data','raw','mortalidade_sp_0_74','resultados_auditados','E08_incerteza_cobertura_bootstrap_distrito.csv'))
out_root <- Sys.getenv('E10_E09_OUT',file.path(root,'outputs','mortalidade_sp_0_74'))
outfile <- function(n) file.path(out_root,'tables',n)
figfile <- function(n) file.path(out_root,'figures',n)
dir.create(file.path(out_root,'tables'),showWarnings=FALSE,recursive=TRUE)
dir.create(file.path(out_root,'figures'),showWarnings=FALSE,recursive=TRUE)
stopifnot(file.exists(infile('base_analitica_celular_PROVISORIA.csv')),
          file.exists(val_path),
          file.exists(boot_path))
d <- read.csv(infile('base_analitica_celular_PROVISORIA.csv'),stringsAsFactors=FALSE,fileEncoding='UTF-8')
validation <- read.csv(val_path,stringsAsFactors=FALSE)
bootstrap <- read.csv(boot_path,stringsAsFactors=FALSE)
keys <- c('codigo_apvp_distrito','ano','sexo','faixa_etaria')
stopifnot(nrow(d)==51840L,all(d$populacao>0),sum(d$obitos)==778039,
  sum(d$populacao)==196070964, length(unique(d$codigo_apvp_distrito))==96L,
  length(unique(d$ano))==18L,length(unique(d$sexo))==2L,
  length(unique(d$faixa_etaria))==15L,!anyDuplicated(d[keys]),
  all(d$homologacao_territorial=='PENDENTE'),
  !any(d$limite_geografico_validado=='SIM'))
write_out <- function(a,n) write.csv(a,outfile(n),row.names=FALSE,fileEncoding='UTF-8')
# A taxa de uma soma = soma de obitos / soma da exposicao; nao media de taxas.
strata <- aggregate(cbind(obitos,populacao)~ano+sexo+faixa_etaria,data=d,FUN=sum)
stopifnot(nrow(strata)==540L,all(strata$populacao>0))
strata$taxa_especifica_100mil <- 100000*strata$obitos/strata$populacao
annual <- aggregate(cbind(obitos,populacao)~ano,data=d,FUN=sum)
annual <- annual[order(annual$ano),]
annual$taxa_bruta_100mil <- 100000*annual$obitos/annual$populacao
age_start <- as.integer(sub('-.*$','',d$faixa_etaria))
old <- aggregate(populacao~ano,data=d[age_start>=55,],FUN=sum)
annual$parcela_55_74_populacao <- old$populacao[match(annual$ano,old$ano)]/annual$populacao
# Padronizacao direta fixa: 30 estratos idade x sexo, pesos internos.
standard <- list('2006'=d[d$ano==2006,], '2019'=d[d$ano==2019,],
                 '2006_2023'=d, '2023'=d[d$ano==2023,])
weights <- list()
for (lbl in names(standard)) {
  w <- aggregate(populacao~sexo+faixa_etaria,data=standard[[lbl]],FUN=sum)
  w$peso <- w$populacao/sum(w$populacao)
  stopifnot(nrow(w)==30L,abs(sum(w$peso)-1)<1e-10)
  weights[[lbl]] <- w[c('sexo','faixa_etaria','peso')]
  tmp <- merge(strata,weights[[lbl]],by=c('sexo','faixa_etaria'),sort=FALSE)
  stopifnot(nrow(tmp)==540L)
  tmp$parcela <- tmp$taxa_especifica_100mil*tmp$peso
  std <- aggregate(parcela~ano,data=tmp,FUN=sum)
  annual[[paste0('taxa_padronizada_',lbl,'_100mil')]] <- std$parcela[match(annual$ano,std$ano)]
}
write_out(annual,'E09_taxas_anuais_e_sensibilidade.csv')
w2019 <- weights[['2019']]
names(w2019)[3] <- 'peso_padrao_2019'
write_out(w2019,'E09_pesos_2019_sexo_idade.csv')
# Decomposicao exata de Kitagawa, apenas contabil-descritiva.
# delta bruto = soma[(r1-r0)*(w1+w0)/2] + soma[(w1-w0)*(r1+r0)/2].
periodos <- list(c(2006,2019),c(2019,2021),c(2019,2023),c(2021,2023))
decomp <- do.call(rbind,lapply(periodos,function(par) {
 a <- strata[strata$ano==par[1],c('sexo','faixa_etaria','populacao','taxa_especifica_100mil')]
 b <- strata[strata$ano==par[2],c('sexo','faixa_etaria','populacao','taxa_especifica_100mil')]
 j <- merge(a,b,by=c('sexo','faixa_etaria'),suffixes=c('_0','_1'))
 stopifnot(nrow(j)==30L)
 w0 <- j$populacao_0/sum(j$populacao_0)
 w1 <- j$populacao_1/sum(j$populacao_1)
 r0 <- j$taxa_especifica_100mil_0; r1 <- j$taxa_especifica_100mil_1
 q0 <- sum(w0*r0);q1 <- sum(w1*r1)
 within <- sum((r1-r0)*(w0+w1)/2)
 compos <- sum((w1-w0)*(r0+r1)/2)
 stopifnot(abs(within+compos-(q1-q0))<1e-8)
 data.frame(inicio=par[1],fim=par[2],taxa_inicio_100mil=q0,
   taxa_fim_100mil=q1,diferenca_bruta_100mil=q1-q0,
   parcela_mudanca_taxas_100mil=within,parcela_composicao_100mil=compos,
   erro_identidade=(within+compos)-(q1-q0))
}))
write_out(decomp,'E09_decomposicao_descritiva_simetrica.csv')
# Contribuicao por idade/sexo para a diferenca de taxas diretamente padronizadas.
start <- strata[strata$ano==2019,c('sexo','faixa_etaria','obitos','populacao','taxa_especifica_100mil')]
names(start)[3:5] <- paste0(names(start)[3:5],'_2019')
for (ano_fim in c(2021,2023)) {
  end <- strata[strata$ano==ano_fim,c('sexo','faixa_etaria','obitos','populacao','taxa_especifica_100mil')]
  names(end)[3:5] <- paste0(names(end)[3:5],'_',ano_fim)
  j <- merge(merge(start,end,by=c('sexo','faixa_etaria')),
             w2019,by=c('sexo','faixa_etaria'))
  stopifnot(nrow(j)==30L)
  j$variacao_obitos <- j[[paste0('obitos_',ano_fim)]]-j$obitos_2019
  j$razao_taxa <- j[[paste0('taxa_especifica_100mil_',ano_fim)]]/j$taxa_especifica_100mil_2019
  j$contribuicao_delta_padronizada_100mil <- j$peso_padrao_2019*
    (j[[paste0('taxa_especifica_100mil_',ano_fim)]]-j$taxa_especifica_100mil_2019)
  stopifnot(abs(sum(j$contribuicao_delta_padronizada_100mil)-
    (annual[["taxa_padronizada_2019_100mil"]][annual$ano==ano_fim]-
     annual[["taxa_padronizada_2019_100mil"]][annual$ano==2019]))<1e-8)
  names(j)[names(j)=='peso_padrao_2019'] <- 'peso_2019'
  write_out(j,paste0('E09_contribuicoes_2019_',ano_fim,'_sexo_idade.csv'))
  if (ano_fim==2021) {
    by_age <- aggregate(cbind(contribuicao_delta_padronizada_100mil,variacao_obitos)~
      faixa_etaria,data=j,FUN=sum)
    by_age <- by_age[order(as.integer(sub('-.*$','',by_age$faixa_etaria))),]
    write_out(by_age,'E09_contribuicoes_2019_2021_faixa_etaria.csv')
  }
}
# Taxa por sexo; padronizar idade dentro do sexo, pesos internos 2019.
sexrates <- do.call(rbind,lapply(unique(strata$sexo),function(s) {
  a <- strata[strata$sexo==s,]
  w <- a[a$ano==2019,c('faixa_etaria','populacao')]
  w$peso <- w$populacao/sum(w$populacao)
  b <- merge(a,w[c('faixa_etaria','peso')],by='faixa_etaria')
  b$termo <- b$taxa_especifica_100mil*b$peso
  spec <- aggregate(termo~ano,data=b,FUN=sum)
  ag <- aggregate(cbind(obitos,populacao)~ano,data=a,FUN=sum)
  ag$taxa_bruta_100mil <- 100000*ag$obitos/ag$populacao
  ag$taxa_padronizada_idade_2019_100mil <- spec$termo[match(ag$ano,spec$ano)]
  ag$sexo <- s
  ag[c('ano','sexo','obitos','populacao','taxa_bruta_100mil','taxa_padronizada_idade_2019_100mil')]
}))
write_out(sexrates[order(sexrates$ano,sexrates$sexo),],'E09_taxas_por_sexo.csv')
# Confirmacao independente contra o CSV do ajuste efetivamente executado em R na E07.
nb <- validation[validation$modelo=='NB2',]
stopifnot(nrow(nb)==7L,setequal(nb$ano,2017:2023))
nb <- merge(nb,annual[c('ano','obitos')],by='ano',suffixes=c('_R','_base'))
stopifnot(all(nb$obitos_R==nb$obitos_base))
nb$desvio_observado_menos_previsto <- nb$obitos_R-nb$previstos
nb$razao_observado_previsto <- nb$obitos_R/nb$previstos
nb <- merge(nb,bootstrap,by='ano')
stopifnot(nrow(nb)==7L,max(abs(nb$cobertura95-nb$cobertura_celular))<1e-9)
cols <- c('ano','treino_ate','obitos_R','previstos','desvio_observado_menos_previsto',
   'razao_observado_previsto','cobertura95','IC_boot_cluster_distrito_2_5',
   'IC_boot_cluster_distrito_97_5','logscore_medio','theta')
nb <- nb[order(nb$ano),]
write_out(nb[cols],'E09_validacao_NB2_e_cobertura.csv')
# Graficos em base R. O script nao constrói novos intervalos preditivos agregados.
png(figfile('E09_01_taxa_bruta_vs_padronizada.png'),width=1150,height=650,res=120)
plot(annual$ano,annual$taxa_bruta_100mil,type='o',pch=16,xlab='Ano',
  ylab='Obitos por 100 mil habitantes estimados',main='Mortalidade 0–74: bruta vs padronizada (2019)',
  ylim=range(c(annual$taxa_bruta_100mil,annual$taxa_padronizada_2019_100mil)))
lines(annual$ano,annual$taxa_padronizada_2019_100mil,type='o',pch=15,lty=2)
abline(v=2019,lty=3)
legend('topleft',legend=c('Bruta','Padronizada sexo x idade, padrao 2019'),lty=c(1,2),pch=c(16,15),bty='n')
dev.off()
png(figfile('E09_02_contribuicao_por_idade_2019_2021.png'),width=1150,height=650,res=120)
par(mar=c(8,5,4,2))
barplot(by_age$contribuicao_delta_padronizada_100mil,names.arg=by_age$faixa_etaria,
 las=2,main='Contribuicoes 2019–2021 por faixa etaria',
 ylab='Diferenca padronizada em obitos por 100 mil')
abline(h=0)
dev.off()
png(figfile('E09_03_observado_vs_previsto_nb2.png'),width=1100,height=650,res=120)
plot(nb$ano,nb$obitos_R,type='o',pch=16,xlab='Ano de validacao',ylab='Obitos',
 main='Observado vs previsto NB2',ylim=range(c(nb$obitos_R,nb$previstos)))
lines(nb$ano,nb$previstos,type='o',lty=2,pch=15)
abline(v=2019.5,lty=3)
legend('topleft',c('Observado','Previsto NB2'),lty=c(1,2),pch=c(16,15),bty='n')
dev.off()
png(figfile('E09_04_decomposicao_descritiva.png'),width=1100,height=650,res=120)
barplot(rbind(decomp$parcela_mudanca_taxas_100mil,decomp$parcela_composicao_100mil),
 beside=TRUE,names.arg=paste(decomp$inicio,decomp$fim,sep='–'),
 ylab='Componentes da variacao bruta por 100 mil',
 main='Decomposicao simetrica: variacao de taxas vs composicao')
abline(h=0)
legend('topright',c('Taxas especificas','Composicao'),fill=c('gray20','gray80'),bty='n')
dev.off()
cat('E09: integridade verificada; tabelas e figuras produzidas a partir da base derivada PROVISORIA.\n')
cat('R executado: sim (somente quando este arquivo for efetivamente rodado).\n')