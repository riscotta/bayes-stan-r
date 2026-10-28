# E08 — validação e diagnóstico das saídas R E07.
# Fonte histórica: E10/script/02_E08_diagnosticar.R; executar pelo orquestrador.
sfile<-sub("^--file=","",grep("^--file=",commandArgs(FALSE),value=TRUE))
root<-normalizePath(file.path(dirname(sfile),"..",".."),mustWork=TRUE)
e07dir<-Sys.getenv("E10_E07_DIR",file.path(root,"data","raw","mortalidade_sp_0_74"))
e08dir<-Sys.getenv("E10_E08_DIR",file.path(root,"outputs","mortalidade_sp_0_74","E08_R"))
dir.create(e08dir,recursive=TRUE,showWarnings=FALSE)
loadcsv<-function(n)read.csv(file.path(e07dir,n),stringsAsFactors=FALSE)
writecsv<-function(x,n)write.csv(x,file.path(e08dir,n),row.names=FALSE,fileEncoding="UTF-8")
a<-loadcsv("E07_R_validacao_anual.csv")
d<-loadcsv("E07_R_calibracao_distrito_ano.csv")
s<-loadcsv("E07_R_calibracao_idade_sexo_ano.csv")
f<-loadcsv("E07_R_diagnosticos_ajuste.csv")
stopifnot(all(c("ano","modelo","obitos","previstos","cobertura95","theta") %in% names(a)),
 setequal(a$ano,2017:2023),nrow(d[d$modelo=="NB2",])==672L,
 nrow(s[s$modelo=="NB2",])==210L,all(f$convergiu))
nb<-a[a$modelo=="NB2",];nb<-nb[order(nb$ano),]
nb$desvio<-nb$obitos-nb$previstos
nb$razao_observado_previsto<-nb$obitos/nb$previstos
writecsv(nb,"E08_R_calibracao_anual.csv")
dn<-d[d$modelo=="NB2",]
dn$residuo_pearson_agregado<-(dn$obitos-dn$previstos)/sqrt(dn$previstos)
corrows<-lapply(2017:2022,function(yr){
 x<-dn[dn$ano==yr,c("codigo_apvp_distrito","residuo_pearson_agregado")]
 y<-dn[dn$ano==yr+1,c("codigo_apvp_distrito","residuo_pearson_agregado")]
 xy<-merge(x,y,by="codigo_apvp_distrito",suffixes=c("_0","_1"))
 stopifnot(nrow(xy)==96L)
 ct<-cor.test(xy$residuo_pearson_agregado_0,xy$residuo_pearson_agregado_1)
 data.frame(inicio=yr,fim=yr+1,n_distritos=96L,pearson_r=unname(ct$estimate),
 p_exploratorio_nao_independente=ct$p.value)
})
writecsv(do.call(rbind,corrows),"E08_R_persistencia_residuo_distrital.csv")
# Bootstrap cluster com 96 distritos, B=2500 (não IC do total de óbitos).
set.seed(20261028)
ci<-lapply(2017:2023,function(yr){
 x<-dn[dn$ano==yr,];x<-x[order(x$codigo_apvp_distrito),]
 stopifnot(nrow(x)==96L)
 v<-x$cobertura95
 boot<-replicate(2500L,mean(sample(v,length(v),replace=TRUE)))
 data.frame(ano=yr,cobertura_celular=mean(v),
  IC_boot_cluster_distrito_2_5=unname(quantile(boot,.025)),
  IC_boot_cluster_distrito_97_5=unname(quantile(boot,.975)),n_distritos=96L)
})
ci<-do.call(rbind,ci);writecsv(ci,"E08_R_incerteza_cobertura_bootstrap_distrito.csv")
stopifnot(max(abs(ci$cobertura_celular-nb$cobertura95))<1e-10)
po<-a[a$modelo=="Poisson",]
cp<-merge(nb,po,by="ano",suffixes=c("_NB2","_Poisson"))
comp<-data.frame(ano=cp$ano,
 ganho_logscore_NB2=cp$logscore_medio_NB2-cp$logscore_medio_Poisson,
 ganho_cobertura_pp_NB2=100*(cp$cobertura95_NB2-cp$cobertura95_Poisson),
 delta_previstos=cp$previstos_NB2-cp$previstos_Poisson)
writecsv(comp,"E08_R_comparacao_modelos.csv")
calib<-sum(nb$obitos[nb$ano<=2019])/sum(nb$previstos[nb$ano<=2019])
sens<-data.frame(ano=nb$ano,obitos=nb$obitos,previstos=nb$previstos,
 previsto_corrigido_calibracao_pre_2017_19=nb$previstos*calib,
 razao_original=nb$obitos/nb$previstos,
 razao_corrigida=nb$obitos/(nb$previstos*calib),fator_correcao_est_pre=calib)
writecsv(sens,"E08_R_sensibilidade_calibracao_pre.csv")
sn<-s[s$modelo=="NB2",];sn$periodo<-ifelse(sn$ano<=2019,"pre_2020","pos_2020")
sg<-aggregate(cbind(obitos,previstos,celulas)~periodo+sexo+faixa_etaria,data=sn,FUN=sum)
sg$obs_prev<-sg$obitos/sg$previstos;sg$dif<-sg$obitos-sg$previstos
writecsv(sg,"E08_R_calibracao_estratos_periodo.csv")
for(yr in 2017:2023){
 z<-dn[dn$ano==yr,];zz<-sn[sn$ano==yr,];t<-nb[nb$ano==yr,]
 stopifnot(sum(z$obitos)==t$obitos,sum(zz$obitos)==t$obitos,
 abs(sum(z$previstos)-t$previstos)<1e-4,
 abs(sum(zz$previstos)-t$previstos)<1e-4)
}
writeLines(c("E08 R: identidades, previsões e cobertura verificadas.",
 "Bootstrap distrital R: B=2500, seed 20261028; amostras podem diferir de NumPy.",
 "Não há intervalo de incerteza do total agregado; correlações não provam causalidade."),
 file.path(e08dir,"LEIA_RESULTADOS.txt"))
cat("E08 R: testes e diagnósticos finalizados.\n")
