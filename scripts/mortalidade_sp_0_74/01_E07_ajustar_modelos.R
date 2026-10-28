# E07 — modelos de contagem Poisson e NB2 para mortalidade 0–74, SP, 2006–2023.
# Fonte: pacote histórico E10, script/01_E07_ajustar_modelos.R. Não executado neste ambiente.
# Exige base celular E04 local, NÃO versionada. Pacote MASS somente para NB2.
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=2L)stop("Uso: Rscript 01_E07_ajustar_modelos.R <base_E04.csv> <saida>")
base_file<-args[[1]];out_dir<-args[[2]]
if(!file.exists(base_file))stop("Arquivo da base não encontrado: ",base_file)
if(!requireNamespace("MASS",quietly=TRUE))stop("Instale o pacote MASS")
dir.create(out_dir,recursive=TRUE,showWarnings=FALSE)
set.seed(20261027)
d<-read.csv(base_file,stringsAsFactors=FALSE,check.names=FALSE)
req<-c("codigo_apvp_distrito","sexo","ano","faixa_etaria","obitos","populacao")
stopifnot(all(req %in% names(d)),nrow(d)==51840L,all(complete.cases(d[,req])),
 all(d$obitos>=0),all(d$populacao>0),all(d$obitos==floor(d$obitos)),
 !anyDuplicated(d[,c("codigo_apvp_distrito","sexo","ano","faixa_etaria")]))
ages<-paste0(seq(0,70,5),"-",seq(4,74,5),"a")
stopifnot(setequal(unique(d$faixa_etaria),ages),
 setequal(unique(d$sexo),c("Feminino","Masculino")),
 length(unique(d$codigo_apvp_distrito))==96L,setequal(unique(d$ano),2006:2023))
d$faixa_etaria<-factor(d$faixa_etaria,levels=ages)
d$sexo<-factor(d$sexo,levels=c("Feminino","Masculino"))
d$distrito<-factor(d$codigo_apvp_distrito)
d$tempo<-d$ano-2012
f<-obitos~faixa_etaria*sexo+distrito+tempo+offset(log(populacao))
folds<-list(list(cutoff=2016L,test=2017L),list(cutoff=2017L,test=2018L),
 list(cutoff=2018L,test=2019L),list(cutoff=2019L,test=2020:2023))
annual<-list();stratum<-list();districts<-list();overall<-list();fits<-list();k<-0L
calc_group<-function(z)c(celulas=nrow(z),obitos=sum(z$obitos),previstos=sum(z$previstos),
 obs_prev=sum(z$obitos)/sum(z$previstos),zeros_obs=sum(z$zero_obs),zeros_esp=sum(z$zero_esp),
 cobertura95=mean(z$cobertura95),logscore_medio=mean(z$logscore),
 mae=mean(abs(z$obitos-z$previstos)),rmse=sqrt(mean((z$obitos-z$previstos)^2)))
by_groups<-function(z,columns){
 ix<-interaction(z[,columns,drop=FALSE],drop=TRUE,lex.order=TRUE)
 spl<-split(z,ix,drop=TRUE)
 do.call(rbind,lapply(spl,function(part) {
   cbind(part[1,columns,drop=FALSE],as.data.frame(as.list(calc_group(part))))
 }))
}
for(fo in folds){
 tr<-d[d$ano<=fo$cutoff,,drop=FALSE];te<-d[d$ano %in% fo$test,,drop=FALSE]
 fp<-glm(f,data=tr,family=poisson(link="log"),
         control=glm.control(maxit=100,epsilon=1e-9))
 fn<-MASS::glm.nb(f,data=tr,control=glm.control(maxit=200,epsilon=1e-9),maxit=200)
 for(modelo in c("Poisson","NB2")){
  fit<-if(modelo=="Poisson")fp else fn
  theta<-if(modelo=="NB2")fit$theta else NA_real_
  mu<-as.numeric(predict(fit,newdata=te,type="response"))
  if(modelo=="Poisson"){
   pzero<-dpois(0,lambda=mu);ll<-dpois(te$obitos,lambda=mu,log=TRUE)
   low<-qpois(.025,lambda=mu);high<-qpois(.975,lambda=mu)
  }else{
   pzero<-dnbinom(0,size=theta,mu=mu);ll<-dnbinom(te$obitos,size=theta,mu=mu,log=TRUE)
   low<-qnbinom(.025,size=theta,mu=mu);high<-qnbinom(.975,size=theta,mu=mu)
  }
  z<-te[,c("ano","sexo","faixa_etaria","codigo_apvp_distrito","obitos")]
  z$previstos<-mu;z$zero_obs<-as.integer(z$obitos==0);z$zero_esp<-pzero
  z$cobertura95<-as.integer(z$obitos>=low & z$obitos<=high);z$logscore<-ll
  k<-k+1L
  overall[[k]]<-cbind(data.frame(modelo=modelo,treino_ate=fo$cutoff,
   teste=paste(fo$test,collapse=","),theta=theta),as.data.frame(as.list(calc_group(z))))
  a<-by_groups(z,c("ano"));a$modelo<-modelo;a$treino_ate<-fo$cutoff
  a$theta<-theta;annual[[k]]<-a
  b<-by_groups(z,c("ano","sexo","faixa_etaria"));b$modelo<-modelo
  b$treino_ate<-fo$cutoff;stratum[[k]]<-b
  c<-by_groups(z,c("ano","codigo_apvp_distrito"));c$modelo<-modelo
  c$treino_ate<-fo$cutoff;districts[[k]]<-c
  fits[[k]]<-data.frame(modelo=modelo,treino_ate=fo$cutoff,
   convergiu=isTRUE(fit$converged),theta=theta,AIC_treino=AIC(fit),nobs_treino=nobs(fit))
  message(modelo," | treino até ",fo$cutoff," | log-score ",round(mean(ll),5),
   " | cobertura ",round(mean(z$cobertura95),5))
 }
}
write.csv(do.call(rbind,overall),file.path(out_dir,"E07_R_validacao_modelos.csv"),row.names=FALSE)
write.csv(do.call(rbind,annual),file.path(out_dir,"E07_R_validacao_anual.csv"),row.names=FALSE)
write.csv(do.call(rbind,stratum),file.path(out_dir,"E07_R_calibracao_idade_sexo_ano.csv"),row.names=FALSE)
write.csv(do.call(rbind,districts),file.path(out_dir,"E07_R_calibracao_distrito_ano.csv"),row.names=FALSE)
write.csv(do.call(rbind,fits),file.path(out_dir,"E07_R_diagnosticos_ajuste.csv"),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(out_dir,"E07_R_sessionInfo.txt"))
message("Fim E07. Resultados em ",normalizePath(out_dir))
