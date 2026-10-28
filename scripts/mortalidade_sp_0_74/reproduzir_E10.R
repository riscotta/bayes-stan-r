# E10 — pipeline R com execução não destrutiva; nenhum dado privado publicado.
# Rscript --vanilla scripts/mortalidade_sp_0_74/reproduzir_E10.R --modo=auditar
# Rscript --vanilla scripts/mortalidade_sp_0_74/reproduzir_E10.R --modo=conferir
# Rscript --vanilla scripts/mortalidade_sp_0_74/reproduzir_E10.R --modo=completo
args<-commandArgs(trailingOnly=TRUE)
if(length(args)!=1L||!args %in% c("--modo=auditar","--modo=conferir","--modo=completo"))
 stop("Escolha --modo=auditar, --modo=conferir ou --modo=completo")
modo<-sub("^--modo=","",args)
argfile<-grep("^--file=",commandArgs(FALSE),value=TRUE)
stopifnot(length(argfile)==1L)
sfile<-normalizePath(sub("^--file=","",argfile),mustWork=TRUE)
sdir<-dirname(sfile)
root<-normalizePath(file.path(sdir,"..",".."),mustWork=TRUE)
out<-file.path(root,"outputs","mortalidade_sp_0_74")
dir.create(out,recursive=TRUE,showWarnings=FALSE)
run_r<-function(file,extra=character()){
  rbin<-Sys.which("Rscript");if(!nzchar(rbin))stop("Rscript não encontrado")
  output<-suppressWarnings(system2(rbin,args=c("--vanilla",shQuote(file),
   vapply(extra,shQuote,character(1))),stdout=TRUE,stderr=TRUE))
  status<-attr(output,"status");if(is.null(status))status<-0L
  if(length(output))cat(paste(output,collapse="\n"),"\n")
  if(status!=0L)stop("Falhou a execução: ",file)
  invisible(output)
}
if(modo=="auditar"){
 run_r(file.path(sdir,"mortalidade_sp_0_74.R"))
 cat("Modo auditar concluído: usa indicadores públicos; não recalcula E04/E07.\n")
}else{
 ddir<-file.path(root,"data","raw","mortalidade_sp_0_74")
 base<-file.path(ddir,"base_analitica_celular_PROVISORIA.csv")
 if(!file.exists(base))stop("Base E04 celular não encontrada localmente: ",base)
 if(modo=="completo"){
  if(!requireNamespace("MASS",quietly=TRUE))stop("Pacote MASS necessário")
  e07dir<-file.path(out,"E07_reajustado")
  dir.create(e07dir,recursive=TRUE,showWarnings=FALSE)
  run_r(file.path(sdir,"01_E07_ajustar_modelos.R"),c(base,e07dir))
  e08dir<-file.path(out,"E08_recalculada")
  Sys.setenv(E10_E07_DIR=e07dir,E10_E08_DIR=e08dir)
  source(file.path(sdir,"02_E08_diagnosticar.R"),local=new.env(parent=globalenv()))
  bootfile<-file.path(e08dir,"E08_R_incerteza_cobertura_bootstrap_distrito.csv")
 }else{
  e07dir<-ddir
  required<-c("E07_R_validacao_anual.csv","E07_R_calibracao_distrito_ano.csv",
   "E07_R_calibracao_idade_sexo_ano.csv","E07_R_diagnosticos_ajuste.csv")
  if(!all(file.exists(file.path(e07dir,required))))
   stop("No modo conferir são necessários os quatro CSVs R E07 locais; veja README.")
  bootfile<-file.path(ddir,"resultados_auditados",
   "E08_incerteza_cobertura_bootstrap_distrito.csv")
  e08dir<-file.path(out,"E08_conferida")
  Sys.setenv(E10_E07_DIR=e07dir,E10_E08_DIR=e08dir)
  source(file.path(sdir,"02_E08_diagnosticar.R"),local=new.env(parent=globalenv()))
 }
 e09dir<-file.path(out,paste0("E09_",modo))
 Sys.setenv(E10_E09_OUT=e09dir,
  E10_E07_ANUAL=file.path(e07dir,"E07_R_validacao_anual.csv"),
  E10_E08_BOOTSTRAP=bootfile)
 source(file.path(sdir,"E09_analise_completa.R"),local=new.env(parent=globalenv()))
 refdir<-file.path(ddir,"resultados_auditados")
 refs<-list.files(refdir,pattern="^E09_.*[.]csv$",full.names=FALSE)
 cmp<-function(filename){
  local_file<-file.path(e09dir,"tables",filename)
  if(!file.exists(local_file))
   return(data.frame(arquivo=filename,estado="NAO_GERADO",max_delta=NA_real_))
  a<-read.csv(file.path(refdir,filename),stringsAsFactors=FALSE)
  b<-read.csv(local_file,stringsAsFactors=FALSE)
  if(!setequal(names(a),names(b))||nrow(a)!=nrow(b))
   return(data.frame(arquivo=filename,estado="ESTRUTURA_DIFERENTE",max_delta=NA_real_))
  keys<-intersect(c("ano","sexo","faixa_etaria","inicio","fim"),names(a))
  if(length(keys)){
   a<-a[do.call(order,a[keys]),,drop=FALSE]
   b<-b[do.call(order,b[keys]),,drop=FALSE]
  }
  b<-b[names(a)]
  num<-names(a)[vapply(a,is.numeric,logical(1))]
  delta<-if(length(num))max(unlist(lapply(num,function(n)abs(a[[n]]-b[[n]]))),na.rm=TRUE)else 0
  nonnum<-setdiff(names(a),num)
  text_ok<-all(vapply(nonnum,function(n)identical(as.character(a[[n]]),as.character(b[[n]])),logical(1)))
  estado<-if(!text_ok)"TEXTO_DIFERENTE"else if(is.finite(delta)&&delta<1e-7)"CONCORDA_ATE_1e-7"else "DIVERGENCIA_NUMERICA"
  data.frame(arquivo=filename,estado=estado,max_delta=delta)
 }
 comparison<-do.call(rbind,lapply(refs,cmp))
 write.csv(comparison,file.path(e09dir,"COMPARACAO_E09_PUBLICADA.csv"),row.names=FALSE)
 print(comparison,row.names=FALSE)
 if(modo=="conferir"&&any(comparison$estado!="CONCORDA_ATE_1e-7"))
  stop("Houve divergência numérica/estrutural no modo conferir; veja comparação.")
 cat("Pipeline ",modo," concluído: ",e09dir,"\n",sep="")
}
