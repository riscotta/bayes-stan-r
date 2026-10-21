#!/usr/bin/env Rscript
# Extensao E08_MP. MP1 aprovado por decisao explicita; hash de aditivo/config exige coincidencia.
# Uso: Rscript --vanilla executar_E08_MP.R --stage=verify|prior|smoke|full --output=...
options(stringsAsFactors = FALSE)
argv <- commandArgs(trailingOnly=TRUE)
arg <- function(n, default=NA_character_) {
  x <- grep(paste0('^--',n,'='), argv, value=TRUE)
  if(length(x)!=1L) return(default)
  substring(x, nchar(n)+4L)
}
stage <- arg('stage','verify')
stopifnot(stage %in% c('verify','prior','smoke','full'))
scr <- grep('^--file=',commandArgs(),value=TRUE)
if(length(scr)!=1L) stop('Executar via Rscript --vanilla <arquivo>')
root <- normalizePath(file.path(dirname(sub('^--file=','',scr)),'..'), mustWork=TRUE)
out <- arg('output',file.path(root,'07_retornos',paste0('execucao_',stage,'_',format(Sys.time(),'%Y%m%d_%H%M%S'))))
if(dir.exists(out) && length(list.files(out, all.files=TRUE, no..=TRUE))) stop('Diretorio de saida ocupado: ',out)
dir.create(out,recursive=TRUE,showWarnings=FALSE)
assert <- function(p,msg) if(!isTRUE(p)) stop('E08_MP - ',msg,call.=FALSE)
for(pkg in c('digest','jsonlite')) assert(requireNamespace(pkg,quietly=TRUE),paste('Pacote R ausente:',pkg))
sha <- function(f) digest::digest(f,algo='sha256',file=TRUE)
write_json <- function(x,p) jsonlite::write_json(x,p,pretty=TRUE,auto_unbox=TRUE,null='null',digits=NA)
write_csv <- function(x,p) utils::write.csv(x,p,row.names=FALSE,na='')
base_path <- file.path(root,'04_dados_imutaveis','base_F5_principal_E04.csv')
spec_path <- file.path(root,'03_referencias_congeladas','Especificacao_E06_CONGELADA.json')
config_path <- file.path(root,'01_especificacao','configuracao_proposta_E08_MP.json')
assert(file.exists(base_path)&&file.exists(spec_path)&&file.exists(config_path),'Faltam entradas')
config <- jsonlite::read_json(config_path,simplifyVector=TRUE)
spec <- jsonlite::read_json(spec_path,simplifyVector=TRUE)
dat <- read.csv(base_path,check.names=FALSE)
keys <- c('dog_id','f5_score_norm','owner_breed','sex','size','sterilized','f5_age_years')
assert(all(keys %in% names(dat)),'Colunas da base faltantes')
assert(nrow(dat)==2084L&&!anyDuplicated(dat$dog_id)&&length(unique(dat$owner_breed))==196L,'N ou identificadores inesperados')
assert(all(stats::complete.cases(dat[,keys])),'Ausencia em campos principais')
assert(all(dat$sex %in% c('female','male')),'Sexo inesperado')
assert(all(dat$size %in% 0:4)&&all(dat$sterilized %in% c('yes','no')),'Categorias inesperadas')
assert(all(is.finite(dat$f5_score_norm))&&all(is.finite(dat$f5_age_years)),'Escala nao finita')
write_json(list(data=format(Sys.time(),tz='UTC',usetz=TRUE),status='INTEGRIDADE_DE_DADOS',
                n_caes=nrow(dat),n_racas=length(unique(dat$owner_breed)),
                dados_sha256=sha(base_path),especificacao_E06_sha256=sha(spec_path),
                configuracao_sha256=sha(config_path),stage=stage),file.path(out,'integridade_dados.json'))
if(stage=='verify') {
  writeLines('MP0: dados e esquema conferidos; nenhum modelo foi ajustado.',file.path(out,'LEIA_RESULTADO.txt'))
  quit(save='no',status=0L)
}
# Mesmo desenho de E06, calculado apenas com a media e desvio do treino de cada fold.
design <- function(tr,te) {
  m <- mean(tr$f5_age_years); s <- stats::sd(tr$f5_age_years)
  assert(is.finite(s)&&s>0,'DP idade <=0')
  mx <- function(d) {
    z <- (d$f5_age_years-m)/s
    o <- cbind(male=as.numeric(d$sex=='male'),size_0=as.numeric(d$size==0),
      size_1=as.numeric(d$size==1),size_3=as.numeric(d$size==3),
      size_4=as.numeric(d$size==4),sterilized_no=as.numeric(d$sterilized=='no'),
      age_z=z,age_z_sq=z^2)
    o
  }
  list(Xtr=mx(tr),Xte=mx(te),mean_age=m,sd_age=s)
}
set.seed(config$seed_prior)
prior <- function(model) {
  N <- 5000L
  a <- rnorm(N,0,1.5); b_age <- rnorm(N,0,0.5); b_age2 <- rnorm(N,0,0.5)
  w <- abs(rnorm(N,0,1)); lam <- rnorm(N,0,1.5)
  delta <- lam / sqrt(1+lam^2); tau <- if(model=='M2') abs(rnorm(N,0,0.5)) else rep(0,N)
  res <- list()
  for(z in c(-2,0,2)) {
    eta <- a + b_age*z + b_age2*z^2 + tau*rnorm(N)
    err <- w*(delta*(abs(rnorm(N))-sqrt(2/pi))+sqrt(1-delta^2)*rnorm(N))
    yy <- eta + err
    res[[length(res)+1L]] <- data.frame(modelo=model,age_z=z,
      p01=unname(quantile(yy,.01)),p05=unname(quantile(yy,.05)),
      p50=unname(quantile(yy,.5)),p95=unname(quantile(yy,.95)),
      p99=unname(quantile(yy,.99)),pct_menor_min_observado=mean(yy<min(dat$f5_score_norm)),
      pct_maior_max_observado=mean(yy>max(dat$f5_score_norm)))
  }
  do.call(rbind,res)
}
if(stage=='prior') {
  write_csv(rbind(prior('M1'),prior('M2')),file.path(out,'prior_predictive_ILUSTRATIVA.csv'))
  writeLines('Simulacao a priori sem ajuste; extremos amostrais NAO sao limites teoricos.',file.path(out,'LEIA_RESULTADO.txt'))
  quit(save='no',status=0L)
}
# MP1 requer aprovacao EXPLICITA vinculada a hash das especificacoes novas.
approval_path <- file.path(root,'01_especificacao','aprovacao_MP1.json')
assert(file.exists(approval_path),'MP1 pendente: falta aprovacao_MP1.json')
approval <- jsonlite::read_json(approval_path,simplifyVector=TRUE)
assert(isTRUE(approval$aprovado),'MP1 pendente: aprovado=false')
assert(identical(approval$sha256_aditivo,sha(file.path(root,'01_especificacao','ADITIVO_METODOLOGICO_E08_MP.md'))),'MP1: hash aditivo divergente')
assert(identical(approval$sha256_configuracao,sha(config_path)),'MP1: hash config divergente')
assert(identical(approval$autorizacao,'AJUSTES_E08_MP_APROVADOS'),'MP1: autorizacao ausente')
for(pkg in c('cmdstanr','posterior','pbivnorm')) assert(requireNamespace(pkg,quietly=TRUE),paste('Pacote R ausente:',pkg))
assert(nzchar(cmdstanr::cmdstan_path()),'CmdStan nao configurado')
writeLines(capture.output(sessionInfo()),file.path(out,'sessionInfo.txt'))
write_json(list(R=R.version.string,cmdstan=as.character(cmdstanr::cmdstan_version()),
                cmdstanr=as.character(utils::packageVersion('cmdstanr')),
                posterior=as.character(utils::packageVersion('posterior')),
                pbivnorm=as.character(utils::packageVersion('pbivnorm')),
                chains=config$chains,iter_warmup=config$warmup,iter_sampling=config$sampling,
                adapt_delta=config$adapt_delta,max_treedepth=config$max_treedepth,
                draw_limit=config$draw_limit,seed_outer=config$seed_outer),file.path(out,'ambiente.json'))
# Integridade numerica teorica SN: alpha 0 = Normal; soma Normal SN com delta 0 = Normal.
sn_cdf <- function(x,xi,omega,lambda) {
  delta <- lambda/sqrt(1+lambda^2)
  pmin(1,pmax(0,2*pbivnorm::pbivnorm((x-xi)/omega,rep(0,length(omega)),rho=-delta)))
}
for(z in c(-2,-.8,0,.8,2)) {
  assert(abs(sn_cdf(z,0,1,0)-pnorm(z))<2e-6,'CDF skew-normal alfa=0 divergente')
  assert(abs(sn_cdf(z,0,1.3,0)-pnorm(z/1.3))<2e-6,'Escala skew-normal divergente')
}
# Referencias C0: copies byte-a-byte da E07; nao sao reescritas.
stan_files <- c(M1_old=file.path(root,'03_referencias_congeladas','modelo_M1_E07.stan'),
                M2_old=file.path(root,'03_referencias_congeladas','modelo_M2_E07.stan'),
                M1_sn=file.path(root,'02_codigo','stan','M1_skew_normal_E08_MP.stan'),
                M2_sn=file.path(root,'02_codigo','stan','M2_skew_normal_E08_MP.stan'))
assert(all(file.exists(stan_files)),'Arquivos Stan ausentes')
write_csv(data.frame(arquivo=names(stan_files),sha256=vapply(stan_files,sha,character(1))),file.path(out,'hashes_stan.csv'))
# MP1 revisao: nenhum executavel gerado ao lado das fontes imutaveis.
# CmdStanR usa o diretorio do .stan por padrao; definir dir de compilacao isolado.
compiled_dir <- file.path(out, 'stan_compilados')
dir.create(compiled_dir, recursive=TRUE, showWarnings=FALSE)
mods <- lapply(stan_files, function(p) cmdstanr::cmdstan_model(
  stan_file=p, dir=compiled_dir, pedantic=TRUE, quiet=FALSE))
assert(all(vapply(mods, function(x) file.exists(x$exe_file()), logical(1))),
       'Compilacao nao gerou todos os executaveis esperados')
write_csv(data.frame(modelo=names(mods),
                     executavel=vapply(mods,function(x) basename(x$exe_file()),character(1))),
          file.path(out,'executaveis_stan_E08_MP.csv'))
dir.create(file.path(out,'cmdstan_csv'),showWarnings=FALSE)
quad_normal <- function(n=21L) {
  J <- matrix(0,n,n)
  for(j in seq_len(n-1L)) J[j,j+1L] <- J[j+1L,j] <- sqrt(j)
  E <- eigen(J,symmetric=TRUE)
  list(nodes=E$values,weights=E$vectors[1,]^2)
}
gh <- quad_normal()
logmeanexp <- function(x) { v <- max(x); assert(is.finite(v),'log dens nao finita'); v+log(mean(exp(x-v))) }
# Densidade e CDF CONDICIONAIS em cada draw, com integracao exata do efeito de nova raca
# nas familias normal/skew-normal e GH numerica controlada em Student-t(5).
component <- function(x,eta,sigma,tau,lambda,family,new_breed,kind='cdf') {
  n <- length(eta); assert(length(sigma)==n,'Escala nao conforme')
  if(family=='normal') {
    s <- if(new_breed) sqrt(sigma^2+tau^2) else sigma
    return(if(kind=='cdf') pnorm(x,eta,s) else dnorm(x,eta,s,log=TRUE))
  }
  if(family=='skew') {
    delta <- lambda/sqrt(1+lambda^2)
    xi <- eta-sigma*delta*sqrt(2/pi)
    s <- if(new_breed) sqrt(sigma^2+tau^2) else sigma
    d <- if(new_breed) sigma*delta/s else delta
    al <- d/sqrt(1-d^2)
    if(kind=='cdf') return(sn_cdf(x,xi,s,al))
    z <- (x-xi)/s
    return(log(2)+dnorm(z,log=TRUE)-log(s)+pnorm(al*z,log.p=TRUE))
  }
  if(family=='t5') {
    if(!new_breed) return(if(kind=='cdf') pt((x-eta)/sigma,df=5) else dt((x-eta)/sigma,df=5,log=TRUE)-log(sigma))
    if(kind=='cdf') {
      accum <- numeric(n)
      for(k in seq_along(gh$nodes)) accum <- accum + gh$weights[k]*pt((x-eta-tau*gh$nodes[k])/sigma,df=5)
      return(accum)
    }
    dens <- numeric(n)
    for(k in seq_along(gh$nodes)) dens <- dens + gh$weights[k]*dt((x-eta-tau*gh$nodes[k])/sigma,df=5)/sigma
    return(log(pmax(dens,.Machine$double.xmin)))
  }
  stop('Familia nao programada')
}
# Quem nao aparece no treino recebe distribuicao preditiva marginal, nunca u=0 para quantis.
predict_mix <- function(dm,Xte,te,breed_index,model,family) {
  n <- nrow(Xte); D <- nrow(dm)
  B <- as.matrix(dm[,paste0('beta[',seq_len(ncol(Xte)),']'),drop=FALSE])
  A <- as.numeric(dm[,'alpha'])
  sigma <- as.numeric(dm[,if(family=='skew') 'omega' else 'sigma'])
  tau <- if(model=='M2') as.numeric(dm[,'tau']) else rep(0,D)
  lambda <- if(family=='skew') as.numeric(dm[,'lambda']) else rep(0,D)
  pred <- vector('list',n)
  for(i in seq_len(n)) {
    eta <- as.numeric(B %*% Xte[i,]) + A
    new <- is.na(breed_index[i]) && model=='M2'
    if(model=='M2' && !new) eta <- eta+tau*as.numeric(dm[,sprintf('z_breed[%d]',breed_index[i])])
    cdf <- function(q) mean(component(q,eta,sigma,tau,lambda,family,new,'cdf'))
    left <- min(eta)-12*max(sigma+tau)-2
    right <- max(eta)+12*max(sigma+tau)+2
    assert(cdf(left)<.05 && cdf(right)>.95,paste('Bracketing falhou cão',te$dog_id[i]))
    qq <- vapply(c(.05,.25,.75,.95),function(p) uniroot(function(v) cdf(v)-p,c(left,right),tol=1e-5)$root,numeric(1))
    ld <- component(te$f5_score_norm[i],eta,sigma,tau,lambda,family,new,'pdf')
    pred[[i]] <- data.frame(yhat=mean(eta),logdens=logmeanexp(ld),
                          q05=qq[1],q25=qq[2],q75=qq[3],q95=qq[4])
  }
  do.call(rbind,pred)
}
# Folds EXCLUSIVAMENTE por dog_id; SHA256 deterministico, sem desfecho no split.
make_folds <- function(ids,seed,K) {
  hashes <- vapply(ids,function(i) digest::digest(paste(seed,i,sep=':'),algo='sha256',serialize=FALSE),character(1))
  ord <- order(hashes,ids); f <- integer(length(ids)); f[ord] <- (seq_along(ids)-1L)%%K+1L
  data.frame(dog_id=ids,fold=f,sha256_split=hashes)
}
folds <- make_folds(dat$dog_id,config$seed_outer,config$K_outer)
assert(!anyDuplicated(folds$dog_id)&&all(table(folds$fold)>0),'Particao invalida')
write_csv(folds,file.path(out,'folds_NOVOS_E08_MP.csv'))
# A etapa smoke ajusta SOMENTE uma pequena amostra, sem aceitar o resultado como evidência.
if(stage=='smoke') {
  set.seed(config$seed_smoke)
  mini <- dat[sample(seq_len(nrow(dat)),40),]
  dd <- design(mini,mini); breads <- sort(unique(mini$owner_breed))
  info <- list()
  for(fam in c('t5','normal','skew')) for(m in c('M1','M2')) {
    key <- paste0(m,if(fam=='skew') '_sn' else '_old')
    sd <- list(N=nrow(mini),K=ncol(dd$Xtr),X=dd$Xtr,y=mini$f5_score_norm)
    if(fam!='skew') sd$gaussian <- as.integer(fam=='normal')
    if(m=='M2') sd <- c(sd,list(J=length(breads),breed=match(mini$owner_breed,breads),
                             tau_scale=if(fam=='skew') NULL else 0.5))
    # O candidato skew nao recebe tau_scale; o historico precisa desse campo.
    sd <- sd[!vapply(sd,is.null,logical(1))]
    fit <- mods[[key]]$sample(data=sd,seed=config$seed_smoke,chains=2,parallel_chains=2,
      iter_warmup=100,iter_sampling=100,refresh=0,
      output_dir=file.path(out,'cmdstan_csv'),output_basename=paste0('SMOKE_',fam,'_',m))
    # MP2 v1.5: falha de qualquer cadeia deve bloquear o smoke (nao apenas a existencia de CSV).
    codes <- fit$return_codes()
    fit_csv <- fit$output_files()
    write_json(list(familia=fam,modelo=m,etapa='smoke',return_codes=as.integer(codes),
                    output_files=basename(fit_csv),esperado_cadeias=2L),
               file.path(out,paste0('STATUS_SMOKE_',fam,'_',m,'.json')))
    assert(length(codes)==2L && all(!is.na(codes) & codes==0L),
           sprintf('Smoke %s/%s: retorno nao-zero ou desconhecido de CmdStan: %s',
                   fam,m,paste(codes,collapse=',')))
    assert(length(fit_csv)==2L && all(file.exists(fit_csv)),
           sprintf('Smoke %s/%s: era esperado CSV de 2 cadeias reconhecidas por CmdStanR',fam,m))
    info[[length(info)+1L]] <- data.frame(familia=fam,modelo=m,fit_csv=paste(basename(fit_csv),collapse='|'))
  }
  write_csv(do.call(rbind,info),file.path(out,'SMOKE_NAO_VALIDACAO.csv'))
  quit(save='no',status=0L)
}
assert(stage=='full','Estagio invalido')
all_pred <- list(); diag_all <- list(); post_all <- list(); feat_all <- list(); ppc_all <- list()
for(fold in seq_len(config$K_outer)) {
  idx <- folds$fold[match(dat$dog_id,folds$dog_id)]==fold
  tr <- dat[!idx,,drop=FALSE]; te <- dat[idx,,drop=FALSE]
  assert(nrow(tr)+nrow(te)==nrow(dat)&&nrow(te)>0,'Treino/teste inconsistentes')
  dd <- design(tr,te)
  breeds <- sort(unique(tr$owner_breed)); btr <- match(tr$owner_breed,breeds); bte <- match(te$owner_breed,breeds)
  n_breed <- table(tr$owner_breed); support <- as.integer(n_breed[te$owner_breed]); support[is.na(support)] <- 0L
  feat_all[[length(feat_all)+1L]] <- data.frame(fold=fold,N_train=nrow(tr),N_test=nrow(te),
    J_train=length(breeds),n_new=sum(is.na(bte)),idade_media_treino=dd$mean_age,idade_dp_treino=dd$sd_age,
    X_colunas=paste(colnames(dd$Xtr),collapse='|'))
  shared <- data.frame(dog_id=te$dog_id,fold=fold,y=te$f5_score_norm,
    age=te$f5_age_years,sex=te$sex,size=te$size,sterilized=te$sterilized,
    n_raca_treino=support,raca_vista_treino=!is.na(bte))
  baseline <- cbind(shared,familia='baseline',modelo='M0',yhat=mean(tr$f5_score_norm),
      logdens=NA_real_,q05=NA_real_,q25=NA_real_,q75=NA_real_,q95=NA_real_)
  all_pred[[length(all_pred)+1L]] <- baseline
  for(fam in c('t5','normal','skew')) for(m in c('M1','M2')) {
    key <- paste0(m,if(fam=='skew') '_sn' else '_old')
    sd <- list(N=nrow(tr),K=ncol(dd$Xtr),X=dd$Xtr,y=tr$f5_score_norm)
    if(fam!='skew') sd$gaussian <- as.integer(fam=='normal')
    if(m=='M2') sd <- c(sd,list(J=length(breeds),breed=btr,
                             tau_scale=if(fam=='skew') NULL else 0.5))
    sd <- sd[!vapply(sd,is.null,logical(1))]
    seed <- as.integer(config$seed_full+fold*100+match(fam,c('t5','normal','skew'))*10+match(m,c('M1','M2')))
    message('E08_MP FULL | ',fam,' ',m,' fold=',fold,' seed=',seed)
    fit <- mods[[key]]$sample(data=sd,chains=config$chains,parallel_chains=config$chains,
        seed=seed,iter_warmup=config$warmup,iter_sampling=config$sampling,
        adapt_delta=config$adapt_delta,max_treedepth=config$max_treedepth,refresh=200,
        output_dir=file.path(out,'cmdstan_csv'),output_basename=paste0('E08_MP_',fam,'_',m,'_f',fold))
    # MP2 v1.5: impedir que uma cadeia falha seja omitida silenciosamente no full.
    codes <- fit$return_codes()
    fit_csv <- fit$output_files()
    write_json(list(familia=fam,modelo=m,fold=fold,etapa='full',
                    return_codes=as.integer(codes),output_files=basename(fit_csv),
                    esperado_cadeias=as.integer(config$chains)),
               file.path(out,sprintf('STATUS_FULL_%s_%s_f%d.json',fam,m,fold)))
    assert(length(codes)==config$chains && all(!is.na(codes) & codes==0L),
           sprintf('Full %s/%s fold %d: retorno CmdStan invalido: %s',
                   fam,m,fold,paste(codes,collapse=',')))
    assert(length(fit_csv)==config$chains && all(file.exists(fit_csv)),
           sprintf('Full %s/%s fold %d: numero de cadeias/CSVs reconhecidos inconsistente',fam,m,fold))
    summ <- as.data.frame(fit$summary(
      variables=NULL, posterior::default_summary_measures(),
      posterior::default_convergence_measures(),
      mcse_mean=posterior::mcse_mean))
    assert(all(c('variable','rhat','ess_bulk','ess_tail','mcse_mean') %in% names(summ)),
           'Resumo MCMC sem campos obrigatorios, incluindo MCSE_mean')
    assert(all(is.finite(summ$mcse_mean)), 'MCSE_mean nao finita')
    summ$familia <- fam; summ$modelo <- m; summ$fold <- fold
    post_all[[length(post_all)+1L]] <- summ
    diag <- as.data.frame(fit$sampler_diagnostics(format='df'))
    assert(all(c('.chain','energy__','divergent__','treedepth__') %in% names(diag)),'Diagnosticos NUTS ausentes')
    chain <- split(diag,diag$.chain)
    diag_all[[length(diag_all)+1L]] <- do.call(rbind,lapply(names(chain),function(ch) {
      a <- chain[[ch]]; en <- a$energy__
      data.frame(familia=fam,modelo=m,fold=fold,chain=as.integer(ch),
        divergencias=sum(a$divergent__),n_treedepth_max=sum(a$treedepth__>=config$max_treedepth),
        ebfmi=mean(diff(en)^2)/stats::var(en),max_rhat=max(summ$rhat,na.rm=TRUE),
        min_bulk_ess=min(summ$ess_bulk,na.rm=TRUE),min_tail_ess=min(summ$ess_tail,na.rm=TRUE),
        max_mcse_mean=max(summ$mcse_mean,na.rm=TRUE))
    }))
    vars <- c('alpha','beta',if(fam=='skew') c('omega','lambda') else 'sigma',if(m=='M2') c('tau','z_breed'))
    dm <- as.matrix(posterior::as_draws_matrix(fit$draws(variables=vars)))
    assert(all(is.finite(dm)),'Draw nao finito')
    ix <- unique(as.integer(round(seq(1,nrow(dm),length.out=min(nrow(dm),config$draw_limit)))))
    dm <- dm[ix,,drop=FALSE]
    # PPC treino e posterior predictive sao conceitos diferentes: aqui somente treino.
    set.seed(seed+60000L)
    ix_ppc <- unique(as.integer(round(seq(1,nrow(dm),length.out=min(nrow(dm),200L)))))
    for(j in ix_ppc) {
      eta_tr <- as.numeric(dd$Xtr %*% as.numeric(dm[j,paste0('beta[',seq_len(ncol(dd$Xtr)),']')])) + as.numeric(dm[j,'alpha'])
      if(m=='M2') eta_tr <- eta_tr + as.numeric(dm[j,'tau']) * as.numeric(dm[j,paste0('z_breed[',btr,']')])
      sc <- as.numeric(dm[j,if(fam=='skew') 'omega' else 'sigma'])
      if(fam=='skew') {
        lam <- as.numeric(dm[j,'lambda']); delta <- lam/sqrt(1+lam^2)
        rep_y <- eta_tr + sc*(delta*(abs(rnorm(nrow(tr)))-sqrt(2/pi)) + sqrt(1-delta^2)*rnorm(nrow(tr)))
      } else if(fam=='normal') rep_y <- eta_tr + sc*rnorm(nrow(tr))
      else rep_y <- eta_tr + sc*rt(nrow(tr),5)
      sk <- function(x) {v <- stats::sd(x); if(v==0) return(NA_real_); mean((x-mean(x))^3)/v^3}
      ppc_all[[length(ppc_all)+1L]] <- data.frame(familia=fam,modelo=m,fold=fold,draw=j,
        mean_rep=mean(rep_y),sd_rep=stats::sd(rep_y),skew_rep=sk(rep_y),
        frac_abaixo_p05_treino=mean(rep_y<unname(quantile(tr$f5_score_norm,.05))),
        frac_acima_p95_treino=mean(rep_y>unname(quantile(tr$f5_score_norm,.95))),
        media_observada=mean(tr$f5_score_norm),sd_observado=stats::sd(tr$f5_score_norm),
        skew_observado=sk(tr$f5_score_norm))
    }
    p <- predict_mix(dm,dd$Xte,te,bte,m,fam)
    assert(all(is.finite(as.matrix(p))) && all(p$q05<p$q25 & p$q25<p$q75 & p$q75<p$q95),'Predicoes invalidas')
    all_pred[[length(all_pred)+1L]] <- cbind(shared,familia=fam,modelo=m,p)
    write_json(list(familia=fam,modelo=m,fold=fold,seed=seed,
      N_train=nrow(tr),N_test=nrow(te),cmdstan_csv=basename(fit$output_files()),
      posterior_draws_usados=nrow(dm),fim_utc=format(Sys.time(),tz='UTC',usetz=TRUE)),
      file.path(out,sprintf('ajuste_%s_%s_f%d.json',fam,m,fold)))
    rm(dm,fit); gc(verbose=FALSE)
  }
  write_csv(do.call(rbind,all_pred),file.path(out,'checkpoint_INCOMPLETO_predicoes.csv'))
}
pred <- do.call(rbind,all_pred)
assert(nrow(pred)==nrow(dat)*7L,'Contagem OOF inconciliavel')
assert(!anyDuplicated(pred[,c('dog_id','familia','modelo')]),'OOF duplicado')
write_csv(pred,file.path(out,'predicoes_oof_E08_MP.csv'))
write_csv(do.call(rbind,diag_all),file.path(out,'diagnosticos_mcmc_E08_MP.csv'))
write_csv(do.call(rbind,post_all),file.path(out,'posterior_E08_MP.csv'))
write_csv(do.call(rbind,feat_all),file.path(out,'features_folds_E08_MP.csv'))
write_csv(do.call(rbind,ppc_all),file.path(out,'ppc_treino_E08_MP.csv'))
scor <- function(d) {
  has <- all(is.finite(d$q05))
  data.frame(n_caes=length(unique(d$dog_id)),rmse=sqrt(mean((d$y-d$yhat)^2)),
     mae=mean(abs(d$y-d$yhat)),logscore=if(has) mean(d$logdens) else NA_real_,
     abaixo_q05=if(has) mean(d$y<d$q05) else NA_real_,
     acima_q95=if(has) mean(d$y>d$q95) else NA_real_,
     cobertura90=if(has) mean(d$y>=d$q05 & d$y<=d$q95) else NA_real_,
     cobertura50=if(has) mean(d$y>=d$q25 & d$y<=d$q75) else NA_real_,
     largura90=if(has) mean(d$q95-d$q05) else NA_real_,
     interval_score90=if(has) mean((d$q95-d$q05)+20*(d$q05-d$y)*as.numeric(d$y<d$q05)+
                                        20*(d$y-d$q95)*as.numeric(d$y>d$q95)) else NA_real_)
}
met <- do.call(rbind,lapply(split(pred,interaction(pred$familia,pred$modelo,drop=TRUE)),function(d) {
 cbind(familia=d$familia[1],modelo=d$modelo[1],scor(d))
}))
rownames(met)<-NULL
write_csv(met,file.path(out,'metricas_E08_MP.csv'))
con <- do.call(rbind,lapply(c('t5','normal','skew'),function(f) {
 d <- met[met$familia==f,,drop=FALSE]
 a <- d[d$modelo=='M1',]; b <- d[d$modelo=='M2',]
 data.frame(familia=f,rmse_M1=a$rmse,rmse_M2=b$rmse,delta_rmse_M1_menos_M2=a$rmse-b$rmse,
    delta_logscore_M2_menos_M1=b$logscore-a$logscore,
    delta_interval_score90_M1_menos_M2=a$interval_score90-b$interval_score90)
}))
write_csv(con,file.path(out,'contrastes_E08_MP.csv'))
# Estratos predefinidos; quintis de previsao sao diagnosticos e nao selecionadores.
d2 <- pred[pred$modelo!='M0',,drop=FALSE]
d2$suporte <- ifelse(d2$n_raca_treino==0,'nova',ifelse(d2$n_raca_treino<5,'1-4',ifelse(d2$n_raca_treino<20,'5-19','>=20')))
d2$idade_faixa <- cut(d2$age,breaks=c(-Inf,3,7,12,Inf),labels=c('ate_3','3_a_7','7_a_12','acima_12'))
strat <- list()
for(family in c('t5','normal','skew')) for(m in c('M1','M2')) {
 ds <- d2[d2$familia==family & d2$modelo==m,,drop=FALSE]
 ds$quintil_pred <- as.integer(cut(rank(ds$yhat,ties.method='first'),breaks=5,labels=FALSE))
 for(group in c('suporte','idade_faixa','sex','size','sterilized','quintil_pred')) {
  spl <- split(ds,as.character(ds[[group]]),drop=TRUE)
  for(lv in names(spl)) {
   dd <- spl[[lv]]
   strat[[length(strat)+1L]] <- cbind(familia=family,modelo=m,estrato=group,nivel=lv,
     status=if(length(unique(dd$dog_id))<50) 'DESCRITIVO_N_INSUFICIENTE' else 'DESCRITIVO',scor(dd))
  }
 }
}
write_csv(do.call(rbind,strat),file.path(out,'metricas_estratos_E08_MP.csv'))
# Bootstrap pareado por caes; nao representa 2.084 tutores independentes.
set.seed(config$seed_bootstrap)
ids <- dat$dog_id; B <- config$bootstrap_B
bs <- list()
for(family in c('t5','normal','skew')) {
 d <- d2[d2$familia==family,,drop=FALSE]
 a <- d[d$modelo=='M1',]; b <- d[d$modelo=='M2',]
 a <- a[match(ids,a$dog_id),]; b <- b[match(ids,b$dog_id),]
 assert(identical(a$dog_id,b$dog_id),'Bootstrap nao pareado')
 vals <- matrix(NA_real_,B,3L)
 for(z in seq_len(B)) {
  ix <- sample.int(length(ids),replace=TRUE)
  vals[z,] <- c(sqrt(mean((a$y[ix]-a$yhat[ix])^2))-sqrt(mean((b$y[ix]-b$yhat[ix])^2)),
    mean(b$logdens[ix]-a$logdens[ix]),
    mean((a$q95[ix]-a$q05[ix])+20*(a$q05[ix]-a$y[ix])*(a$y[ix]<a$q05[ix])+20*(a$y[ix]-a$q95[ix])*(a$y[ix]>a$q95[ix]))-
    mean((b$q95[ix]-b$q05[ix])+20*(b$q05[ix]-b$y[ix])*(b$y[ix]<b$q05[ix])+20*(b$y[ix]-b$q95[ix])*(b$y[ix]>b$q95[ix])))
 }
 for(j in seq_len(ncol(vals))) bs[[length(bs)+1L]] <- data.frame(familia=family,
   contraste=c('delta_RMSE_M1_menos_M2','delta_logscore_M2_menos_M1','delta_IS90_M1_menos_M2')[j],
   LI95=unname(quantile(vals[,j],.025)),LS95=unname(quantile(vals[,j],.975)),
   bootstrap_B=B,unidade='cao',aviso='tutor_id_indisponivel')
}
write_csv(do.call(rbind,bs),file.path(out,'bootstrap_pareado_E08_MP.csv'))
# Incerteza de cada cauda por CAO, nao pelas linhas de uma validacao repetida.
tail_boot <- list()
for(family in c('t5','normal','skew')) for(m in c('M1','M2')) {
  d <- d2[d2$familia==family & d2$modelo==m,,drop=FALSE]
  d <- d[match(ids,d$dog_id),,drop=FALSE]
  assert(identical(d$dog_id,ids),'Taxa de cauda sem alinhamento por cao')
  draws <- matrix(NA_real_,B,2L)
  for(z in seq_len(B)) {
    ii <- sample.int(length(ids),replace=TRUE)
    draws[z,1] <- mean(d$y[ii]<d$q05[ii])
    draws[z,2] <- mean(d$y[ii]>d$q95[ii])
  }
  for(k in 1:2) tail_boot[[length(tail_boot)+1L]] <- data.frame(
    familia=family,modelo=m,metrica=c('abaixo_q05','acima_q95')[k],
    estimativa=mean(if(k==1) d$y<d$q05 else d$y>d$q95),
    LI95=unname(quantile(draws[,k],.025)),LS95=unname(quantile(draws[,k],.975)),
    bootstrap_B=B,unidade='cao',limite='tutor_id_indisponivel')
}
write_csv(do.call(rbind,tail_boot),file.path(out,'bootstrap_caudas_E08_MP.csv'))
# Comparacao pareada da nova familia versus C0 no MESMO fold/cao.
family_boot <- list()
for(m in c('M1','M2')) for(comparator in c('t5','normal')) {
  a <- d2[d2$familia=='skew' & d2$modelo==m,,drop=FALSE]
  b <- d2[d2$familia==comparator & d2$modelo==m,,drop=FALSE]
  a <- a[match(ids,a$dog_id),]; b <- b[match(ids,b$dog_id),]
  assert(identical(a$dog_id,b$dog_id),'Familias nao pareadas')
  db <- matrix(NA_real_,B,3L)
  score_vec <- function(d,ix) mean((d$q95[ix]-d$q05[ix])+20*(d$q05[ix]-d$y[ix])*(d$y[ix]<d$q05[ix])+
                                  20*(d$y[ix]-d$q95[ix])*(d$y[ix]>d$q95[ix]))
  for(z in seq_len(B)) {
    ix <- sample.int(length(ids),replace=TRUE)
    db[z,] <- c(mean(a$logdens[ix]-b$logdens[ix]),
      score_vec(b,ix)-score_vec(a,ix),
      sqrt(mean((b$y[ix]-b$yhat[ix])^2))-sqrt(mean((a$y[ix]-a$yhat[ix])^2)))
  }
  full_estim <- c(mean(a$logdens-b$logdens),
    score_vec(b,seq_along(ids))-score_vec(a,seq_along(ids)),
    sqrt(mean((b$y-b$yhat)^2))-sqrt(mean((a$y-a$yhat)^2)))
  for(k in 1:3) family_boot[[length(family_boot)+1L]] <- data.frame(modelo=m,
    contraste=paste0('skew_vs_',comparator),
    metrica=c('delta_logscore_skew_menos_ref','delta_IS90_ref_menos_skew','delta_RMSE_ref_menos_skew')[k],
    estimativa=full_estim[k],LI95=unname(quantile(db[,k],.025)),
    LS95=unname(quantile(db[,k],.975)),bootstrap_B=B,unidade='cao')
}
write_csv(do.call(rbind,family_boot),file.path(out,'bootstrap_familias_E08_MP.csv'))
all_diag <- do.call(rbind,diag_all)
flag <- any(!is.finite(all_diag$ebfmi)|all_diag$ebfmi<.3|all_diag$divergencias>0|
            all_diag$n_treedepth_max>0|all_diag$max_rhat>1.01|
            all_diag$min_bulk_ess<400|all_diag$min_tail_ess<400)
write_json(list(status='EXECUTADO_NAO_VALIDADO',
  gate='PENDENTE: sem holdout externo; revisao independente e PPC treino completos ainda obrigatorios',
  flag_mcmc=flag,n_caes=nrow(dat),linhas_oof=nrow(pred),
  n_modelos_fit=3L*2L*config$K_outer,
  historico_explorado='Os mesmos caes ja foram examinados em E08',
  timestamp_utc=format(Sys.time(),tz='UTC',usetz=TRUE)),file.path(out,'EXECUCAO_COMPLETA_NAO_VALIDADA.json'))
unlink(file.path(out,'checkpoint_INCOMPLETO_predicoes.csv'))
writeLines('COMPUTACAO CONCLUIDA. GATE E08_MP NAO APROVADO AUTOMATICAMENTE.',file.path(out,'LEIA_RESULTADO.txt'))
