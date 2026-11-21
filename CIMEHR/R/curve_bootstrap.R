.curve_bootstrap_integer <- function(x, name, minimum) {
  if(!is.numeric(x)||length(x)!=1L||!is.finite(x)||x<minimum||x!=floor(x)||x>.Machine$integer.max)
    stop(name,' must be one integer >= ',minimum,'.',call.=FALSE)
  as.integer(x)
}

.curve_bootstrap <- function(object, template, type, newdata, times,
    aggregate, weights, bandwidth, B, seed, workers, level) {
  B<-.curve_bootstrap_integer(B,'B',2L)
  workers<-.curve_bootstrap_integer(workers,'workers',1L)
  if(!is.numeric(level)||length(level)!=1L||!is.finite(level)||level<=0||level>=1)
    stop('conf.int must lie strictly between zero and one.',call.=FALSE)
  if(is.null(object$refit)||is.null(object$refit$data)||is.null(object$refit$arguments))
    stop('This fit does not retain bootstrap data and settings; refit with the updated package.',call.=FALSE)
  tab<-.outcome_table(object)
  if(!nrow(tab)||any(!tab$point_valid))
    stop('Bootstrap curves require an identified, converged original outcome fit.',call.=FALSE)
  if(type=='population'&&is.null(newdata)&&!is.null(weights))
    stop('Supply newdata explicitly for a weighted bootstrap population curve.',call.=FALSE)
  if(type=='population'&&is.null(newdata)&&!aggregate)
    newdata<-.trajectory_reference_profiles(object,NULL)
  if(!is.null(seed)) {
    seed<-.curve_bootstrap_integer(seed,'seed',0L)
    had_seed<-exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE)
    if(had_seed)old_seed<-get('.Random.seed',envir=.GlobalEnv,inherits=FALSE)
    on.exit(if(had_seed)assign('.Random.seed',old_seed,envir=.GlobalEnv) else
      if(exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE))rm('.Random.seed',envir=.GlobalEnv),add=TRUE)
    set.seed(seed)
  }
  n<-length(object$refit$subject_ids)
  if(n<2L)stop('Participant bootstrap requires at least two participants.',call.=FALSE)
  samples<-lapply(seq_len(B),function(i)sample.int(n,n,replace=TRUE))
  seeds<-sample.int(.Machine$integer.max,B,replace=FALSE)
  key<-function(d)paste(if('profile'%in%names(d))d$profile else 1L,d$time,sep=':')
  target<-key(template)
  fields<-function(d) {
    z<-list(if('profile'%in%names(d))d$profile else 1L,d$time)
    if(nrow(d)>0L&&length(z[[2L]])==nrow(d)&&all(vapply(z,function(x)is.numeric(x)&&!is.object(x)&&
      is.null(dim(x))&&length(x)%in%c(1L,nrow(d)),logical(1))))z
  }
  label<-function(x){s<-as.character(x);s[is.na(s)]<-'NA';s}
  lev<-lapply(fields(template),function(x)unique(label(unique(x))))
  code<-function(d) {
    z<-fields(d)
    if(is.null(z)||!length(lev))return(NULL)
    k<-Map(function(x,l){u<-unique(x);match(label(u),l)[match(x,u)]},z,lev)
    (k[[1L]]-1)*length(lev[[2L]])+k[[2L]]
  }
  coded<-code(template)
  rows<-tryCatch(suppressWarnings(.curve_resample_rows(object$refit)),error=function(e)NULL)
  one<-function(i) {
    fitted<-FALSE;stage<-'fit';messages<-character()
    ans<-tryCatch(withCallingHandlers({
      f<-.curve_bootstrap_refit(object,samples[[i]],seeds[i],rows)
      messages<-c(messages,f$diagnostics$warnings %||% character())
      t<-.outcome_table(f)
      if(!nrow(t)||any(!t$point_valid))stop('Refitted outcome model is not identified or converged.')
      fitted<-TRUE;stage<-'curve'
      domain<-.trajectory_range(f)
      tt<-times[times>=domain[1L]-1e-10&times<=domain[2L]+1e-10]
      values<-rep(NA_real_,nrow(template))
      if(length(tt)) {
        d<-if(type=='baseline')baseline_mean(f,tt,bandwidth,se.fit=FALSE) else
          population_trajectory(f,newdata,tt,bandwidth,aggregate,weights,se.fit=FALSE)
        at<-if(is.null(coded)||is.null(cd<-code(d)))match(key(d),target) else match(cd,coded)
        if(anyNA(at)||anyDuplicated(at))stop('Bootstrap prediction profiles do not align with the original curve.')
        values[at]<-d$fit
      }
      missing<-sum(!is.finite(values))
      list(values=values,fitted=TRUE,stage=if(missing)'curve'else'',
        reason=if(missing)paste(missing,'curve points unsupported or nonfinite in this resample.')else'',
        warnings=paste(unique(messages),collapse=' | '))
    },warning=function(w){messages<<-c(messages,conditionMessage(w));invokeRestart('muffleWarning')}),
    error=function(e)list(values=rep(NA_real_,nrow(template)),fitted=fitted,
      stage=stage,reason=conditionMessage(e),warnings=paste(unique(messages),collapse=' | ')))
    ans
  }
  workers<-min(workers,B)
  if(.Platform$OS.type=='windows'&&workers>1L) {
    warning('Multicore bootstrap is unavailable on Windows; using one worker.',call.=FALSE)
    workers<-1L
  }
  res<-if(workers>1L)parallel::mclapply(seq_len(B),one,mc.cores=workers,
    mc.preschedule=FALSE,mc.set.seed=FALSE) else lapply(seq_len(B),one)
  for(i in seq_len(B))if(!is.list(res[[i]])||is.null(res[[i]]$values))
    res[[i]]<-list(values=rep(NA_real_,nrow(template)),fitted=FALSE,
      stage='worker',reason=paste(as.character(res[[i]]),collapse=' '),warnings='')
  draws<-do.call(rbind,lapply(res,`[[`,'values'))
  dimnames(draws)<-list(paste0('bootstrap_',seq_len(B)),target)
  counts<-colSums(is.finite(draws))
  sdj<-function(j) {
    z<-draws[,j];z<-z[is.finite(z)]
    if(length(z)<2L)NA_real_ else stats::sd(z)
  }
  probe<-unname(which(counts>=2L))
  if(length(probe))probe<-sort(unique(c(probe[round(seq(1,length(probe),length.out=min(length(probe),64L)))],
    probe[!duplicated(counts[probe])])))
  reference<-vapply(probe,sdj,numeric(1))
  se<-NULL
  for(mode in 1:0)if(is.null(se)) {
    s<-cimehr_sp_column_sd(draws,mode)
    if(identical(s[probe],reference))se<-s
  }
  if(is.null(se))se<-vapply(seq_len(ncol(draws)),sdj,numeric(1))
  failures<-data.frame(replicate=seq_len(B),stage=vapply(res,`[[`,character(1),'stage'),
    reason=vapply(res,`[[`,character(1),'reason'),stringsAsFactors=FALSE)
  failures<-failures[nzchar(failures$reason),,drop=FALSE]
  warnings<-data.frame(replicate=seq_len(B),message=vapply(res,`[[`,character(1),'warnings'),stringsAsFactors=FALSE)
  warnings<-warnings[nzchar(warnings$message),,drop=FALSE]
  if(any(counts<B))warning(sprintf(
    'Some bootstrap fits or curve points were unavailable: %d-%d of %d usable replicates per point. Inspect $bootstrap$failures and $data$n_boot; fewer than two give unavailable SEs and intervals.',
    min(counts),max(counts),B),call.=FALSE)
  q<-stats::qnorm((1+level)/2)
  template$se<-se;template$lower<-template$fit-q*se;template$upper<-template$fit+q*se
  template$n_boot<-counts
  template<-.report_curve_inference_metadata(template,object)
  list(data=template,bootstrap=list(B=B,successful_fits=sum(vapply(res,`[[`,logical(1),'fitted')),
    n_success=counts,replicates=draws,failures=failures,warnings=warnings,seeds=seeds,
    seed=seed,workers=workers,confidence_level=level,method='participant',interval='Wald'))
}
