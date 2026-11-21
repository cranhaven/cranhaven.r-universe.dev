.curve_refit_methods <- function() c(
  'cimehr','cimehr_hl','cimehr_spline','cimehr_spline_hl',
  'liang_visit','liang_marker','mi_liang','li_liang','ehrjoint')

.curve_capture_refit <- function(data, arguments) {
  if(!is.data.frame(data)||!nrow(data)||!is.list(arguments)||
     is.null(names(arguments))||anyDuplicated(names(arguments))||
     any(!nzchar(names(arguments))))
    stop('Cannot retain invalid mean-curve refitting inputs.',call.=FALSE)
  if('data'%in%names(arguments))
    stop('Refitting data must be stored separately from its arguments.',call.=FALSE)
  if(length(arguments$method)!=1L||!arguments$method%in%.curve_refit_methods())
    stop('Mean-curve refitting is unavailable for this method.',call.=FALSE)
  id<-arguments$id
  if(length(id)!=1L||!id%in%names(data)||anyNA(data[[id]]))
    stop('Mean-curve refitting requires a valid subject identifier.',call.=FALSE)
  # arguments contains evaluated values, including formula objects.
  list(version=1L,data=as.data.frame(data),arguments=arguments,
       subject_ids=unique(data[[id]]))
}

.curve_resample_rows <- function(refit) {
  d<-refit$data
  split(seq_len(nrow(d)),match(d[[refit$arguments$id]],refit$subject_ids))
}

.curve_resample_subjects <- function(refit, indices, rows=NULL) {
  if(!is.list(refit)||!identical(refit$version,1L)||
     !is.data.frame(refit$data)||!is.list(refit$arguments))
    stop('This fit lacks retained refitting data; refit with the updated package.',call.=FALSE)
  d<-refit$data;id<-refit$arguments$id;ids<-refit$subject_ids
  if(length(id)!=1L||!id%in%names(d)||!length(ids)||
     anyNA(d[[id]])||!identical(unique(d[[id]]),ids))
    stop('The retained subject information is inconsistent.',call.=FALSE)
  if(!is.numeric(indices)||!length(indices)||any(!is.finite(indices))||
     any(indices!=floor(indices))||any(indices<1|indices>length(ids)))
    stop('indices must be valid integer indices of the retained subjects.',call.=FALSE)
  indices<-as.integer(indices)
  if(is.null(rows))rows<-split(seq_len(nrow(d)),match(d[[id]],ids))
  blocks<-rows[indices]
  take<-unlist(blocks,use.names=FALSE)
  if(identical(oldClass(d),'data.frame')) {
    out<-lapply(d,function(x)if(length(dim(x))!=2L)x[take] else x[take,,drop=FALSE])
    a<-attributes(d)
    attributes(out)<-c(a[setdiff(names(a),c('row.names','class'))],
      list(row.names=.set_row_names(length(take)),class='data.frame'))
  } else out<-d[take,,drop=FALSE]
  out[[id]]<-rep.int(seq_along(indices),lengths(blocks))
  rownames(out)<-NULL
  mapping<-data.frame(new_id=seq_along(indices),source_index=indices,
    source_id=ids[indices],stringsAsFactors=FALSE,check.names=FALSE)
  list(data=out,mapping=mapping)
}

.curve_bootstrap_refit <- function(object, indices, seed, rows=NULL) {
  if(!inherits(object,'cimehr_fit'))
    stop('Supply a fitted CIMEHR package model.',call.=FALSE)
  if(!is.numeric(seed)||length(seed)!=1L||!is.finite(seed)||
     seed!=floor(seed)||abs(seed)>.Machine$integer.max)
    stop('seed must be one finite integer.',call.=FALSE)
  seed<-as.integer(seed)
  sample<-.curve_resample_subjects(object$refit,indices,rows)
  args<-object$refit$arguments
  args$data<-sample$data;args$seed<-seed
  args$controls$nuisance_fit<-NULL
  for(nm in intersect(c('seed','pair_seed','bootstrap_seed'),names(args$controls)))
    args$controls[[nm]]<-seed
  old_kind<-RNGkind()
  had_seed<-exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE)
  if(had_seed)old_seed<-get('.Random.seed',envir=.GlobalEnv,inherits=FALSE)
  on.exit({
    do.call(RNGkind,as.list(old_kind))
    if(had_seed)assign('.Random.seed',old_seed,envir=.GlobalEnv)
    else if(exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE))
      rm('.Random.seed',envir=.GlobalEnv)
  },add=TRUE)
  set.seed(seed)
  ans<-.cimehr_without_identification_signal(do.call(fit_method,args))
  ans$refit<-NULL
  ans$bootstrap_sample<-sample$mapping
  ans$bootstrap_seed<-seed
  ans
}
