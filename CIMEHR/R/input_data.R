.validate_data <- function(data,id,time,outcome,obs_indicator,covariates,entry_col,censor_col) {
  if(!is.data.frame(data)||!nrow(data))stop('data must be a nonempty data.frame.',call.=FALSE)
  required<-unique(c(id,time,outcome,obs_indicator,covariates,entry_col,censor_col))
  missing<-setdiff(required,names(data))
  if(length(missing))stop('Missing columns: ',paste(missing,collapse=', '),call.=FALSE)
  if(anyNA(data[[id]]))stop('Subject IDs cannot be missing.',call.=FALSE)
  numeric_cols<-setdiff(required,id)
  bad<-numeric_cols[!vapply(data[numeric_cols],is.numeric,logical(1))]
  if(length(bad))stop('Numeric columns required: ',paste(bad,collapse=', '),'. Encode factors explicitly with model.matrix() before fitting.',call.=FALSE)
  if(any(is.infinite(data[[time]])))stop('Visit times must be finite or NA for a zero-visit placeholder.',call.=FALSE)
  visit<-is.finite(data[[time]])
  groups<-match(data[[id]],unique(data[[id]])); nsub<-max(groups)
  placeholders<-tabulate(groups[!visit],nsub); counts<-tabulate(groups[visit],nsub)
  if(any(placeholders>1L)||any(placeholders>0L&counts>0L))
    stop('A zero-visit subject has exactly one time=NA placeholder and no actual visit rows.',call.=FALSE)
  if(any(!visit & !is.na(data[[obs_indicator]]) & data[[obs_indicator]]!=0))
    stop('A zero-visit placeholder cannot have a recorded outcome indicator.',call.=FALSE)
  for(column in c(entry_col,censor_col)) {
    v<-data[[column]]
    if(any(!is.finite(v)))stop('Follow-up columns must be finite for every subject: ',column,call.=FALSE)
    first<-match(seq_len(nsub),groups)
    if(any(v!=v[first][groups]))stop('Follow-up columns must be constant within subject: ',column,call.=FALSE)
  }
  if(!is.null(censor_col)&&any(visit & data[[time]]>data[[censor_col]]))
    stop('Visit times cannot exceed the supplied censoring time.',call.=FALSE)
  if(!is.null(entry_col)&&any(visit & data[[time]]<data[[entry_col]]))
    stop('Visit times cannot precede the supplied entry time.',call.=FALSE)
  if(!is.null(entry_col)&&!is.null(censor_col)&&any(data[[entry_col]]>data[[censor_col]]))
    stop('Entry time cannot exceed censoring time.',call.=FALSE)
  if(any(visit & (is.na(data[[obs_indicator]])|!data[[obs_indicator]]%in%c(0,1))))
    stop('Every visit requires a recording indicator of 0 or 1.',call.=FALSE)
  if(any(visit & data[[obs_indicator]]==1 & !is.finite(data[[outcome]]),na.rm=TRUE))
    stop('Recorded visits require a finite outcome.',call.=FALSE)
  invisible(TRUE)
}

.mask_entry_visits <- function(data,id,time,outcome,obs_indicator,entry_col) {
  if(is.null(entry_col))return(list(data=data,n=0L))
  at<-is.finite(data[[time]])&data[[time]]==data[[entry_col]]
  if(!any(at))return(list(data=data,n=0L))
  ids<-data[[id]];lost<-setdiff(unique(ids[at]),unique(ids[!at]))
  ph<-data[match(lost,ids),,drop=FALSE]
  ph[[time]]<-rep(NA_real_,nrow(ph));ph[[outcome]]<-rep(NA_real_,nrow(ph));ph[[obs_indicator]]<-rep(NA,nrow(ph))
  out<-rbind(data[!at,,drop=FALSE],ph)
  out<-out[order(match(out[[id]],unique(ids)),is.na(out[[time]])),,drop=FALSE]
  rownames(out)<-NULL
  list(data=out,n=sum(at))
}

.cimehr_check_numeric_columns <- function(data, columns) {
  columns <- unique(columns)
  if (!is.null(columns) && !is.character(columns))
    stop("Covariate names must be character vectors or NULL.", call. = FALSE)
  present <- intersect(columns, names(data))
  bad <- present[!vapply(data[present], is.numeric, logical(1L))]
  if (length(bad)) stop("Modeled covariates must be numeric; use model.matrix() to create explicit contrasts: ",
    paste(bad, collapse = ", "), call. = FALSE)
  invisible(NULL)
}
