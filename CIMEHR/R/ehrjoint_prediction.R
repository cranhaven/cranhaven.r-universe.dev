.trajectory_ehr_baseline <- function(object,times,bandwidth) {
  b<-object$prediction$baseline$increments
  if(!is.data.frame(b)||!all(c('time','dA','dLambda')%in%names(b)))
    stop('This EHRJoint fit lacks baseline increments; refit with the updated package.',call.=FALSE)
  data.frame(time=times,fit=.trajectory_local_linear(b$time,b$dA,b$dLambda,times,bandwidth))
}

.trajectory_ehr_mean <- function(object,newdata,times,bandwidth) {
  spec<-object$prediction
  if(is.null(spec))stop('This EHRJoint fit lacks its prediction specification.',call.=FALSE)
  if(is.null(newdata))newdata<-spec$reference
  if(!is.data.frame(newdata)||!nrow(newdata))stop('newdata must be a nonempty data.frame.',call.=FALSE)
  required<-spec$long_covs
  if(!all(required%in%names(newdata)))stop('newdata is missing: ',paste(setdiff(required,names(newdata)),collapse=', '),call.=FALSE)
  if(any(!vapply(newdata[,required,drop=FALSE],is.numeric,logical(1))))
    stop('Outcome covariate profiles must be numeric, as in the fitted model.',call.=FALSE)
  X<-as.matrix(newdata[,required,drop=FALSE])
  if(any(!is.finite(X)))stop('Outcome covariate profiles must be finite.',call.=FALSE)
  fixed<-drop(X%*%spec$coefficients[required])
  b<-.trajectory_ehr_baseline(object,times,bandwidth)$fit
  data.frame(profile=rep(seq_len(nrow(newdata)),each=length(times)),
             time=rep(times,nrow(newdata)),fit=rep(b,nrow(newdata))+rep(fixed,each=length(times)))
}
