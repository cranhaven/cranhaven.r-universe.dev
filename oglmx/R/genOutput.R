# collection of functions used to produce regression output

# works
.regtype.oglmx<-function(object){
  if (ncol(object$NoVarModData)==2){
    if (sum(object$NoVarModData$weights==1)!=nrow(object$NoVarModData)){ # this is needed for backwards compatibility, for saved data before v3
      NotWeight<-FALSE
    } else {NotWeight<-TRUE}
  } else {NotWeight<-TRUE}
  Zero<-ifelse(NotWeight,"","Weighted ")
  First<-ifelse(object$Hetero,"Heteroskedastic ","")
  Second<-ifelse(object$NOutcomes>2,"Ordered ","")
  if (object$link=="logit"){
    Third<-"Logit "
  } else if (object$link=="probit"){
    Third<-"Probit "
  } else if (object$link=="cloglog"){
    Third<-"CLogLog "
  } else if (object$link=="loglog"){
    Third<-"LogLog "
  } else if (object$link=="cauchit"){
    Third<-"Cauchit "
  }
  Fourth<-"Regression"
  value<-paste(Zero,First,Second,Third,Fourth,sep="")
  return(value)
}

# working
nobs.oglmx<-function(object, ...){
  return(attr(object$loglikelihood,"No.Obs"))
}

# working
logLik.oglmx<-function(object, ...){
  value<-object$loglikelihood[1]
  attr(value,"df")<-length(object$coefficients)
  return(value)
}

# works with output of new version, need to check with output of old version
.BaseLL<-function(object){
  data<-object$NoVarModData
  if (ncol(data)==2){
    BaseLL<-as.numeric(logLik(oglmx(Y~1,data=data,weights=data$weights)))
  } else {
    BaseLL<-as.numeric(logLik(oglmx(Y~1,data=data)))
  }
  return(BaseLL)
}


# working
vcov.oglmx<-function(object,tol=1e-20,...){
  if (is.null(object$BHHHhessian)){
    vcov<-qr.solve(-object$hessian,tol=tol)
  } else {
    vcov<- qr.solve(object$hessian,tol=tol)%*%(object$BHHHhessian*(attr(object$loglikelihood,"No.Obs")/(attr(object$loglikelihood,"No.Obs")-1)))%*%qr.solve(object$hessian,tol=tol)
  }
  colnames(vcov)<-rownames(vcov)<-names(object$coefficients)
  return(vcov)
}

# working
summary.oglmx<-function(object,tol=1e-20, ... ){
  stdEr.oglmx<-diag(vcov(object,tol=tol))^0.5
  t<-object$coefficients/stdEr.oglmx
  p <- 2*pnorm( -abs( t))
  results <- cbind("Estimate"=object$coefficients,
                   "Std. error"=stdEr.oglmx,
                   "t value"=t, "Pr(>|t|)"=p)
  betaresults<-results[attr(object$coefficients,"coefftypes")[[1]], ,drop=FALSE]
  deltaresults<-results[attr(object$coefficients,"coefftypes")[[2]], ,drop=FALSE]
  cutoffresults<-results[attr(object$coefficients,"coefftypes")[[3]], ,drop=FALSE]
  resultsSplit<-list(betaresults,deltaresults,cutoffresults)
  summary<-list(regtype=.regtype.oglmx(object),loglikelihood=object$loglikelihood,estimate=results,estimateDisplay=resultsSplit,no.iterations=object$no.iterations,McFaddensR2=McFaddensR2.oglmx(object),AIC=AIC(object),coefficients=object$coefficients)
  class(summary)<-"summary.oglmx"
  summary
}

# working
print.summary.oglmx<-function(x, ... ){
  cat(x$regtype,"\n")
  cat("Log-Likelihood:", x$loglikelihood, "\n")
  cat("No. Iterations:", x$no.iterations, "\n")
  cat("McFadden's R2:",x$McFaddensR2,"\n")
  cat("AIC:",x$AIC,"\n")
  if (nrow(x$estimateDisplay[[1]])>0 & nrow(x$estimateDisplay[[2]])==0 & nrow(x$estimateDisplay[[3]])==0){
    printCoefmat(x$estimateDisplay[[1]])
  } else if (nrow(x$estimateDisplay[[1]])>0){
    if (nrow(x$estimateDisplay[[2]])>0){
      cat("-----","Mean Equation","------\n")
    }
    printCoefmat(x$estimateDisplay[[1]],signif.legend=FALSE)
  }
  if (nrow(x$estimateDisplay[[2]])>0){
    if (nrow(x$estimateDisplay[[1]])>0){
      cat("-----","SD Equation","------\n")
    }
    if (nrow(x$estimateDisplay[[3]])>0){
      printCoefmat(x$estimateDisplay[[2]],signif.legend=FALSE)
    } else {
      printCoefmat(x$estimateDisplay[[2]])
    }
  }
  if (nrow(x$estimateDisplay[[3]])>0){
    cat("-----","Threshold Parameters","-----\n")
    printCoefmat(x$estimateDisplay[[3]])
  }
}

# works
McFaddensR2.oglmx<-function(object){
  value<-1-logLik(object)/.BaseLL(object)
  return(value)
}

# works
AIC.oglmx<-function(object, ..., k=2){
  # 2*number of estimated parameters - 2*log likelihood
  value<-k*length(object$coefficients)-2*logLik(object)
  return(value)
}

# works
logLik.summary.oglmx<-function(object, ...){
  value<-object$loglikelihood[1]
  attr(value,"df")<-length(object$coefficients)
  return(value)
}


# works
coef.oglmx<-function(object, ...){
  attr(object$coefficients,"coefftypes")<-NULL
  return(object$coefficients)
}

# works
coef.summary.oglmx<-function(object, ...){
  attr(object$coefficients,"coefftypes")<-NULL
  return(object$coefficients)
}

print.margins.oglmx<-function(x, ... ){
  for (m in 1:length(x)){
    cat("Marginal Effects on Pr(Outcome==",names(x)[m],")","\n",sep="")
    if (m==length(x)){
      printCoefmat(x[[m]])
    } else {
      printCoefmat(x[[m]],signif.legend=FALSE)
      cat("------------------------------------","\n")
    }
  }
}
