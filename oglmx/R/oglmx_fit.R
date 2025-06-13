oglmx.fit<-function(outcomeMatrix,X,Z,w,beta,delta,threshparam,link,start,sdmodel,optmeth="maxLik",analhessian,robust){
  
  # outcomeMatrix<-results.oprobhetOGLMXENV[[1]]
  # X<-results.oprobhetOGLMXENV[[2]]
  # Z<-results.oprobhetOGLMXENV[[3]]
  # w<-results.oprobhetOGLMXENV[[4]]
  # beta<-results.oprobhetOGLMXENV[[5]]
  # delta<-results.oprobhetOGLMXENV[[6]]
  # threshparam<-results.oprobhetOGLMXENV[[7]]
  # start<-results.oprobhetOGLMXENV[[8]]
  # optmeth<-results.oprobhetOGLMXENV[[9]]
  # analhessian<-results.oprobhetOGLMXENV[[10]]
  # sdmodel<-results.oprobhetOGLMXENV[[11]]
  # robust<-results.oprobhetOGLMXENV[[12]]
  # link<-results.oprobhetOGLMXENV[[13]]
  
  
  whichXest<-is.na(beta)
  whichZest<-is.na(delta)
  no.est.beta<-sum(whichXest)
  no.est.delta<-sum(whichZest)
  whichAlphaest<-is.na(threshparam)
  no.est.thresh<-sum(whichAlphaest)
  no.estparams<-no.est.beta+no.est.delta+no.est.thresh
  if (!is.null(start) & length(start)!=no.estparams){
    stop("Specified vector of start values for parameters is of incorrect length.")
  }
  
  whichparametersmean<-c(!logical(no.est.beta),logical(no.est.delta+no.est.thresh))
  whichparametersthresh<-c(logical(no.est.beta+no.est.delta),!logical(no.est.thresh))
  whichparametersscale<-c(logical(no.est.beta),!logical(no.est.delta),logical(no.est.thresh))
  
  if (sum(whichZest)>0){
    gsdmodel<-D(sdmodel,"z")
    hsdmodel<-D(gsdmodel,"z")
  } else {gsdmodel<-NULL; hsdmodel<-NULL}
  
  CalcEnv<-list(outcomeMat=outcomeMatrix,X=X,Z=Z,w=w,beta=beta,delta=delta,threshparam=threshparam,
            link=link,sdmodel=sdmodel,gsdmodel=gsdmodel,hsdmodel=hsdmodel,whichparametersmean=whichparametersmean,
            whichparametersthresh=whichparametersthresh,whichparametersscale=whichparametersscale,
            whichXest=whichXest,whichZest=whichZest,whichAlphaest=whichAlphaest,analhessian=analhessian)
  CalcEnv<-list2env(CalcEnv)
  
  parametertypes<-list(whichparametersmean,whichparametersscale,whichparametersthresh)
  if (is.null(start)){
    start<-calcstartvalues(parametertypes,sdmodel,threshparam)
  }
  
  
  
  if (optmeth=="maxLik"){
    maximum<-oglmx.maxlik(CalcEnv,start)
    results<-collectmaxLikOutput(maximum)
  }
  
  outcomenames<-colnames(outcomeMatrix)
  threshnames<-sapply(c(2:length(outcomenames)),function(x){paste("Threshold (",outcomenames[x-1],"->",outcomenames[x],")",sep="")})
  names(results$coefficients)<-c(colnames(X)[whichXest],colnames(Z)[whichZest],threshnames[whichAlphaest])
                      
  
  attr(results$coefficients,"coefftypes")<-list(whichparametersmean,whichparametersscale,whichparametersthresh)
  results$allparams<-list(beta=CalcEnv$beta,delta=CalcEnv$delta,threshparam=CalcEnv$threshparam)
  results$Est.Parameters<-list(beta=whichXest,delta=whichZest,alpha=whichAlphaest)
  #return(list(CalcEnv,results))
  if (robust){
    results$BHHHmatrix<-calcBHHHmatrix(CalcEnv)
  } else {results$BHHHmatrix<-NULL}
  
  class(results)<-"oglmx.fit"
  #results<-list(loglikelihood,link=link,no.iterations=maxLikRes$iterations,coefficients=coefficients,returnCode=maxLikRes$code,gradient=maxLikRes$gradient
  #              ,hessian=maxLikRes$hessian,BHHHhessian=BHHHmatrix,NOutcomes=no.outcomes,Outcomes=listoutcomes,sdmodel=sdmodel,allparams=allparameters,Est.Parameters=Est.Parameters)
  return(results)
  
}

collectmaxLikOutput<-function(x){
  output<-list()
  output$loglikelihood<-x$maximum
  output$coefficients<-x$estimate
  output$gradient<-x$gradient
  output$no.iterations<-x$iterations
  output$returnCode<-x$code
  output$hessian<-x$hessian
  return(output)
}

oglmx.maxlik<-function(inputenv,start){
  inputfunc<-function(par){
    updateComponents(inputenv,par)
    ll<-loglikelihood.oglmx(inputenv)
    score<-score_oglmx(inputenv)
    attr(ll,"gradient")<-score
    if (inputenv$analhessian){
      hessian<-hessian_oglmx(inputenv)
      attr(ll,"hessian")<-hessian
    }
    return(ll)
  }
  output<-maxLik(inputfunc,start=start,iterlim=300,finalHessian=TRUE,method="NR") # ,control=list(printLevel=4)
  return(output)
}
