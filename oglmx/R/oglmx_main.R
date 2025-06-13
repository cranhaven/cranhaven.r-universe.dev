oglmx<-function(formulaMEAN, formulaSD=NULL, data, start=NULL, weights=NULL, link="probit",
                constantMEAN=TRUE, constantSD=TRUE, beta=NULL, delta=NULL, threshparam=NULL,
                analhessian=TRUE, sdmodel=expression(exp(z)), SameModelMEANSD=FALSE, na.action,
                savemodelframe=TRUE, Force=FALSE, robust=FALSE){
  cl<-match.call()
  oglmxoutput<-list()
  fitinput<-list()
  fitinput$analhessian<-analhessian
  fitinput$sdmodel<-sdmodel
  fitinput$robust<-robust
  fitinput$link<-link
  oglmxoutput$link<-link
  oglmxoutput$sdmodel<-sdmodel
  oglmxoutput$call<-cl
 # if (!constantMEAN){formulaMEAN<-update(formulaMEAN,~0+.)}
  
  if (!is.null(formulaSD)){
  #  if (!constantSD){formulaSD<-update(formulaSD,~0+.)}
    cl$formulaMEAN<-mergeformulas(formulaMEAN,formulaSD)
  } else if (SameModelMEANSD){
    formulaSD<-formulaMEAN
  }
  
  
  names(cl)[match("formulaMEAN",names(cl))]<-"formula"
  #return(cl)
  m<-match(c("formula","data","subset","weights","na.action","offset"),names(cl),0L)
  mf<-cl[c(1L,m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf<-eval(mf,parent.frame())
  
  factorvars<-names(attr(attr(mf,"terms"),"dataClasses"))[attr(attr(mf,"terms"),"dataClasses")=="factor"]
  attr(factorvars,"levels")<-lapply(factorvars,function(x){levels(mf[[x]])})
  oglmxoutput$factorvars<-factorvars
  
  mt<- attr(mf,"terms")
  Y<-as.factor(model.response(mf,"numeric"))
  outcomenames<-levels(Y)
  oglmxoutput$Outcomes<-outcomenames
  
  X<-model.matrix(formulaMEAN,mf)
  factorvarsX<-names(attr(X,"contrasts"))
  
  if (!constantMEAN){
    Xint<-match("(Intercept)",colnames(X),nomatch = 0L)
    if (Xint>0L){X<-X[,-Xint,drop=FALSE]}
  }
  #termsMEAN<-terms(formulaMEAN)
  weights<-as.vector(model.weights(mf))
  
  oglmxoutput$NoVarModData<-data.frame(cbind(Y,weights))
  
  No.Outcomes<-nlevels(Y)
  if (No.Outcomes > 20 & !Force){
    stop("More than 20 different values for outcome variable.\n If you are sure you wish to estimate this model rerun command with Force option set to TRUE.")
  }
  
  Y<-as.numeric(Y)
  outcomeMatrix<-1 * ((col(matrix(0, length(Y), No.Outcomes))) == Y)
  colnames(outcomeMatrix)<-outcomenames
  
  oglmxoutput$NOutcomes<-No.Outcomes
  
  if (!is.null(formulaSD)){
    Z<-model.matrix(formulaSD,mf)
    #termsSD<-terms(formulaSD)
    oglmxoutput$Hetero<-TRUE
    if (!constantSD){
      Zint<-match("(Intercept)",colnames(Z),nomatch = 0L)
      if (Zint>0L){Z<-Z[,-Zint,drop=FALSE]}
    }  
  } else {
    Z<-matrix(rep(1,nrow(X)),ncol=1)
    oglmxoutput$Hetero<-FALSE
  }
  
  oglmxoutput$formula<-list(meaneq=formulaMEAN,sdeq=formulaSD)
  
  # beta
  if (!is.null(beta) & length(beta)==1){
    beta<-c(beta,rep(NA,ncol(X)-1))
  } else if (!is.null(beta) & length(beta)>1){
    # check that the specified vector is of correct length
    if (length(beta)!=ncol(X)){stop("Specified beta vector of incorrect length.")}
  } else if (is.null(beta)){
    beta<-rep(NA,ncol(X))
  }
  # delta
  if (!is.null(delta) & length(delta)==1){
    delta<-c(delta,rep(NA,ncol(Z)-1))
  } else if (!is.null(delta) & length(delta)>1){
    # check that the specified vector is of correct length
    if (length(delta)!=ncol(Z)){stop("Specified delta vector of incorrect length.")}
  } else if (is.null(delta)){
    delta<-rep(NA,ncol(Z))
  }
  # threshparam
  if (!is.null(threshparam) & length(threshparam)==1){
    threshparam<-c(threshparam,rep(NA,No.Outcomes-2))
  } else if (!is.null(threshparam) & length(threshparam)>1){
    # check that the specified vector is of correct length
    if (length(threshparam)!=No.Outcomes-1){stop("Specified vector of threshold parameters of incorrect length.")}
  } else if (is.null(threshparam)){
    threshparam<-rep(NA,No.Outcomes-1)
  }
  
  if (savemodelframe){
    oglmxoutput$modelframes<-list(X=X,Z=Z)
  }
  
  if (oglmxoutput$Hetero){
    namesX<-colnames(X)[colnames(X)!="(Intercept)"]
    namesZ<-colnames(Z)[colnames(Z)!="(Intercept)"]
    meanandvarNAME<-namesX[namesX %in% namesZ]
    meanandvarLOC<-match(meanandvarNAME,colnames(X))
    meanandvarLOCZ<-match(meanandvarNAME,colnames(Z))
    oglmxoutput$BothEq<-data.frame(meanandvarNAME,meanandvarLOC,meanandvarLOCZ,stringsAsFactors = FALSE)
  } else {oglmxoutput$BothEq<-NULL}
  
  # collect variable means and check which variables are binary.
  XVarMeans<-apply(X,2,mean)
  XVarBinary<-apply(X,2,.checkbinary)
  
  ZVarMeans<-apply(Z,2,mean)
  ZVarBinary<-apply(Z,2,.checkbinary)
  
  oglmxoutput$varMeans<-list(XVarMeans,ZVarMeans)
  oglmxoutput$varBinary<-list(XVarBinary,ZVarBinary)
  
  FitInput<-append(list(outcomeMatrix=outcomeMatrix,X=X,Z=Z,w=weights,beta=beta,delta=delta,threshparam=threshparam,
                 start=start,optmeth="maxLik"),fitinput)
  #return(FitInput)
  results<-append(oglmxoutput,do.call("oglmx.fit",FitInput))
  attr(results$loglikelihood,"No.Obs")<-length(Y)
  
  class(results)<-"oglmx"
  return(results)
  
  #return(list(Y,X,Z,outcomeMatrix,weights))
}


