

#=================================#
# The overall package description #
#=================================#

#' CureAuxSP: Mixture Cure Models with Auxiliary Subgroup Survival Probabilities.
#'
#' This package provides an information synthesis framework that can Estimate mixture cure models with subgroup survival probabilities
#'    as auxiliary information. The underlying methods are based on the paper titled "Efficient auxiliary information synthesis for
#'    cure rate model", which has been published in Jie Ding, Jialiang Li and Xiaoguang Wang (2024) <doi:10.1093/jrsssc/qlad106>.
#' project work
#' @importFrom methods is
#' @importFrom stats cov model.frame model.matrix model.response optim pchisq pnorm rbinom rexp runif sd uniroot
#' @docType package
#' @name CureAuxSP
NULL




#==========================================================================#
# Semi-parametric mixture cure model with auxiliary information - using CV #
#==========================================================================#

#==== The function for fitting mixture cure model with heterogeneous auxiliary information (t*-year survival probability) ====#
#' @title Semi-parametric mixture cure model with auxiliary subgroup survival information
#'
#' @description Fit the semi-parametric mixture cure model with auxiliary subgroup survival probability information
#'   based on the control variate technique.
#'
#' @aliases SMC.AuxSP
#'
#' @param formula a formula expression, of the form \code{response ~ predictors}.
#'   The \code{response} is a \code{Surv} object (from R package "survival") with right censoring.
#'   It is used to specify the covariate (risk factor) effects on the failure time of uncured subjects.
#'   See the documentation for \code{survreg} and \code{Surv} in R package \code{survival} for details.
#'   The expression to the right of the "~" specifies the effect of covariates on the failure time of uncured patients.
#' @param cureform indicator function a formula expression, of the form \code{cureform ~ predictors}.
#'   It is used to specify the effects of covariates on the cure rate.
#'   Note that a covariate is allowed to be used in both \code{formula} and \code{cureform}.
#' @param sdata a survival dataset (dataframe) in which to interpret the variables named in the \code{formula} and the \code{cureform}.
#' @param aux indicates the historical aggregated statistics. It is a list of lists, and each sub-list represents auxiliary information from the same time point.
#'   We combine multiple time points together and each time point contains the following four elements
#'   \code{tstar} the time point that the auxiliary information was calculated at;
#'   \code{sprob} auxiliary subgroup survival rates for each subgroup at the current time point;
#'   \code{gfunc} a function used to identify the subgroup.
#' @param hetero denotes a logical value. If it is \code{TRUE}, the penalization will be applied to identify the
#'   potential heterogeneous auxiliary subgroup survival rates and make a refinement to the final estimator.
#' @param N records the sample size of the external sdata that we extract auxiliary information from.
#'   The default value is \code{N = Inf}, which is the case that the uncertainty is ignored.
#'   If \code{N} is not \code{Inf}, the method that takes the uncertainty into consideration will be adopted automatically.
#' @param latency specifies the model used in latency part.
#'   It can be \code{PH} which represents the proportional hazards model, or \code{AFT} which represents the accelerated failure time model.
#'   The default is the PH mixture cure model, that is, \code{latency = "PH"}
#' @param nboot specifies the number of bootstrap sampling. The default \code{nboot = 400}.
#'
#' @details
#'    This is a function used to fit the semiparametric mixture cure model with auxilary subgrop survival information.
#'    The method used here is the control variate technique.
#'    The test statistic for evaluating the homogeneity assumption will also be calculated.
#'
#' @return
#'   An object of class \code{SMC.AuxSP} is returned.
#'   Specifically, it contains: \code{model}, a description of the model we fit; \code{suffix}, a character that indicates the status of auxiliary information; \code{coefficients}, estimated parameters.
#'   For more friendly presentation, we provide a function \code{Print.SMC.AuxSP()} that can examine this newly defined class.
#'
#' @examples
#'
#' #--------------------------------------------------------------------#
#' # illustration via simulated dataset (from PH mixture cure model) ####
#' #--------------------------------------------------------------------#
#'
#' ## library
#' library(survival)
#' library(CureAuxSP)
#'
#' ## generate both the internal dataset of interest and the external dataset
#'
#' # - the internal dataset
#' set.seed(1)
#' sdata.internal <- sdata.SMC(n = 300)
#' head(sdata.internal)
#'
#' # - the external dataset
#' set.seed(1)
#' sdata.external <- sdata.SMC(n = 10000)
#'
#' ## prepare the auxiliary information based on the external dataset
#'
#' # - define two functions for subgroup splitting
#' gfunc.t1 <- function(X,Z=NULL){
#'   rbind((X[,1] <  0 & X[,2] == 0), (X[,1] >= 0 & X[,2] == 0),
#'         (X[,1] <  0 & X[,2] == 1), (X[,1] >= 0 & X[,2] == 1))}
#' gfunc.t2 <- function(X,Z=NULL){rbind((X[,2] == 0), (X[,2] == 1))}
#'
#' # - calculate subgroup survival rates
#' sprob.t1 <- Probs.Sub(tstar = 1, sdata = sdata.external,
#'                       G = gfunc.t1(X = sdata.external[,-c(1,2)]))
#' sprob.t2 <- Probs.Sub(tstar = 2, sdata = sdata.external,
#'                       G = gfunc.t2(X = sdata.external[,-c(1,2)]))
#' cat("Information at t* = 1:", sprob.t1, "\nInformation at t* = 2:", sprob.t2)
#'
#' # - prepare the set that collects information about auxiliary data
#' aux <- list(
#'   time1 = list(tstar = 1, gfunc = gfunc.t1, sprob = c(0.73,0.70,0.88,0.83)),
#'   time2 = list(tstar = 2, gfunc = gfunc.t2, sprob = c(0.62,0.76)-0.20)
#' )
#'
#' \donttest{
#' ## fit the model without auxiliary information
#' set.seed(1)
#' sol.PHMC <-  SMC.AuxSP(
#'   formula = Surv(yobs,delta) ~ X1 + X2, cureform = ~ X1,
#'   sdata = sdata.internal, aux = NULL, latency = "PH"
#' )
#' Print.SMC.AuxSP(object = sol.PHMC)
#'
#' ## fit the model with auxiliary information
#'
#' # - ignore heterogeneity
#' set.seed(1)
#' sol.PHMC.Homo <-  SMC.AuxSP(
#'   formula = Surv(yobs,delta) ~ X1 + X2, cureform = ~ X1,
#'   sdata = sdata.internal, aux = aux, hetero = FALSE, latency = "PH"
#' )
#' Print.SMC.AuxSP(object = sol.PHMC.Homo)
#'
#' # - consider heterogeneity
#' set.seed(1)
#' sol.PHMC.Hetero <-  SMC.AuxSP(
#'   formula = Surv(yobs,delta) ~ X1 + X2, cureform = ~ X1,
#'   sdata = sdata.internal, aux = aux, hetero = TRUE, latency = "PH"
#' )
#' Print.SMC.AuxSP(object = sol.PHMC.Hetero)
#' }
#'
#' \donttest{
#' #--------------------------------------------------------------------#
#' # illustration via real breast cancer dataset (from TCGA program) ####
#' #   - the R package "TCGAbiolinks" should be downloaded in advance
#' #   - see "10.18129/B9.bioc.TCGAbiolinks" for more help
#' #--------------------------------------------------------------------#
#'
#' ## library
#' library(survival)
#' library(CureAuxSP)
#'
#' ## prepare the breast cancer dataset
#'
#' # - download clinical data from the TCGA website
#' query <- TCGAbiolinks::GDCquery(project = "TCGA-BRCA",
#'                                 data.category = "Clinical", file.type = "xml")
#' TCGAbiolinks::GDCdownload(query)
#' clinical <- TCGAbiolinks::GDCprepare_clinic(query, clinical.info = "patient")
#'
#' # - a preparation
#' sdata.pre <- data.frame(
#'   yobs   = ifelse(!is.na(clinical[,'days_to_death']),clinical[,'days_to_death'],
#'                   clinical[,'days_to_last_followup'])/365,
#'   delta  = ifelse(!is.na(clinical[,'days_to_death']),1,0),
#'   ER     = ifelse(
#'     clinical[,'breast_carcinoma_estrogen_receptor_status']=='Positive',1,
#'     ifelse(clinical[,'breast_carcinoma_estrogen_receptor_status']=='Negative',0,NA)),
#'   Age    = clinical[,'age_at_initial_pathologic_diagnosis'],
#'   Race   = ifelse(clinical[,'race_list']=='BLACK OR AFRICAN AMERICAN','black',
#'                   ifelse(clinical[,'race_list']=='WHITE','white','other')),
#'   Gender = ifelse(clinical[,'gender']=='FEMALE','Female',
#'                   ifelse(clinical[,'gender']=='MALE','Male',NA)),
#'   Stage  = sapply(
#'     clinical[,'stage_event_pathologic_stage'],
#'     function(x,pattern='Stage X|Stage IV|Stage [I]*'){
#'       ifelse(grepl(pattern,x),regmatches(x,regexpr(pattern,x)),NA)},USE.NAMES = FALSE)
#' )
#'
#' # - extract covariates and remove undesiable subjects and NA
#' sdata.TCGA <- na.omit(
#'   sdata.pre[
#'     sdata.pre[,'yobs'] > 0
#'     & sdata.pre[,'Age'] <= 75
#'     & sdata.pre[,'Gender'] == "Female"
#'     & sdata.pre[,'Race'] %in% c('white')
#'     & sdata.pre[,'Stage'] %in% c('Stage I','Stage II','Stage III'),
#'     c('yobs','delta','Age','ER'),
#'   ]
#' )
#' rownames(sdata.TCGA) <- NULL
#'
#' # - summary statistics of the internal dataset
#' summary(sdata.TCGA)
#'
#' ## plot a figure to show the existence of a cure fraction
#' # pdf("Figure_KM_TCGA_BRCA.pdf",width=8.88,height=6.66); {
#' plot(
#'   survival::survfit(survival::Surv(yobs, delta) ~ 1, data = sdata.TCGA),
#'   conf.int = T, mark.time = TRUE, lwd = 2,
#'   ylab = "Survival Probability", xlab = "Survival Time (in Years)",
#'   xlim = c(0,25), ylim = c(0,1)
#' )
#' # }; dev.off()
#'
#' ## fit the model without auxiliary information
#'
#' # - rescale the Age variable
#' Age.Min <- min(sdata.TCGA$Age); Age.Max <- max(sdata.TCGA$Age)
#' sdata.TCGA$Age <- (sdata.TCGA$Age-Age.Min)/(Age.Max-Age.Min)
#'
#' # - fit the model
#' set.seed(1)
#' sol.PHMC <-  SMC.AuxSP(
#'   formula = Surv(yobs,delta) ~ Age + ER, cureform = ~ Age + ER,
#'   sdata = sdata.TCGA, aux = NULL, latency = "PH"
#' )
#' Print.SMC.AuxSP(object = sol.PHMC)
#'
#' ## fit the model with auxiliary information
#'
#' # - prepare the auxiliary information
#' Age.Cut <- c(0,(c(40,50,60)-Age.Min)/(Age.Max-Age.Min),1)
#' gfunc.t1 <- function(X,Z){
#'   rbind((X[,1] >= Age.Cut[1] & X[,1] <  Age.Cut[2] & X[,2] == 1),
#'         (X[,1] >= Age.Cut[2] & X[,1] <  Age.Cut[3] & X[,2] == 1),
#'         (X[,1] >= Age.Cut[3] & X[,1] <  Age.Cut[4] & X[,2] == 1),
#'         (X[,1] >= Age.Cut[4] & X[,1] <= Age.Cut[5] & X[,2] == 1),
#'         (X[,1] >= Age.Cut[1] & X[,1] <  Age.Cut[2] & X[,2] == 0),
#'         (X[,1] >= Age.Cut[2] & X[,1] <  Age.Cut[3] & X[,2] == 0),
#'         (X[,1] >= Age.Cut[3] & X[,1] <  Age.Cut[4] & X[,2] == 0),
#'         (X[,1] >= Age.Cut[4] & X[,1] <= Age.Cut[5] & X[,2] == 0))}
#' gfunc.t2 <- function(X,Z){rbind((X[,2] == 1), (X[,2] == 0))}
#' aux <- list(
#'   time1 = list(tstar = 5, gfunc = gfunc.t1,
#'                sprob = c(0.810,0.935,0.925,0.950,0.695,0.780,0.830,0.850)),
#'   time2 = list(tstar = 10, gfunc = gfunc.t2, sprob = c(0.825,0.705))
#' )
#'
#' # - ignore heterogeneity
#' set.seed(1)
#' sol.PHMC.Homo <-  SMC.AuxSP(
#'   formula = Surv(yobs,delta) ~ Age + ER, cureform = ~ Age + ER,
#'   sdata = sdata.TCGA, aux = aux, hetero = FALSE, N = 1910, latency = "PH"
#' )
#' Print.SMC.AuxSP(object = sol.PHMC.Homo)
#'
#' # - consider heterogeneity
#' set.seed(1)
#' sol.PHMC.Hetero <-  SMC.AuxSP(
#'   formula = Surv(yobs,delta) ~ Age + ER, cureform = ~ Age + ER,
#'   sdata = sdata.TCGA, aux = aux, hetero = TRUE, N = 1910, latency = "PH"
#' )
#' Print.SMC.AuxSP(object = sol.PHMC.Hetero)
#' }
#'
#' @export SMC.AuxSP
SMC.AuxSP <- function(formula, cureform, sdata, aux = NULL,
                      hetero = FALSE, N = Inf, latency = "PH", nboot = 400
){

  ## basic (whether these inputs are valid enough to make the function works well)
  call <- match.call()
  Y <- model.response(model.frame(formula,sdata))
  if (!inherits(Y, "Surv")){stop("Response must be a survival object")}

  ## prepare - necesary vectors and matrices + extract vars from formulas
  mf.latency <- model.frame(formula,sdata)
  mf.incidence <- model.frame(cureform,sdata)
  X <-  model.matrix(attr(mf.latency, "terms"), mf.latency)[,-1,drop=F]
  Z <-  model.matrix(attr(mf.incidence, "terms"), mf.incidence)[,-1,drop=F]
  vars.name.response <- all.vars(formula)[c(1,2)]
  vars.name.latency <- sapply(colnames(X),function(x){for(xx in c(" ","(",")",":")){x <- sub(xx,"_",x,fixed=T)};x},USE.NAMES=F)
  vars.name.incidence <- sapply(colnames(Z),function(x){for(xx in c(" ","(",")",":")){x <- sub(xx,"_",x,fixed=T)};x},USE.NAMES=F)
  vars.name.covariate <- union(vars.name.latency,vars.name.incidence)
  colnames(X) <- vars.name.latency; colnames(Z) <- vars.name.incidence
  yobs  <- sdata[,vars.name.response[1]]
  delta <- sdata[,vars.name.response[2]]

  ## fit the model under homogeneous assumption
  fit <- SMC.AuxSP.fit(
    yobs=yobs,delta=delta,X=X,Z=Z,aux=aux,hetero=hetero,N=N,
    latency=latency,nboot=nboot
  )
  fit$call <- call
  class(fit) <- c("SMC.AuxSP")

  ## return
  return(fit)

}




#==== The summary function for fitting mixture cure model with auxiliary information ====#
#' @title Print SMC.AuxSP object returned by our main function SMC.AuxSP()
#'
#' @description Output of \code{SMC.AuxSP} object.
#'
#' @aliases Print.SMC.AuxSP
#'
#' @param object an object of SMC.AuxSP
#'
#' @return
#'    This function has no return value and is used to print the results in a \code{SMC.AuxSP} class with a better presentation.
#'
#' @export Print.SMC.AuxSP
Print.SMC.AuxSP <- function(object){

  ## print information for model fitting
  cat(paste(object$model,":\n",sep=""))

  ## print the fitting results for regression coefficients
  cat("\n- Cure Probability Model\n")
  print(object$coefficients$incidence)
  cat("\n- Failure Time Distribution Model\n")
  print(object$coefficients$latency)

  ## print other information for information synthesis estimators
  if(object$suffix != "ori"){

    if(object$suffix == "homo"){
      cat("\n- Test For Evaluating Homogeneity\n")
      print(object$test.chisq)
    }else if(object$suffix == "hetero"){
      if(all(object$tau==0)){
      }else{
        cat("\n- Identified Heterogeneous Auxiliary Information\n")
        print(object$tau[object$tau!=0])
      }
    }

  }

  invisible()

}


#==== The main function for fitting the model ====#
SMC.AuxSP.fit <- function(
    yobs,delta,X,Z,aux,
    hetero=TRUE,N=Inf,
    latency="PH",nboot=400
){

  ## = Specify the dimension
  pX <- ncol(X)   # number of parameters in internal data for latency part
  pZ <- ncol(Z)+1
  p <- pX+pZ # total number of parameters
  bet.names <- colnames(X)
  gam.names <- c('Intercept',colnames(Z))
  n <- length(yobs)   # sample size in internal data
  if(latency=="PH"){
    SMC.fit <- SMC.PH.fit; SMC.AuxSP.EE <- SMC.AuxSP.EE.PH
  }else if(latency=="AFT"){
    SMC.fit <- SMC.AFT.fit; SMC.AuxSP.EE <- SMC.AuxSP.EE.AFT
  }
  suffix <- ifelse(is.null(aux),"ori",ifelse(hetero==FALSE,"homo","hetero"))


  ## = Get estimators with or without auxiliary information
  if(suffix %in% c("homo","hetero")){

    ### Fit original estimators
    smcfit   <- SMC.fit(yobs,delta,X,Z)
    bet.ori  <- smcfit$latency[,1]
    gam.ori  <- smcfit$incidence[,1]
    info.ori <- list()

    ### prepare the auxinfo matrix
    tstar.unique <- sort(unique(sapply(aux,function(timej){timej$tstar})))
    auxinfo <- as.numeric()
    for(itime in 1:length(aux)){ # itime <- 1
      aux.c <- aux[[itime]]
      K.c <- length(aux[[itime]]$sprob)
      auxinfo <- rbind(
        auxinfo,
        cbind(rep(aux.c$tstar,K.c),
              rep(which(tstar.unique==aux.c$tstar),K.c),
              aux.c$sprob,
              aux.c$gfunc(X,Z)*1))
    }
    colnames(auxinfo) <- c('tstar','tstaridx','sprob',paste('ind',1:n,sep="")) # rename this matrix

    ### original control variate related elements
    dif      <- SMC.AuxSP.EE(bet.ori,gam.ori,yobs,delta,X,Z,smcfit$w,auxinfo)
    pai      <- apply(auxinfo[,-c(1:3)],1,mean)

    ### bootstrap procedure for preparation
    bet.all <- array(NA,dim=c(nboot,pX))
    gam.all <- array(NA,dim=c(nboot,pZ))
    dif.all <- sprob.KM.all <- array(NA,dim=c(nboot,nrow(auxinfo)))
    pai.all <- array(NA,dim=c(nboot,nrow(auxinfo)))
    iboot <- 1; nwrong <- 0
    while(iboot <= nboot){
      if(nwrong > (nboot*0.4)){stop("An error!")}
      idx <- sample(1:n,n,replace=TRUE)
      smctry <- try({
        smcfit.iboot <- SMC.fit(yobs[idx],delta[idx],X[idx,,drop=F],Z[idx,,drop=F])
      }, silent = T) # smcfit.iboot$convergence==FALSE
      if( is(smctry, "try-error") == TRUE){nwrong<-nwrong+1;next}
      w.iboot <- smcfit.iboot$w
      bet.iboot <- smcfit.iboot$latency[,1]; gam.iboot <- smcfit.iboot$incidence[,1]
      bet.all[iboot,] <- bet.iboot
      gam.all[iboot,] <- gam.iboot
      sprob.KM.all[iboot,] <- sapply(1:nrow(auxinfo),function(iaux){
        St.Sub.KM(auxinfo[iaux,'tstar'],yobs[idx],delta[idx],auxinfo[iaux,idx+3,drop=F])})
      dif.all[iboot,] <- SMC.AuxSP.EE(bet.iboot,gam.iboot,yobs[idx],delta[idx],X[idx,,drop=F],
                                      Z[idx,,drop=F],w.iboot,auxinfo[,c(1:3,idx+3)])
      pai.all[iboot,] <- apply(auxinfo[,idx+3],1,mean)
      iboot <- iboot + 1
    }
    (bet.ori.se <- apply(bet.all,2,sd))
    (gam.ori.se <- apply(gam.all,2,sd))
    V.KM <- cov(sprob.KM.all)*n
    Sigma.all <- cov(cbind(bet.all,gam.all,dif.all))*n
    dis.all <- pai.all*mvtnorm::rmvnorm(nboot,mean=rep(0,nrow(auxinfo)),sigma=V.KM/N)

    ### obtain matrices: Sigma.all+V.KM+c(bet.ori,gam.ori)+dif+n+auxinfo
    # - get some matrices
    Sigma <- Sigma.all[1:p,1:p]
    Gam <- Sigma.all[(p+1):(p+nrow(auxinfo)),1:p,drop=F]
    V <- Sigma.all[(p+1):(p+nrow(auxinfo)),(p+1):(p+nrow(auxinfo)),drop=F]
    # - reformulate V
    V.new <- V + diag(pai)%*%V.KM%*%diag(pai)*(n/N)
    V.inv <- MASS::ginv(V.new)
    A <- Sigma-t(Gam)%*%V.inv%*%Gam
    A.inv <- solve(A)
    W <- rbind(cbind(A.inv,-A.inv%*%t(Gam)%*%V.inv), # W0 <- solve(Sigma.all)
               cbind(-V.inv%*%Gam%*%A.inv,V.inv+V.inv%*%Gam%*%A.inv%*%t(Gam)%*%V.inv))
    W.inv <- MASS::ginv(W) # Sigma.all (a new version)

    ### do test: Chi-squard tests
    test.chisq.value <- as.vector(n*t(dif)%*%V.inv%*%dif)
    test.chisq.dfreedom <- nrow(auxinfo)
    test.chisq.pvalue <- 1-pchisq(test.chisq.value,test.chisq.dfreedom) # smaller is worse
    test.chisq <- c(value= test.chisq.value,df=test.chisq.dfreedom,pvalue=test.chisq.pvalue)

    ### estimators with auxiliary information - homogeneous
    if(suffix == "homo"){
      the.homo    <- as.vector(c(bet.ori,gam.ori)-t(V.inv%*%Gam)%*%dif);the.homo
      the.homo.se <- sqrt( diag((A)/n) ); the.homo.se
      bet.homo    <- the.homo[1:pX]
      gam.homo    <- the.homo[-c(1:pX)]
      bet.homo.se <- the.homo.se[1:pX]
      gam.homo.se <- the.homo.se[-c(1:pX)]
      info.homo   <- list(test.chisq=test.chisq)
    }


    ### estimators with auxiliary information - heterogeneous
    if(suffix == "hetero"){

      # - transform necessary matrices: A+Gam+V.inv+auxinfo+nu
      V.inv.SVD <- svd(V.inv)
      V.inv.root <- V.inv.SVD$u%*%diag(sqrt(V.inv.SVD$d))%*%t(V.inv.SVD$v)

      # - solve adaptive lasso using R package "lars"
      sol.penfit <-   SMC.AuxSP.Quad.Pen(bet=bet.ori,gam=gam.ori,dif=dif,pai=pai,V.inv.root=V.inv.root,
                                         Gam=Gam,V.inv=V.inv,auxinfo=auxinfo)

      # - obtain final estimates
      tau.hetero <- sol.penfit$tau; tau.hetero
      names(tau.hetero) <- paste("G",do.call(c,lapply(1:length(aux),function(itime){paste(itime,1:length(aux[[itime]]$sprob),sep="")})),sep="")
      the.hetero <-  sol.penfit$the; the.hetero
      tau.path <- sol.penfit$tau.path

      # - bootstrap type estimator of variance
      the.hetero.all <- array(NA,dim=c(nboot,pX+pZ))
      tau.hetero.all <- array(NA,dim=c(nboot,nrow(auxinfo)))
      tau.path.all <- rep(list(list()),nboot)
      for(iboot in 1:nboot){
        sol.penfit.iboot <- SMC.AuxSP.Quad.Pen(
          bet=bet.all[iboot,],gam=gam.all[iboot,],dif=dif.all[iboot,]-dis.all[iboot,],pai=pai.all[iboot,],
          V.inv.root=V.inv.root,Gam=Gam,V.inv=V.inv,auxinfo=auxinfo)
        tau.hetero.all[iboot,] <- sol.penfit.iboot$tau
        the.hetero.all[iboot,] <- sol.penfit.iboot$the
        tau.path.all[[iboot]]      <- sol.penfit.iboot$tau.path
      }
      (tau.hetero.se <- apply(tau.hetero.all,2,sd))
      (the.hetero.se <- apply(the.hetero.all,2,sd))
      bet.hetero <- the.hetero[1:pX]; gam.hetero <- the.hetero[-c(1:pX)]
      bet.hetero.se <- the.hetero.se[1:pX]; gam.hetero.se <- the.hetero.se[-c(1:pX)]
      info.hetero <- list(tau=tau.hetero,test.chisq=test.chisq)

    }

  }else{

    ### Fit original estimators
    smcfit   <- SMC.fit(yobs,delta,X,Z)
    bet.ori  <- smcfit$latency[,1]
    gam.ori  <- smcfit$incidence[,1]
    info.ori <- list()

    ### bootstrap procedure for preparation
    bet.all <- array(NA,dim=c(nboot,pX))
    gam.all <- array(NA,dim=c(nboot,pZ))
    iboot <- 1; nwrong <- 0
    while(iboot <= nboot){
      if(nwrong > (nboot*0.4)){stop("An error!")}
      idx <- sample(1:n,n,replace=TRUE)
      smctry <- try({
        smcfit.iboot <- SMC.fit(yobs[idx],delta[idx],X[idx,,drop=F],Z[idx,,drop=F])
      }, silent = T) # smcfit.iboot$convergence==FALSE
      if( is(smctry, "try-error") == TRUE){nwrong<-nwrong+1;next}
      w.iboot <- smcfit.iboot$w
      bet.iboot <- smcfit.iboot$latency[,1]; gam.iboot <- smcfit.iboot$incidence[,1]
      bet.all[iboot,] <- bet.iboot
      gam.all[iboot,] <- gam.iboot
      iboot <- iboot + 1
    }
    (bet.ori.se <- apply(bet.all,2,sd))
    (gam.ori.se <- apply(gam.all,2,sd))

  }


  ## = do inference and combine results into pre-specified style
  # - for latency part
  bet.c <- get(paste(c("bet"),suffix,sep="."))
  bet.c.se <- get(paste(c("bet"),suffix,"se",sep="."))
  zvalue.bet.c <- bet.c/bet.c.se
  pvalue.bet.c <- 2*(1-pnorm(abs(zvalue.bet.c)))
  # - for incidence part
  gam.c <- get(paste(c("gam"),suffix,sep="."))
  gam.c.se <- get(paste(c("gam"),suffix,"se",sep="."))
  zvalue.gam.c <- gam.c/gam.c.se
  pvalue.gam.c <- 2*(1-pnorm(abs(zvalue.gam.c)))
  # - combine inference results
  latency.c <- data.frame(Est=bet.c,SE=bet.c.se,zvalue=zvalue.bet.c,pvalue=pvalue.bet.c,row.names=bet.names)
  incidence.c <- data.frame(Est=gam.c,SE=gam.c.se,zvalue=zvalue.gam.c,pvalue=pvalue.gam.c,row.names=gam.names)
  # - combine all results
  info.c <- get(paste(c("info"),suffix,sep="."))
  # - other refinements
  out <- c(list(
    model=paste(latency," Mixture Cure Model ",
                ifelse(suffix=="ori","without",ifelse(suffix=="homo","with Homogeneous","with Heterogeneous")),
                " Auxiliary Information",sep=""),
    suffix=suffix,
    coefficients=list(latency=latency.c,incidence=incidence.c)
  ),info.c)

  ## = extract output values
  return(out)

}



#==== Auxiliary information's Estimating Equations at different time points (for ph) ====#
SMC.AuxSP.EE.PH <- function(bet,gam,yobs,delta,X,Z,w,auxinfo){

  # the estimating equations in individual levels
  Stx <- SMC.PH.Stx(yobs,delta,X,Z,bet,gam,w,cross=TRUE,tm=auxinfo[,"tstar"])$Stx
  Psi <- (t(Stx)-auxinfo[,'sprob'])*auxinfo[,-c(1:3)]

  # output
  return(apply(Psi,1,mean))
}


#==== Auxiliary information's Estimating Equations at different time points (for aft) ====#
SMC.AuxSP.EE.AFT <- function(bet,gam,yobs,delta,X,Z,w,auxinfo){
  # Auxiliary Information's Estimating Equations at different time points

  # the estimating equations in individual levels
  Stx <- SMC.AFT.Stx(yobs,delta,X,Z,bet,gam,w,cross=TRUE,tm=auxinfo[,"tstar"])$Stx
  Psi <- (t(Stx)-auxinfo[,'sprob'])*auxinfo[,-c(1:3)]

  # output
  return(apply(Psi,1,mean))
}


#==== Sub-function: for fitting penaltized estimator ====#
SMC.AuxSP.Quad.Pen <- function(bet,gam,dif,pai,V.inv.root,Gam,V.inv,auxinfo){

  n <- ncol(auxinfo[,-c(1:3)])
  y.tilde <- as.vector( V.inv.root%*%dif )
  X.tilde <- V.inv.root%*%diag(pai)

  # solve adaptive lasso using lars
  w <- (1/abs(dif))*(pai)
  X.tilde.star <- t(t(X.tilde)/w)
  sol.lars <- lars::lars(X.tilde.star,y.tilde,trace=FALSE,normalize=FALSE,intercept=FALSE)
  tau.path <- t(as.matrix(sol.lars$beta))/w # each
  tau.path.RSS <- apply(X.tilde %*% tau.path - y.tilde,2,function(x){sum(x^2)})
  tau.path.Card <- apply(tau.path,2,function(x){sum(x!=0)})
  IC.all <- as.numeric( tau.path.RSS + log(n)/n * tau.path.Card ) # BIC type criterion
  min_IC.idx <- which.min( IC.all  )
  # output: return final estimates
  tau <- tau.path[,min_IC.idx]
  return(list(
    the=as.vector(c(bet,gam)-t(V.inv%*%Gam)%*%(dif-pai*tau)),
    tau=tau,
    tau.path=tau.path
  ))

}




#==========================================================================#
# Calculate Subgroup survival rates using KM
#==========================================================================#

#==== for calculating subgroup survival rates ====#
#' @title Calculate subgroup survival probabilities
#'
#' @description Calculate Subgroup survival probabilities basedon the Kaplan-Meier estimation procedure
#'
#' @aliases Probs.Sub
#'
#' @param tstar time points that the survival probabilities will be estimated at.
#' @param sdata a survival dataset (dataframe) in which to interpret the variables named in the \code{formula} and the \code{cureform}.
#' @param G a matrix used to indicate which subgroups he/she belongs to for each of these subjects.
#'
#' @return
#'    It returns the estimated subgroup survival probabilities for a given survival dataset.
#'
#' @export Probs.Sub
Probs.Sub <- function(tstar,sdata,G){

  tSP <- St.Sub.KM(
    tstar = tstar, yobs = sdata$yobs, delta = sdata$delta, G = G
  )

  return( round(tSP,2) )
}


#==== for calculating subgroup survival rates ====#
St.Sub.KM <- function(tstar,yobs,delta,G){

  K <- nrow(G)
  tSP <- rep(0, K)
  for(k in 1:K){
    idx <- (G[k,]==1)
    fit.k <- summary(survival::survfit(survival::Surv(yobs, delta) ~ 1, subset=idx))
    tSP[k] <- min( c(1,fit.k$surv)[ c(0,fit.k$time) <= tstar ] )
  }
  return( tSP )
}




#==========================================================================#
# Semi-parametric cox mixture cure model  -- similar to that in smcure ####
#==========================================================================#



#==== The main function for fitting model ====#
SMC.PH.fit <- function(yobs,delta,X,Z,incidence=c("logit"),Var=FALSE,nboot=100,
                       em.maxit=100,em.tol=1e-5){

  ### Specify the dimension and prepare data
  n <- length(yobs) # number of individuals
  pX <- ncol(X)
  pZ <- ncol(Z)+1
  ZI <- as.matrix(cbind(rep(1,n),Z)) # [dataframe: for incidence part]

  ### do EM algorithm
  # obtain initial bet, gam and baseline nonparametric part
  bet.old <- survival::coxph(survival::Surv(yobs,delta)~X,subset=(delta==1),method="breslow")$coef
  gam.old <- eval(parse(text=paste("glm", "(", "delta~Z",",family = quasibinomial(link='",incidence,"'",")",")",sep = "")))$coef
  Sutx.old <- SMC.PH.Sutx(yobs,delta,X,bet.old,delta,cross=FALSE,tm=NULL)$Sutx
  numit <- 1
  repeat{

    # calculate w first
    expZgam <- exp(ZI%*%gam.old)
    uncureprob <- expZgam/(1+expZgam)
    w <- as.vector(delta+(1-delta)*(uncureprob*Sutx.old)/(1-uncureprob+uncureprob*Sutx.old))

    # update gam
    gam <- eval(parse(text=paste("glm","(","w~Z",",family = quasibinomial(link='",incidence,"'", ")",")", sep = "")))$coef
    # update bet
    bet <- survival::coxph(survival::Surv(yobs,delta)~X+offset(log(w)),subset=(w!=0),method="breslow")$coef
    # bet <- coxph(Surv(yobs[w>0],delta[w>0])~X[w>0,,drop=F],weights=w[w>0],method="breslow")$coef
    # update Sutx
    Sutx <- SMC.PH.Sutx(yobs,delta,X,bet,w,cross=FALSE,tm=NULL)$Sutx

    ### update or stop
    convergence.value <- max(c(abs(bet.old-bet)/abs(bet),abs(gam.old-gam)/abs(gam)))
    if(convergence.value >= em.tol & numit < em.maxit){
      bet.old <- bet; gam.old <- gam
      Sutx.old <- Sutx
      numit <- numit + 1
    }else{
      break
    }

  } # end em reps
  convergence <- (numit<em.maxit & convergence.value < em.tol)

  ### extract output values
  out <- list(
    latency = data.frame(Est=bet,row.names=colnames(X)),
    incidence = data.frame(Est=gam,row.names=c('Intercept',colnames(Z))),
    convergence=convergence,numit=numit,
    w=w
  )
  return(out)

}


#==== Survival function for uncured patients (with PH latency) ====#
SMC.PH.Sut <- function(yobs,delta,X,bet,w,tm=NULL){ # for uncured

  # preparation: calculate hazards
  expXbet <- as.vector(exp(X%*%bet))
  sumRisk <- sapply(yobs,function(yobsi){sum((yobs>=yobsi)*w*expXbet)})
  ht <- ifelse(delta==1,delta/sumRisk,0)

  # calculate hazards (cumulative)
  Ht <- sapply(tm,function(tmi){sum((yobs<=tmi)*ht)})
  Ht[tm>max(yobs[delta==1])] <- Inf; Ht[tm<min(yobs[delta==1])] <- 0

  # calculate Survivals
  Sut <- exp(-Ht)

  # output
  return(Sut)

}



#==== Survival function for uncured patients (with PH latency) ====#
SMC.PH.Sutx <- function(yobs,delta,X,bet,w,cross=FALSE,tm=NULL){ # for uncured

  # preparation: calculate hazards
  expXbet <- as.vector(exp(X%*%bet))
  sumRisk <- sapply(yobs,function(yobsi){sum((yobs>=yobsi)*w*expXbet)})
  ht <- ifelse(delta==1,delta/sumRisk,0)
  # calculate
  if(cross){
    # calculate hazards
    Ht <- sapply(tm,function(tmi){sum((yobs<=tmi)*ht)})
    Ht[tm>max(yobs[delta==1])] <- Inf; Ht[tm<min(yobs[delta==1])] <- 0
    # calculate Survivals
    St.baseline <- exp(-Ht)
    Sutx <- outer(expXbet,St.baseline,function(x,y){y^x})
  }else{
    # calculate hazards
    Ht <- sapply(yobs,function(yobsi){sum((yobs<=yobsi)*ht)})
    Ht[yobs>max(yobs[delta==1])] <- Inf; Ht[yobs<min(yobs[delta==1])] <- 0
    # calculate Survivals
    St.baseline <- exp(-Ht)
    Sutx <- St.baseline^expXbet
  }
  # output
  list(Sutx = Sutx)

}



#==== Survival function for the whole population (with PH latency) ====#
SMC.PH.Stx <- function(yobs,delta,X,Z,bet,gam,w,cross=FALSE,tm=NULL){ # for all

  # calculate uncureprob
  expZgam <- exp(cbind(1,Z)%*%gam)
  uncureprob <- as.vector(expZgam/(1+expZgam))
  Sutx <- SMC.PH.Sutx(yobs,delta,X,bet,w,cross,tm)$Sutx
  Stx <- Sutx*uncureprob+1-uncureprob
  # output
  return(list(Stx=Stx))

}





#==========================================================================#
# Semi-parametric aft mixture cure model  -- similar to that in  smcure ####
#==========================================================================#

#==== The main function for fitting model ====#
SMC.AFT.fit <- function(yobs,delta,X,Z,incidence=c("logit"),intercept=FALSE,
                        Var=FALSE,nboot=100,em.maxit=100,em.tol=1e-5){

  ### Specify the dimension and prepare data
  n <- length(yobs) # number of individuals
  pX <- ifelse(intercept==FALSE,ncol(X),ncol(X)+1)
  pZ <- ncol(Z)+1
  XI <- as.matrix(cbind(rep(1,n),X))
  ZI <- as.matrix(cbind(rep(1,n),Z)) # [dataframe: for incidence part]

  ### do EM algorithm
  # obtain initial bet, gam and baseline nonparametric part
  bet.old <- survival::survreg(survival::Surv(yobs,delta)~X)$coef # from survival package
  if(intercept==FALSE){bet.old <- bet.old[-1]}
  gam.old <- eval(parse(text=paste("glm", "(", "delta~Z",",family = quasibinomial(link='",incidence,"'",")",")",sep = "")))$coef
  Sutx.old <- SMC.AFT.Sutx(yobs,delta,X,bet.old,delta,cross=FALSE,tm=NULL,intercept)$Sutx
  numit <- 1
  repeat{

    # calculate w first
    expZgam <- exp(ZI%*%gam.old)
    uncureprob <- expZgam/(1+expZgam)
    w <- as.vector(delta+(1-delta)*(uncureprob*Sutx.old)/(1-uncureprob+uncureprob*Sutx.old))

    # update gam
    gam <- eval(parse(text=paste("glm","(","w~Z",",family = quasibinomial(link='",incidence,"'", ")",")", sep = "")))$coef
    # update bet
    bet <- optim(par=rep(0,pX),fn=SMC.AFT.rank,method="Nelder-Mead",
                 control=list(maxit=500,reltol=1e-04),
                 yobs=yobs,delta=delta,X=X,w=w,intercept=intercept)$par
    # update Sutx
    Sutx <- SMC.AFT.Sutx(yobs,delta,X,bet,w,cross=FALSE,tm=NULL,intercept)$Sutx

    ### update or stop
    # convergence.value <- max(max(abs(c(gam-gam.old,bet-bet.old))),max(abs(c(Sutx-Sutx.old))))
    convergence.value <- max(c(abs(bet.old-bet)/abs(bet),abs(gam.old-gam)/abs(gam)))
    if(convergence.value >= em.tol & numit < em.maxit){
      bet.old <- bet; gam.old <- gam
      Sutx.old <- Sutx
      numit <- numit + 1
    }else{
      break
    }

  } # end em reps
  convergence <- (numit<em.maxit & convergence.value < em.tol)

  ### extract values
  names.latency <- c('Intercept',colnames(X))
  if(intercept==FALSE){names.latency <- names.latency[-1]}
  names.incidence <- c('Intercept',colnames(Z))
  out <- list(
    latency = data.frame(Est=bet,row.names=names.latency),
    incidence = data.frame(Est=gam,row.names=names.incidence),
    convergence = convergence,numit=numit,
    w=w
  )
  return(out)

}

#==== Survival function for uncured patients (with AFT latency) ====#
SMC.AFT.Sutx <- function(yobs,delta,X,bet,w,cross=FALSE,tm=NULL,intercept=FALSE){ # for uncured

  # prepare error
  if(intercept==FALSE){Xbet<-as.vector(X%*%bet)}else{Xbet<-as.vector(cbind(1,X)%*%bet)}
  error <- log(yobs)-Xbet
  # preparation: calculate hazards
  sumRisk <- sapply(error,function(errori){sum((error>=errori)*w)})
  ht <- ifelse(delta==1,delta/sumRisk,0)
  # calculate
  if(cross){
    # prepare errors in tm cross
    error2 <- outer(Xbet,log(tm),function(x,y){y-x})
    # calculate hazards
    Ht <- sapply(error2,function(errori){sum((error<=errori)*ht)})
    Ht[error>max(error[delta==1])] <- Inf; Ht[error<min(error[delta==1])] <- 0
    Ht <- matrix(Ht,ncol=length(tm))
    # calculate Survivals
    Sutx <- exp(-Ht)
  }else{
    # calculate hazards
    Ht <- sapply(error,function(errori){sum((error<=errori)*ht)})
    Ht[error>max(error[delta==1])] <- Inf; Ht[error<min(error[delta==1])] <- 0
    # calculate Survivals
    Sutx <- exp(-Ht)
  }
  # output
  list(Sutx = Sutx)

}


#==== Survival function for the whole population (with AFT latency) ====#
SMC.AFT.Stx <- function(yobs,delta,X,Z,bet,gam,w,cross=FALSE,tm=NULL,intercept=FALSE){ # for all

  # calculate uncureprob
  expZgam <- exp(cbind(1,Z)%*%gam)
  uncureprob <- as.vector(expZgam/(1+expZgam))
  Sutx <- SMC.AFT.Sutx(yobs,delta,X,bet,w,cross,tm,intercept)$Sutx
  Stx <- Sutx*uncureprob+1-uncureprob
  # output
  return(list(Stx=Stx))

}

#==== The rank function (with AFT latency) ====#
SMC.AFT.rank <- function(bet,yobs,delta,X,w,intercept=FALSE){

  if(intercept==FALSE){
    error <- as.vector(log(yobs)-X%*%bet)
  }else{
    error <- as.vector(log(yobs)-cbind(1,X)%*%bet)
  }
  LossGehan <- mean(sapply(1:length(yobs),function(i){sum((error[i]<error)*abs(error-error[i])*w*delta[i])}))
  return(LossGehan)

}



#=============================#
# data generating function ####
#=============================#

#==== The main function for fitting model ====#
#' @title Generate simulated dataset from a well-designed PH mixture cure model
#'
#' @description Generate simulated dataset from a well-designed PH mixture cure model.
#'
#' @aliases sdata.SMC
#'
#' @param n the sample size of the simulated dataset.
#' @param trace a logical value that indicates whether the information about cure rate and censoring rate should be printed.
#'
#' @return
#'    It returns the simulated dataset from a well-designed PH mixture cure model.
#'
#' @export sdata.SMC
sdata.SMC <-  function(n,trace=FALSE){

  # Generate Data from PH Mixture Cure Model: S(t|x)=1-p(x)+p(x)Su(t|x)
  # Number of Covariates: 2
  # cure rate = 50%, censoring rate = 60%

  ### some basic settings
  bet <- c(1,-1) # for latency
  gam <- c(0,-1) # for incidence
  tau <- 8.1

  ### generate covariates
  X <- array(NA, dim=c(n,length(bet)))
  Z <- array(NA, dim=c(n,length(gam)-1))
  X[,1] <- runif(n,-1,1)
  X[,2] <- rbinom(n,1,0.5)
  Z[,1] <- X[,1]

  ### Su: generate failure times for uncured patients
  Futx <- function(t,x,bet,tau){ # survival function with truncation
    if(t>=tau){FF<-1}else{
      psi <- exp(sum(x*bet))
      St0 <- (exp(-t)-exp(-tau))/(1-exp(-tau))
      FF <- 1-St0^psi
    }
    return(FF)
  }
  get.stime <- function(t,u,x,bet,tau){ u - Futx(t,x,bet,tau) }
  stime <- rep(NA, n)
  for(i in 1:n){
    stime.c <- uniroot(get.stime,c(0,tau),u=runif(1,0,1),x=X[i,],bet=bet,tau=tau)$root
    stime[i] <- stime.c
  }

  ## incidence part (and indicators for cured group), then change some stime to Inf
  logit.state <- cbind(1,Z)%*%gam
  p.uncure <- exp(logit.state)/(1+exp(logit.state))
  cure.state <- rbinom(n, 1, p.uncure) # uncured(y=1) or not
  cure.rate <- 1-mean(cure.state)
  stime[cure.state==0] <- Inf

  ## generate censoring time
  ctime <- rexp(n, 1/8)
  delta <- as.numeric(stime<=ctime)
  censoring.rate <- 1-mean(delta)

  ## delta and observed failure time
  yobs <- pmin(stime,ctime)

  # output
  info <- c(cure.rate=cure.rate,censoring.rate=censoring.rate)
  sdata <- data.frame(yobs,delta,data.frame(X))
  if(trace==TRUE){print(info)}
  return(sdata)

}





