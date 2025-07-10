#' Rank the time series (ARMA) models based on the confidence envelope for minimum ZIC
#'
#' @param x a vector of time series data (should be included with the maximum of 1000 data points).
#' @param max.p maximum value for AR coefficient.
#' @param max.q maximum value for MA coefficient.
#' @param alphaval confidence limit \eqn{(1-\alpha)} (Default is 0.95).
#' @param model_ZIC type of the information criterion, it can be "AIC", "BIC", or "AICc" (Default is the "AIC").
#'
#' @return a list of ranked models which lies in the confidence envelope, \eqn{CE(\alpha).}
#' \item{Ranked_Models}{A set of top ranked time series models which lie in the confidence envelope \eqn{CE(\alpha)} (with AR and MA coefficients, ZIC values ("AIC", "BIC", or "AICc")).}
#'
#' \item{Confidence_Envelope}{gives the confidence envelope \eqn{CE(\alpha)} for the minimum ZIC.}
#'
#' \item{Confidence_Limit}{the confidence limit, \eqn{1-\alpha}.}
#'
#' \item{Total_Models}{number of total fitted models.}
#'
#' @export
#'
#' @description Narrow down the number of models to look at in model selection using the confidence envelope based on the minimum ZIC values for time series data. Here, we compute the ZIC values ("AIC", "BIC", or "AICc") for time-series data, confidence envelope for the minimum ZIC values for the given confidence limit, and rank the top models which lie in the confidence envelope.
#'
#' @details This program involves the computation of multivariate normal-probabilities with covariance matrices based on minimum ZIC inverting the CDF of the minimum ZIC. It involves both the computation of singular and non-singular probabilities. The methodology is described in Genz (1992).
#' @details Let \eqn{X_j} be the ZIC value for the \eqn{j^{th}} fitted model. Compute the cdf values of the minimum ZIC, \eqn{F_{X_{(1)}}(\cdot)} numerically and then obtain the \eqn{100\cdot (1-\alpha)\%} confidence envelope:
#' \deqn{CE(\alpha)=F^{-1}_{X_{(1)}}(1-\alpha)}
#' @details See details:
#' @details Jayaweera I.M.L.N, Trindade A.A., ``How Certain are You in Your Minimum AIC and BIC Values?", Sankhya A (2023+)
#'
#' @usage RankTS(x,max.p,max.q,alphaval=0.95,model_ZIC="AIC")
#'
#' @importFrom stats arima
#' @importFrom stats toeplitz
#' @importFrom stats logLik
#' @importFrom ltsa tacvfARMA
#' @importFrom ltsa TrenchInverse
#' @importFrom tidytable crossing
#' @importFrom psych tr
#'
#' @references Genz, A. (1992). Numerical computation of multivariate normal probabilities. Journal of computational and graphical statistics, 1(2), 141-149.
#'
#' @examples
#' \donttest{
#' library("ConfZIC")
#' data(Sunspots)
#' x=Sunspots
#' RankTS(x,max.p=13,max.q=13,0.95,"AICc")
#' }



RankTS=function(x,max.p,max.q,alphaval=0.95,model_ZIC="AIC"){

  ###################################

  if (alphaval<0 || alphaval>1) {stop("Confidence limit should be between 0 and 1!")}
  if (length(x)>=1000){stop("No of data points are out of range: need to less than 1000")}
  if (model_ZIC!= "AIC" && model_ZIC!= "BIC" && model_ZIC!= "AICc"){stop("model_ZIC should be either AIC,BIC, or AICc")}


  #####################
  ExactTS<-function(AIC_v,sigmahat,alphaval,m)
  {
    FX=NULL
    num=m
    alphaval=alphaval
    #  S=Sort(AIC_v)
    FX=function(x){
      a=x
      pmvnorm(lower = rep(a,num),
              upper = rep(Inf,num),
              mean = AIC_v[1:num],
              sigma = sigmahat[1:num,1:num],maxpts = 25000, abseps = 0.001,
              releps = 0)
    }



    CDFfinal=function(x){
      ans=1-FX(x)-alphaval
      return(ans)
    }
    ######################################################################


    uval=bisection(f=CDFfinal,a=-10,b=max(GIC)+100, tol = 0.1,m=100)
    percentile15=c(uval)
    #percentile15=u8
    return(percentile15)
  }





  #####################################
  pval=max.p
  qval=max.q

  alphaval=alphaval
  x=x
  n=length(x)
  tot=(pval+1)*(qval+1)
  num=tot
  A<-vector(mode = "list", length = tot)
  B <- vector(mode = "list", length = 6)

  for (u in 1:tot){
    A[[u]]<-B
  }

  u=1
  GICVal=rep(0,tot)
  #GICVal2=rep(0,tot)
  for (p in 0:pval){
    for (q in 0:qval){
      suppressWarnings({fit=try(arima(x,order=c(p,0,q),method="ML",include.mean=FALSE),silent = TRUE)
      })
      if(inherits(fit, "try-error")){
        #gval<-try(tacvfARMA(phi,-theta,n-1))
        Cov_gval<-matrix(rep(0,n*n),nrow=n,ncol=n)
        #CovInv_gval=TrenchInverse(Cov_gval)
        GICaic=0
        GIC_AIC=0
        GIC_BIC=0
        GIC_AICc=0
        # next
      }else {


        phival=fit$model$phi
        thetaval=fit$model$theta


        if (length(phival)==0) {phi=0} else { phi=phival}
        if (length(thetaval)==0) {theta=0} else {theta=thetaval}


        gval<-try(tacvfARMA(phi,-theta,n-1))
        Cov_gval<-toeplitz(gval[1:n])
        CovInv_gval=TrenchInverse(Cov_gval)
        GICaic=fit$aic

        #GICn=(-2/n) * fit$loglik + 2* (p+q+1)/n
        pt=length(fit$coef)
        GIC_AICc=(-2/n) * fit$loglik +pt/(n-pt-1)
        GIC_AIC=(-2/n) * fit$loglik +pt/n
        GIC_BIC=(-2/n) * fit$loglik +pt*log(n)/(2*n)
        #AICc=fit$aic+ 2*pt*(pt+1)/(n-pt-1)
        #GICnew=(1/n)*t(x)%*%CovInv_gval%*%x+ 2 * (p+q+1)/n
        #GICnew2=(2/n)*t(x)%*%CovInv_gval%*%x+ 2 * (p+q+1)/n

      }

      # if (class(fit)=="try-error")  fit=try(arima(x,order=c(p,0,q),method="CSS-ML",include.mean=FALSE),silent = TRUE)

      # p=1; q=5
      #if (p==13 && q==5) fit=try(arima(x,order=c(p,0,q)),silent = TRUE)
      #if (p==14 && q==9) fit=try(arima(x,order=c(p,0,q),include.mean=FALSE),silent = TRUE)

      A[[u]][[1]]<-gval
      A[[u]][[2]]<- Cov_gval
      A[[u]][[3]]<-CovInv_gval
      A[[u]][[4]]<-GIC_AIC
      A[[u]][[5]]<-GIC_BIC
      A[[u]][[6]]<-GIC_AICc

      u=u+1

    }
  }




  GICVal=rep(0,tot)



  if (model_ZIC=="AIC"){

    for (u in 1:tot){
      GICVal[u]=A[[u]][[4]]*n

    }


  }

  if (model_ZIC=="BIC"){
    for (u in 1:tot){
      GICVal[u]=A[[u]][[5]]*n

    }
  }

  if (model_ZIC=="AICc"){
    for (u in 1:tot){
      GICVal[u]=A[[u]][[6]]*n

    }
  }

  A_original=A;

  ARcoef=rep(0,tot)
  MAcoef=rep(0,tot)
  Index=as.matrix(crossing(var1 = 0:p, var2 = 0:q))
  for (u in 1: tot){
    ARcoef=Index[,1]
    MAcoef=Index[,2]
  }
  #AA=cbind(S,R,ARcoef,MAcoef)

  OUT=cbind(GICVal,ARcoef,MAcoef)
  OUT1=cbind(GICVal,ARcoef,MAcoef)

  if (model_ZIC=="AIC"){ colnames(OUT)=c("AIC","ARcoef","MAcoef"); colnames(OUT1)=c("AIC","ARcoef","MAcoef")}
  if (model_ZIC=="BIC"){ colnames(OUT)=c("BIC","ARcoef","MAcoef"); colnames(OUT1)=c("BIC","ARcoef","MAcoef")}
  if (model_ZIC=="AICc"){colnames(OUT)=c("AICc","ARcoef","MAcoef"); colnames(OUT1)=c("AICc","ARcoef","MAcoef")}


  rval=which(OUT1[,1]==0)

  if (length(rval)==0) {OUT=OUT; Aval=A}   else {

    OUT=OUT1[which(OUT1[,1]!=0),]
    Aval=A[-rval]
  }

  ntot=nrow(OUT)
  GICvalue=OUT[,1]
  AICV=GICvalue
  minGIC=min(AICV)



  for (u in 1:ntot){
    if (minGIC==GICvalue[u]) Sigma_f=A[[u]][[2]]
  }


  sigma_v=matrix(rep(0,ntot^2), nrow=ntot,ncol=ntot)

  # calculate the covariance matrix
  for (i in 1:ntot){
    for (j in 1:ntot){
      sigma_v[i,j]=(2/n^2)*tr(Sigma_f%*%A[[i]][[3]]%*%Sigma_f%*%A[[j]][[3]])
    }
  }





  nrow(sigma_v)
  ##################################################### loop
  GIC=AICV
  S=sort(GIC)
  R=order(GIC)
  num=ntot
  AIC_v=GIC

  # Calculate the quantile value

  pern=ExactTS(GIC,sigma_v,alphaval,num)

  result=OUT[order(OUT[,1],decreasing=FALSE),]
  result_final=result[result[,1]<pern,]

  listdata=list(Ranked_Models=result_final, Confidence_Envelope=pern, Confidence_Limit=alphaval, Total_Models=tot)

  return(listdata)

}
