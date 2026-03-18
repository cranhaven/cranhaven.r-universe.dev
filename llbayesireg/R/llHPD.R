#' @title Highest Posterior Density for the L-Logistic Bayesian Regression
#' @name llHPD
#' @description Compute the highest posterior density for the L-Logistic Bayesian Regression intervals of betas and deltas.
#' @param fitll Object of class matrix with the llbayesireg function result.
#' @param prob A number of quantiles of interest. The default is 0.95.
#' @param chain Chain chosen for construction. The default is 1.
#'
#' @return llHPD(fitll, prob = 0.95, chain = 1) give the highest posterior density intervals of betas and deltas.
#'
#' @source The L-Losgistic distribution was introduced by Tadikamalla and Johnson (1982), which refer to this distribution as Logit-Logistic
#' distribution. Here, we have a new parameterization of the Logit-Logistic with the median as a parameter.
#'
#' @references Paz, R.F., Balakrishnan, N and Bazán, J.L. (2018). L-Logistic Distribution: Properties, Inference and an Application to Study Poverty and Inequality in Brazil.
#'
#' @details This function compute the highest posterior density intervals for a Bayesian posterior distribution.
#'
#' @examples
#' # Modelation the coeficient with generated data
#'
#' library(llbayesireg)
#' library(llogistic)
#'
#' # Number of elements to be generated
#'
#' n=50
#'
#' # Generated response
#'
#' bin=2005
#' set.seed(bin)
#' y=rllogistic(n,0.5, 2)
#'
#' fitll = llbayesireg(y, niter=100, jump=10)
#'
#' llHPD(fitll)
#'
#' \donttest{
#' # Modelation the coeficient with real data
#'
#' library(llbayesireg)
#'
#' data("Votes","MHDI")
#'
#' y = Votes[,4]
#' X = MHDI
#'
#' fitll = llbayesireg(y,X)
#'
#' llHPD(fitll)
#' }
#'
#' @importFrom coda HPDinterval
#' @importFrom coda mcmc
#' @importFrom rstan traceplot
#'
#' @import  MCMCpack MASS
#'
#' @export llHPD
llHPD = function (fitll, prob=0.95, chain=1){

  if(is.null(fitll)){
    stop("There is no data")
  }

  betas=fitll$betas
  deltas=fitll$deltas

  if(length(fitll)==5){
    q=1
    d=1
  }else{
    q=fitll$q
    d=fitll$d
  }


  ###########Betas##########

  HPD_B=numeric()
  betaN=numeric(q)
  alpha=(1-prob)/2
  for(i in 1:q){
    if(length(fitll)==5){
      hpd=HPDinterval(mcmc( matrix(betas[,i])),prob=prob)
    }else{
      hpd=HPDinterval(mcmc( matrix(betas[,chain,i])),prob=prob)
    }
    beta=hpd[1:2]
    HPD_B=rbind(HPD_B,beta)
    num=toString(i-1)
    betaN[i]=paste("beta ",num)
  }
  HPD_T=t(HPD_B)
  HPD_T=data.frame(HPD_T)
  names(HPD_T)=betaN
  HPD_B=t(HPD_T)
  aux1=toString(round(alpha*100,2))
  aux2=toString(round((alpha+prob)*100,2))
  aux1=paste(aux1,"%",sep="")
  aux2=paste(aux2,"%",sep="")
  aux1=paste("lower", aux1)
  aux2=paste("upper", aux2)
  HPD_B=data.frame(HPD_B)
  names(HPD_B)=c(aux1,aux2)


  ###########Deltas##########


  HPD_D=numeric()
  deltaN=numeric(d)
  alpha=(1-prob)/2
  for(i in 1:d){
    if(length(fitll)==5){
      hpd=HPDinterval(mcmc( matrix(deltas[,i])),prob=prob)
    }else{
      hpd=HPDinterval(mcmc( matrix(deltas[,chain,i])),prob=prob)
    }
    delta=hpd[1:2]
    HPD_D=rbind(HPD_D,delta)
    num=toString(i-1)
    deltaN[i]=paste("delta ",num)
  }
  HPD_T=t(HPD_D)
  HPD_T=data.frame(HPD_T)
  names(HPD_T)=deltaN
  HPD_D=t(HPD_T)
  aux1=toString(round(alpha*100,2))
  aux2=toString(round((alpha+prob)*100,2))
  aux1=paste(aux1,"%",sep="")
  aux2=paste(aux2,"%",sep="")
  aux1=paste("lower", aux1)
  aux2=paste("upper", aux2)
  HPD_D=data.frame(HPD_D)
  names(HPD_D)=c(aux1,aux2)

  HPDList=list(BetasHPD=HPD_B,DeltasHPD=HPD_D)
  return(HPDList)
}
