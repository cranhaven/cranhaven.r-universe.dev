#' @title Censored Mixed-Effects Models with Autoregressive Correlation Structure and DEC for Normal and t-Student Errors
#' @import numDeriv
#' @import TruncatedNormal
#' @import LaplacesDemon
#' @import tcltk
#' @import MASS
#' @import stats
#' @import relliptical
#' @import expm
#' @description This functino fits left, right or intervalar censored mixed-effects linear model, with autoregressive errors of order \code{p}, using the EM algorithm. It returns estimates, standard errors and prediction of future observations.
#' @param y Vector \code{1 x n} of censored responses, where \code{n} is the sum of the number of observations of each individual
#' @param x Design matrix of the fixed effects of order \code{n x s}, corresponding to vector of fixed effects.
#' @param z Design matrix of the random effects of order\code{n x b}, corresponding to vector of random effects.
#' @param cc Vector of censoring indicators of length \code{n}, where \code{n} is the total of observations. For each observation: \code{0} if non-censored, \code{1} if censored.
#' @param nj Vector \code{1 x m} with the number of observations for each subject,  where \code{m} is the total number of individuals.
#' @param tt Vector \code{1 x n} with the time the measurements were made, where \code{n} is the total number of measurements for all individuals. Default it's considered regular times.
#' @param struc \code{UNC},\code{ARp},\code{DEC},\code{SYM} or \code{DEC(AR)} for uncorrelated ,autoregressive, DEC(phi1,phi2), DEC(phi1,phi2=1), DEC(DEC(phi1,phi2=1)) structure, respectively
#' @param order  Order of the autoregressive process. Must be a positive integer value.
#' @param initial List with the initial values in the next orden: betas,sigma2,alphas,phi and nu. If it is not indicated it will be provided automatically. Default is \code{NULL}
#' @param nu.fixed Logical. Should estimate the parameter "nu" for the t-student distribution?. If is False indicates the value in the list of initial values. Default is \code{FALSE}
#' @param typeModel \code{Normal} for Normal distribution and \code{Student} for t-Student distribution. Default is \code{Normal}
#' @param cens.type \code{left} for left censoring, \code{right} for right censoring and \code{interval} for intervalar censoring. Default is \code{left}
#' @param LI Vector censoring lower limit indicator of length \code{n}. For each observation: \code{0} if non-censored, \code{-inf} if censored. It is only indicated for when \code{cens.type} is \code{both}. Default is \code{NULL}
#' @param LS Vector censoring upper limit indicator of length \code{n}. For each observation: \code{0} if non-censored, \code{inf} if censored.It is only indicated for when \code{cens.type} is \code{both}. Default is \code{NULL}
#' @param MaxIter The maximum number of iterations of the EM algorithm. Default is \code{200}
#' @param error The convergence maximum error. Default is \code{0.0001}
#' @param Prev Indicator of the prediction process. Available at the moment only for the \code{typeModel=normal} case.  Default is \code{FALSE}
#' @param isubj Vector indicator of subject included in the prediction process. Default is \code{NULL}
#' @param step Number of steps for prediction. Default is \code{NULL}
#' @param xpre Design matrix of the fixed effects to be predicted. Default is \code{NULL}.
#' @param zpre Design matrix of the random effects to be predicted. Default is \code{NULL}.
#' @return returns list of class \dQuote{ARpMMEC}:
#' \item{FixEffect}{Data frame with: estimate, standar errors and confidence intervals of the fixed effects.}
#' \item{Sigma2}{Data frame with: estimate, standar errors and confidence intervals  of the variance of the white noise process.}
#' \item{Phi}{Data frame with: estimate, standar errors and confidence intervals  of the autoregressive parameters.}
#' \item{RandEffect}{Data frame with: estimate, standar errors and confidence intervals  of the random effects.}
#' \item{nu}{the parameter "nu" for the t-student distribution}
#' \item{Est}{Vector of parameters estimate (fixed Effects, sigma2, phi, random effects).}
#' \item{SE}{Vector of the standard errors of (fixed Effects, sigma2, phi, random effects).}
#' \item{Residual}{Vector of the marginal residuals.}
#' \item{loglik}{Log-likelihood value.}
#' \item{AIC}{Akaike information criterion.}
#' \item{BIC}{Bayesian information criterion.}
#' \item{AICc}{Corrected Akaike information criterion.}
#' \item{iter}{Number of iterations until convergence.}
#' \item{Yfit}{Vector "y" fitted}
#' \item{MI}{Information matrix}
#' \item{Prev}{Predicted values (if xpre and zpre is not \code{NULL}).}
#' \item{time}{Processing time.}
#' \item{others}{The first and second moments of the random effect and vector Y}
#' @references Olivari, R. C., Garay, A. M., Lachos, V. H., & Matos, L. A. (2021). Mixed-effects 
#' models for censored data with autoregressive errors. Journal of Biopharmaceutical Statistics, 31(3), 273-294.
#' \doi{10.1080/10543406.2020.1852246}
#' @examples
#' \dontrun{
#'p.cens   = 0.1
#'m           = 10
#'D = matrix(c(0.049,0.001,0.001,0.002),2,2)
#'sigma2 = 0.30
#'phi    = 0.6
#'beta   = c(1,2,1)
#'nj=rep(4,10)
#'tt=rep(1:4,length(nj))
#'x<-matrix(runif(sum(nj)*length(beta),-1,1),sum(nj),length(beta))
#'z<-matrix(runif(sum(nj)*dim(D)[1],-1,1),sum(nj),dim(D)[1])
#'data=ARpMMEC.sim(m,x,z,tt,nj,beta,sigma2,D,phi,struc="ARp",typeModel="Normal",p.cens=p.cens)
#'
#'teste1=ARpMMEC.est(data$y_cc,x,z,tt,data$cc,nj,struc="ARp",order=1,typeModel="Normal",MaxIter = 2)
#'teste2=ARpMMEC.est(data$y_cc,x,z,tt,data$cc,nj,struc="ARp",order=1,typeModel="Student",MaxIter = 2)
#'
#'xx=matrix(runif(6*length(beta),-1,1),6,length(beta))
#'zz=matrix(runif(6*dim(D)[1],-1,1),6,dim(D)[1])
#'isubj=c(1,4,5)
#'teste3=ARpMMEC.est(data$y_cc,x,z,tt,data$cc,nj,struc="ARp",order=1,typeModel="Normal",
#'                   MaxIter = 2,Prev=TRUE,step=2,isubj=isubj,xpre=xx,zpre=zz)
#'teste3$Prev
#'
#' }
#' 
#' 
#' @export
#' 
#' 
#' 

ARpMMEC.est=function(y,x,z,tt,cc,nj,struc="UNC",order=1, initial=NULL,nu.fixed=TRUE,
                      typeModel="Normal",cens.type="left", LI=NULL,LS=NULL, MaxIter=200,
                      error=0.0001, Prev=FALSE,step=NULL,isubj=NULL,xpre=NULL,zpre=NULL)
{
  
  m<-length(y); N<-sum(nj); p<-dim(x)[2]; q1<-dim(z)[2]; m1<-m*p; m2<-m*q1
  if(is.matrix(y)) y <- y[as.vector(!is.na(as.vector(t(y))))]
  if(is.matrix(cc)) cc <- cc[as.vector(!is.na(as.vector(t(cc))))]
  if (!is.matrix(x)) x=as.matrix(x)
  if (!is.matrix(z)) x=as.matrix(z)
  if( is.matrix(nj)) nj <- nj[as.vector(!is.na(as.vector(t(nj))))]
  
  
  if(!is.numeric(y))                    stop("y must be a numeric vector. Check documentation!")
  if(sum(is.na(y))>0)                   stop("Vector y does not support NA values.")
  if(!is.vector(y))                     stop("y must be a vector.Check documentation!")
  if(length(y)!=nrow(as.matrix(x)))     stop("x does not have the same number of lines than y.")
  if(length(y)!=length(cc))             stop("cc does not have the same length than y.")
  if(length(y)!=nrow(as.matrix(z)))     stop("x does not have the same number of lines than y.")
  if(length(y)!=sum(nj))                stop("not compatible sizes between the response y and the repetited measures nj")
  if(length(y)==0)                      stop("The parameter y must be provided.")
  if(length(y)!=length(tt))             stop("not compatible sizes between the response y and the vector time tt")
  
  
  if(!is.numeric(x))                    stop("x must be a numeric matrix. Check documentation!")
  if(sum(is.na(x))>0)                   stop("There are some NA values in x.")
  if(!is.matrix(x))                     stop("x must be a matrix. Check documentation!")
  if(det(t(x)%*%x)==0)                  stop("the columns of x must be linearly independent.")
  if(length(x)==0)                      stop("The parameter x must be provided.")
  
  
  if(!is.numeric(z))                    stop("z must be a numeric matrix. Check documentation!")
  if(!is.matrix(z))                     stop("z must be a matrix. Check documentation!")
  if(sum(is.na(z))>0)                   stop("There are some NA values in z.")
  if(length(z)==0)                      stop("The parameter z must be provided.")
  
  
  if(!is.numeric(cc))                   stop("cc must be a numeric vector. Check documentation!")
  if(!is.vector(cc))                    stop("cc must be a vector.Check documentation!")
  if(sum(is.na(cc))>0)                  stop("There are some NA values in cc.")
  if(sum(cc%in%c(0,1))<length(cc))      stop("The elements of the vector cc must be 0 or 1.")
  if(length(cc)==0)                     stop("The parameter cc must be provided.")
  
  
  if(!is.numeric(nj))                   stop("nj must be a numeric vector. Check documentation!")
  if(!is.vector(nj))                    stop("nj must be a vector. Check documentation!")
  if(sum(is.na(nj))>0)                  stop("There are some NA values in nj")
  if(length(nj)==0)                     stop("The parameter nj must be provided.")
  
  
  if(struc!="DEC"&struc!="DEC(AR)"&struc!="SYM"&struc!="ARp"&struc!="UNC") stop("Struc must be UNC, DEC, DEC(AR), SYM or ARp. Check documentation!")
  if(struc=="ARp"){
    if(!is.numeric(order)            )       stop("Orde must be a number. Check documentation!")
    if(length(order)!=1)                    stop("Order must be a value.")
    if(is.numeric(order))
    { if(order!=round(order)|order<=0)         stop("Order must be a positive integer value.")}}
  
  if(!is.null(initial))
  {  if(!is.null(initial$betas))
  {if(!is.numeric(initial$betas))             stop("betas must be a numeric vector. Check documentation!")
    if(!is.vector(initial$betas))              stop("betas must be a vector. Check documentation!")
    if(length(initial$betas)!=ncol(x))         stop("not compatible sizes between the matrix x and parameter betas.")}
    if(!is.null(initial$sigma2))
    {if(!is.numeric(initial$sigma2))           stop("sigma2 must be a scalar.")
      if(length(initial$sigma2)>1)              stop("sigma2 must be a scalar.")}
    if(!is.null(initial$alphas))
    {if(!is.matrix(initial$alphas))                stop("alphas must be a matrix.")
      if(initial$alphas[upper.tri(initial$alphas)]!=initial$alphas[lower.tri(initial$alphas)])stop("alphas must be a simetric matrix.")
      if(dim(initial$alphas)[2]!=ncol(z))            stop("not compatible sizes between the matrix z and parameter alphas.")}
    if(struc=="ARp"){
      if(!is.null(initial$phi))
      {if(!is.numeric(initial$phi))              stop("phi must be a numeric vector. Check documentation!")
        if(length(initial$phi)!=order)              stop("not compatible sizes between the value Arp and parameter phi. Check documentation!")}
    }
  }
  
  if(typeModel!='Normal'& typeModel!='Student')   stop('typeModel must be Normal or Student. Check documentation!')
  
  
  if(cens.type!="left" & cens.type!="right" & cens.type!="interval")stop('cens.type must be left, right or interval. Check documentation!')
  
  
  if(cens.type=="interval"&is.null(LI))    stop("The parameter LI must be provided.. Check documentation!")
  if(cens.type=="interval"&is.null(LS))    stop("The parameter LS must be provided.. Check documentation!")
  if(!is.null(LI)&!is.numeric(LI))     stop("LI must be a numeric vector. Check documentation!")
  if(!is.null(LS)&!is.numeric(LS))     stop("LS must be a numeric vector. Check documentation!")
  if(length(LS)!=length(LI))           stop("not compatible sizes between the vectors LI and LS. Check documentation!")
  if(cens.type=="interval") {
    if(length(y)!=length(LI))            stop("not compatible sizes between the vectors y and LI. Check documentation!")
    if(length(y)!=length(LS))            stop("not compatible sizes between the vectors y and LS. Check documentation!")
  }
  
  
  if (!is.numeric(MaxIter))            stop("MaxIter must be a positive number. Check documentation!")
  if (length(MaxIter) > 1)             stop("MaxIter parameter must be a scalar")
  if (MaxIter <0)                      stop("MaxIter parameter must be positive number")
  if (!is.numeric(error))                 stop("error must be a positive number. Check documentation!")
  if (length(error) > 1)                  stop("error parameter must be a scalar")
  if (error <0)                           stop("error parameter must be positive number")
  
  
  
  if (Prev) {
    if(is.null(step)|is.null(xpre)|is.null(zpre)|is.null(isubj)) stop("step, isubj, xpre, zpre needs to be provided. Check documentation!")
    if (!is.numeric(isubj))             stop("isubj must be a numeric vector. Check documentation!")
    if (!is.numeric(step))              stop("step must be a positive number. Check documentation!")
    if (step <0)                        stop("step parameter must be positive number")
    if (length(step) > 1)               stop("step parameter must be a scalar")
    if (ncol(xpre)!=ncol(as.matrix(x))) stop("xpre must have the same number of columns than x")
    if (sum(is.na(xpre))>0)             stop("There are some NA values in xpre")
    if (!is.numeric(xpre))              stop("xpred must be a numeric matrix")
    if (ncol(zpre)!=ncol(as.matrix(z))) stop("zpre must have the same number of columns than z")
    if (sum(is.na(zpre))>0)             stop("There are some NA values in zpre")
    if (!is.numeric(zpre))              stop("zpred must be a numeric matrix")
    if(nrow(xpre)!=length(isubj)*step)  stop("not compatible sizes between xpre and isubj. Check documentation!")
    if(nrow(zpre)!=length(isubj)*step)  stop("not compatible sizes between zpre and isubj. Check documentation!")
    
  }
  

  if(typeModel=="Normal"){
    if(struc=="ARp"){
      out<-EMCensArpN(cc=cc,y=y,x=x,z=z,tt=tt,nj=nj, Arp=order, initial=initial, cens.type=cens.type, LI=LI,LS=LS,MaxIter=MaxIter,ee=error,
                      Prev=Prev,step=step,isubj=isubj ,xpre=xpre,zpre=zpre)}
    if(struc=="UNC"){
      out<-EMCensArpN(cc=cc,y=y,x=x,z=z,tt=tt,nj=nj, Arp=struc, initial=initial, cens.type=cens.type, LI=LI,LS=LS,MaxIter=MaxIter,ee=error,
                      Prev=Prev,step=step,isubj=isubj ,xpre=xpre,zpre=zpre)}
    if(struc=="DEC"|struc=="DEC(AR)"|struc=="SYM"){
      out<-EMCensDECN(cc=cc,y=y,x=x,z=z,tt=tt,nj=nj, struc=struc, initial=initial, cens.type=cens.type, LI=LI,LS=LS,MaxIter=MaxIter,ee=error,
                      Prev=Prev,step=step,isubj=isubj ,xpre=xpre,zpre=zpre)}
  }

  if(typeModel=="Student"){
    if(struc=="ARp"){
      out<-EMCensArpT(cc=cc,y=y,x=x,z=z,ttc=tt,nj=nj,Arp=order,initial=initial,cens.type=cens.type,LL=LI,LU=LS,nu.fixed=nu.fixed,
                      iter.max=MaxIter,precision=error)
      }
    if(struc=="UNC"){
          out<-EMCensArpT(cc=cc,y=y,x=x,z=z,ttc=tt,nj=nj,Arp=struc,initial=initial,cens.type=cens.type,LL=LI,LU=LS,nu.fixed=nu.fixed,
                      iter.max=MaxIter,precision=error)
      }
    if(struc=="DEC"|struc=="DEC(AR)"|struc=="SYM"){
      out<-EMCensDECT(cc=cc,y=y,x=x,z=z,ttc=tt,nj=nj,struc=struc,initial=initial,cens.type=cens.type,LL=LI,LU=LS,nu.fixed=nu.fixed,
                      iter.max=MaxIter,precision=error)
      }
    
  }
  
  if(struc=="ARp")  
  {
    cat('\n')
    cat('---------------------------------------------------\n')
    cat('Autoregressive censored mixed-effects models \n')
    cat('---------------------------------------------------\n')
    cat('\n')
    cat("Autoregressive order =",order)
    cat('\n')
    cat("Distribution =",typeModel)
    cat('\n')
    if(typeModel=="Student") cat("nu =",out$nu);  cat('\n')
    cat("Subjects =",length(nj),";",'Observations =',sum(nj))
    cat('\n')
    cat('\n')
    cat('-----------\n')
    cat('Estimates\n')
    cat('-----------\n')
    cat('\n')
    cat('- Fixed effects \n')
    cat('\n')
    print(out$tableB)
    cat('\n')
    cat('\n')
    cat('- Sigma^2 \n')
    cat('\n')
    print(out$tableS)
    cat('\n')
    cat('\n')
    cat('- Autoregressives parameters\n')
    cat('\n')
    print(out$tableP)
    cat('\n')
    cat('\n')
    cat('- Random effects \n')
    cat('\n')
    print(out$tableA)
    cat('\n')
    cat('\n')
    cat('------------------------\n')
    cat('Model selection criteria\n')
    cat('------------------------\n')
    cat('\n')
    critFin <- c(out$loglik, out$AIC, out$BIC)
    critFin <- round(t(as.matrix(critFin)),digits=3)
    dimnames(critFin) <- list(c("Value"),c("Loglik", "AIC", "BIC"))
    print(critFin)
    cat('\n')
    cat('-------\n')
    cat('Details\n')
    cat('-------\n')
    cat('\n')
    cat("Convergence reached? =",(out$iter < MaxIter))
    cat('\n')
    cat('Iterations =',out$iter,"/",MaxIter)
    cat('\n')
    cat("Processing time =",out$time,units(out$time))
    cat('\n')
  }
  
  if(struc!="ARp")  
  {
    cat('\n')
    cat('---------------------------------------------------\n')
    cat('DEC censored mixed-effects models \n')
    cat('---------------------------------------------------\n')
    cat('\n')
    cat("Case =",struc)
    cat('\n')
    cat("Distribution =",typeModel)
    cat('\n')
    if(typeModel=="Student") cat("nu =",out$nu);  cat('\n')
    cat("Subjects =",length(nj),";",'Observations =',sum(nj))
    cat('\n')
    cat('\n')
    cat('-----------\n')
    cat('Estimates\n')
    cat('-----------\n')
    cat('\n')
    cat('- Fixed effects \n')
    cat('\n')
    print(out$tableB)
    cat('\n')
    cat('\n')
    cat('- Sigma^2 \n')
    cat('\n')
    print(out$tableS)
    cat('\n')
    cat('\n')
    if(struc!="UNC"){
      cat('- Autoregressives parameters\n')
      cat('\n')
      print(out$tableP)
      cat('\n')
      cat('\n')}
    cat('- Random effects \n')
    cat('\n')
    print(out$tableA)
    cat('\n')
    cat('\n')
    cat('------------------------\n')
    cat('Model selection criteria\n')
    cat('------------------------\n')
    cat('\n')
    critFin <- c(out$loglik, out$AIC, out$BIC)
    critFin <- round(t(as.matrix(critFin)),digits=3)
    dimnames(critFin) <- list(c("Value"),c("Loglik", "AIC", "BIC"))
    print(critFin)
    cat('\n')
    cat('-------\n')
    cat('Details\n')
    cat('-------\n')
    cat('\n')
    cat("Convergence reached? =",(out$iter < MaxIter))
    cat('\n')
    cat('Iterations =',out$iter,"/",MaxIter)
    cat('\n')
    cat("Processing time =",out$time,units(out$time))
    cat('\n')
  }
  
  if(typeModel=="Student") nu<-out$nu
  if(typeModel=="Normal") nu<-NULL
  
  obj.out <- list(FixEffect=out$tableB, Sigma2=out$tableS, Phi=out$tableP,RandEffect=out$tableA, nu=nu,
                  Est=c(out$beta1, sigma2=out$sigmae, phi=out$phi, RnEffect=out$dd), SE=out$SE,Residual=out$residual,
                  loglik=out$loglik, AIC=out$AIC, BIC=out$BIC, AICc=out$AICcorr, iter=out$iter, Yfit=out$yfit,
                  MI=out$MI, Prev=out$Prev, time=out$time, others=list(ubi = out$ubi, ubbi = out$ubbi, uybi = out$uybi, uyi = out$uyi, uyyi = out$uyyi,varbeta=out$varbeta,yog=out$yorg)  )
  
  
  class(obj.out)  =  "ARpMMEC"
  return(obj.out)
  
}


