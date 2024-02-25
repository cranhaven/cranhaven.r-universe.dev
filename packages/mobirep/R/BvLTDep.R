#' Fits bivariate joint tail model
#'
#' Take data on uniform margins and fit the Ledford and Tawn (1997) joint tail model. Also contains the method where additional information from values that are extreme in at most one variable is used.
#'
#' @param data Two column data frame with values given on uniform margins
#' @param mod.thresh.u Modelling threshold on uniform margin
#' @param crit.lev.u Critical level on uniform margin
#' @param sig.lev Significance level for confidence intervals (\code{default} 0.05 for 95\code{\%} confidence interval)
#' @param ci.meth Method to use to obtain confidence intervals, '\code{se}' for standard error confidence intervals and '\code{pl}' for profile likelihood confidence intervals
#' @param marg.inf Is additional marginal information from points that are extreme in only one variable used? \code{FALSE} gives Ledford and Tawn (1997) result, \code{TRUE} gives results using Section 2.4.2 from Hugo Winter's thesis.
#' @export
#' @return Estimates of two dependence parameters with confidence intervals:
#' \itemize{
#' \item  threshold dependent extremal dependence measure
#' \item  threshold dependent coefficient of tail dependence
#' }
#'
#' @importFrom stats approx cor.test na.omit optim
#'          predict qchisq qnorm quantile rnorm spline uniroot


Bv.LT.Dep <- function(data,mod.thresh.u,crit.lev.u,sig.lev=0.05,ci.meth='se',marg.inf=F){

  n <- dim(data)[1]   # How many data points are there?

  # Convert to Pareto margins, since this removes approximation
  # made in Frechet margins
  data.p <- 1/(1-data); data.f <- -1/log(data)
  mod.thresh.p <- 1/(1-mod.thresh.u); crit.lev.p <- 1/(1-crit.lev.u)
  mod.thresh.f <- -1/log(mod.thresh.u); crit.lev.f <- -1/log(mod.thresh.u)


  # Find minimum value of each pair of data (x,y)
  min.f <- apply(data.f,MARGIN=1,FUN=min); min.p <- apply(data.p,MARGIN=1,FUN=min)
  bF <- min.f[min.f>mod.thresh.f]; b <- min.p[min.p>mod.thresh.p]

  # The estimate of (c,eta) depends on the likelihood considered
  if ( marg.inf == F ){
    # Diagnostic plot Y vs log ((1-w)/w)
    nu <- sum(min.p>mod.thresh.p)
    Temp<-data.p[which(min.p>mod.thresh.p),]
    wstar<-Temp[,1]/(Temp[,1]+Temp[,2])
    val1<-(1-wstar)/wstar*mod.thresh.p
    log1<-log((1-wstar)/wstar)

    varext1<- data.p[which(data.p[,1]>mod.thresh.p),]

    fbar1<-c()
    for (i in 1:length(val1)){
      bm<-varext1[which(varext1[,2]>val1[i]),]
      fbar<-length(bm)/n
      fbar1<-c(fbar1,fbar)}

    varext2<- data.p[which(data.p[,2]>mod.thresh.p),]
    fbar2<-c()
    for (i in 1:length(val1)){
      bm<-varext2[which(varext2[,1]>val1[i]),]
      fbar<-length(bm)/n
      fbar2<-c(fbar2,fbar)}

    # Y<-log(fbar1/fbar2)
    # mierda<-cbind(log1,Y)
    # mi<-mierda[order(mierda[,1]),]
    # mi2<-mi[which(mi[,1]>0),]
    # plot(mi2, type="o")
    # abline(h=0)

    jtm.nll <- function(p,z,n,nu,thresh){
      # Constrain the likelihood function to stop it straying into parameter
      # space that will produce non-finite results i.e 0<c<u^(1/eta)
      par.jtm <- thresh^(1/p[2])
      if(0<p[1] && p[1]<par.jtm){
        return( -(n-nu)*log(1-p[1]*thresh^(-1/p[2])) - nu*log(p[1]) + nu*log(p[2]) + ((1/p[2])-1)*sum(log(z)) )
      } else {
        return(10^6)
      }
    }

    # Choose lower and upper bounds as well as start point for the optim
    # function so that it does not stray outside allowed parameter space
    bl <- c(0.01,0.01); bu <- c(Inf,1); start <- c(0.1,0.1)

    # The hessian command in the optim function computes and returns the
    # hessian matrix
    par.vec.dep <- optim(start,jtm.nll,method="L-BFGS-B",z=b,n=n,nu=nu,thresh=mod.thresh.p,lower=bl,upper=bu,hessian=TRUE)

    # Assign the optimized parameters to variables chat and etahat
    chat <- par.vec.dep$par[1]; etahat <- par.vec.dep$par[2]; chibar <- (2*etahat)-1
    # Obtain an estimate of etahat directly using mle
    etahat.mle <- min((sum(log(b/mod.thresh.p))/nu),1)
    etahat.ad <- 1

    # Try a fit where etahat is fixed at the mle value and the other
    # parameter is allowed to vary
    jtm.nll2 <- function(p,z,n,nu,thresh,emle){
      # Constrain the likelihood function to stop it straying into parameter
      # space that will produce non-finite results i.e 0<c<u^(1/eta)
      par.jtm <- thresh^(1/emle)
      if(0<p[1] && p[1]<par.jtm){
        return( -(n-nu)*log(1-p[1]*thresh^(-1/emle)) - nu*log(p[1]) + nu*log(emle) + ((1/emle)-1)*sum(log(z)) )
      } else {
        return(10^6)
      }
    }
    bl <- c(0.01); bu <- c(Inf); start <- c(0.1)
    par.vec.dep.fix <- optim(start,jtm.nll2,method="L-BFGS-B",z=b,n=n,nu=nu,thresh=mod.thresh.p,emle=etahat.mle,lower=bl,upper=bu,hessian=TRUE)
    chat.mle <- par.vec.dep.fix$par
    par.vec.dep.ad <- optim(start,jtm.nll2,method="L-BFGS-B",z=b,n=n,nu=nu,thresh=mod.thresh.p,emle=etahat.ad,lower=bl,upper=bu,hessian=TRUE)
    chat.ad <- par.vec.dep.ad$par  # (nu/n)*crit.lev.p

  } else if ( marg.inf == T ){
    nu <- sum(min.p>mod.thresh.p)
    n01 <- sum( data.p[,1]<mod.thresh.p & data.p[,2]>mod.thresh.p )
    n10 <- sum( data.p[,1]>mod.thresh.p & data.p[,2]<mod.thresh.p )
    n00 <- n - n01 - n10 - nu

    jtm.nll3 <- function(p,z,n00,n01,n10,nu,u){
      # Constrain the likelihood function to stop it straying into parameter
      # space that will produce non-finite results i.e 0<c<u^(1/eta)
      par.jtm <- u^(1/p[2]); par.jtm2 <- u^(1/p[2]-1)
      if((par.jtm*(2/u-1))<p[1] && p[1]<par.jtm2){
        return( -( n00*log(1 - 2/u + p[1]*u^(-1/p[2])) + n01*log(1/u - p[1]*u^(-1/p[2])) + n10*log(1/u - p[1]*u^(-1/p[2])) + nu*log(p[1]) - nu*log(p[2]) - ((1/p[2])-1)*sum(log(z))) )
      } else {
        return(10^6)
      }
    }

    # Choose lower and upper bounds as well as start point for the optim
    # function so that it does not stray outside allowed parameter space
    bl <- c(0.01,0.01); bu <- c(Inf,1); start <- c(0.1,0.1)

    # The hessian command in the optim function computes and returns the
    # hessian matrix
    par.vec.dep <- optim(start,jtm.nll3,method="L-BFGS-B",z=b,n00=n00,n01=n01,n10=n10,nu=nu,u=mod.thresh.p,lower=bl,upper=bu,hessian=TRUE)

    # Assign the optimized parameters to variables chat and etahat
    chat <- par.vec.dep$par[1]; etahat <- par.vec.dep$par[2]; chibar <- (2*etahat)-1
    # Obtain an estimate of etahat directly using mle
    etahat.mle <- min((sum(log(b/mod.thresh.p))/nu),1)
    etahat.ad <- 1

    jtm.nll4 <- function(p,z,n00,n01,n10,nu,u,emle){
      # Constrain the likelihood function to stop it straying into parameter
      # space that will produce non-finite results i.e 0<c<u^(1/eta)
      par.jtm <- u^(1/emle); par.jtm2 <- u^(1/emle-1)
      if((par.jtm*(2/u-1))<p[1] && p[1]<par.jtm2){
        return( -( n00*log(1 - 2/u + p[1]*u^(-1/emle)) + n01*log(1/u - p[1]*u^(-1/emle)) + n10*log(1/u - p[1]*u^(-1/emle)) + nu*log(p[1]) - nu*log(emle) - ((1/emle)-1)*sum(log(z))) )
      } else {
        return(10^6)
      }
    }
    bl <- c(0.01); bu <- c(Inf); start <- c(0.1)
    par.vec.dep.fix <- optim(start,jtm.nll4,method="L-BFGS-B",z=b,n00=n00,n01=n01,n10=n10,nu=nu,u=mod.thresh.p,emle=etahat.mle,lower=bl,upper=bu,hessian=TRUE)
    chat.mle <- par.vec.dep.fix$par
    par.vec.dep.ad <- optim(start,jtm.nll4,method="L-BFGS-B",z=b,n00=n00,n01=n01,n10=n10,nu=nu,u=mod.thresh.p,emle=etahat.ad,lower=bl,upper=bu,hessian=TRUE)
    chat.ad <- par.vec.dep.ad$par
  }

  # Define the function chi(z)=g(c,eta)
  g.func <- function(p,z){
    return( p[1]*(z^(1-(1/p[2]))) )
  }

  f.func <- function(p,z){
    return( 1/(1-(log10(p[2]/p[1])/(log10(z)))) )
  }
  # Calculate the value of chi at the MLEs
  chi <- g.func(c(chat,etahat),crit.lev.p)
  et<- f.func(c(chat,chi),crit.lev.p)
  chi.mle <- g.func(c(chat.mle,etahat.mle),crit.lev.p)
  chi.ad <- g.func(c(chat.ad,etahat.ad),crit.lev.p)

  # Generate confidence intervals for chi
  if(ci.meth=='se'){
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    # Standard Error Method

    # Compute the derivatives using a finite differencing scheme and
    # ensure that eps1 and eps2 are not too large or else the
    # approximation is not valid

    eps1 <- rnorm(1,mean=0,sd=0.01)
    dgdc <- (g.func(c(chat+eps1,etahat),crit.lev.f) - g.func(c(chat,etahat),crit.lev.f))/eps1
    eps2 <- rnorm(1,mean=0,sd=0.01)
    dgde <- (g.func(c(chat,etahat+eps2),crit.lev.f) - g.func(c(chat,etahat),crit.lev.f))/eps2
    dgV <- as.vector(c(dgdc,dgde))

    # We should see perfect dependence between the control data and itself
    # Obtain the quantile CIq of the normal distribution to be used
    CIq <- 1-(sig.lev/2)

    # Obtain the variance for phi=chi(z) by inverting the hessian matrix
    # to get the inverted information matrix and then multiply by the
    # derivative of the function phi=g(c,eta) on both sides
    var.phi <- t(dgV) %*% solve(par.vec.dep$hessian) %*% dgV
    CIl <- chi - qnorm(CIq)*sqrt(var.phi)
    CIu <- chi + qnorm(CIq)*sqrt(var.phi)

    # We should see perfect dependence between the control data and itself #
    if (all(data[,1]==data[,2])){
      chi <- 1; CIl <- 1; CIu <- 1
    }
    chiCIs <- c(CIl,CIu)
    etaCIs <- c(f.func(c(chat,CIl),crit.lev.p),f.func(c(chat,CIu),crit.lev.p))


    ##An independent case


    # Standard error CIs are naturally symmetric so will lead to
    # negative lower bounds, if this happens then set the lower bound
    # to zero

    # Loop to work out pair (chi,chibar) which can either have form
    # (chi>0,chibar=1) for asympt. dependence and (chi=0,chibar<1)
    # for asympt. independence

    #~~~~~~~~~~~~~~~~~~~~~ Pl method ~~~~~~~~~~~~~~~~~~~~~#

  } else if(ci.meth=='pl') {

    if (all(data[,1]==data[,2])){
      chi <- 1; chibar <- 1
    }

    # Loop to work out pair (chi,chibar) which can either have form
    # (chi>0,chibar=1) for asympt. dependence and (chi=0,chibar<1)
    # for asympt. independence

    # Now add code to compute profile likelihood based CIs

    # Compute the likelihood at the MLEs phihat ('chi' below) and etahat
    # Will be used in the calculation of the deviance later
    llik.ph.ep <- (n-nu)*log(1-(chi*(crit.lev.p^((1/etahat)-1)))/(mod.thresh.p^(1/etahat))) + nu*log(chi) + nu*log(crit.lev.p^((1/etahat)-1)) - nu*log(etahat) - ((1/etahat)+1)*sum(log(b))

    # The negative log-likelihood function is defined below which is used
    # when calling the optim function. Seek to minimize this.

    # 'p' is the parameter we are maximizing over (eta), 'phi' is a given value
    # of phi, 'u' is a threshold, 'hu' is the higher threshold, 'n' the number
    # of data points, 'nu' the number of data points that exceed u and z is the
    # data set of exceedances

    nllik.pl <- function(p,phi,thresh,hu,n,nu,z){
      # Calculate the criteria for setting nll=10^6 for parameter space that could
      # throw up problems

      par.sp.ch <- phi*(hu^((1/p)-1))*(thresh^(-1/p))
      if(0<par.sp.ch && par.sp.ch<1){
        return( -(n-nu)*log(1-par.sp.ch) - nu*log(phi) - nu*log(hu^((1/p)-1)) + nu*log(p) + ((1/p)+1)*sum(log(z)))
      } else {
        return(10^6)
      }
    }

    # Choose lower and upper bounds as well as start point for the optim
    # function so that it does not stray outside allowed parameter space
    bl <- 0.01; bu <- 1; start <- 0.7

    # Function below obtains profile likelihood for a given value of phi
    Pl.val <- function(phi){

      # Start the optim function to obtain etahat related to the given phi
      pl.vec <- optim(start,nllik.pl,method="L-BFGS-B",phi=phi,thresh=mod.thresh.p,hu=crit.lev.p,n=n,nu=nu,z=b,lower=bl,upper=bu)
      etahat2 <- pl.vec$par
      test.vec <- pl.vec$value

      # I thought that instead of inputting values of etahat and phi into
      # log-likelihood, could take the negative of the value of negative
      # log-likelihood at phi and etahat

      return( -pl.vec$value )

    }

    # Function to work out the deviance for a given phi
    D.val <- function(phi){
      return(2*(llik.ph.ep-Pl.val(phi)))
    }

    # Subtract the relevant chi-squared value to obtain a value upon which
    # a root-finding algorithm can be used
    crit.val <- function(phi){
      return(D.val(phi) - qchisq((1-sig.lev),1))
    }

    # If looking at control square against itself, value of chi should be 1
    if(all(all(data[,1]==data[,2]))){
      chiCIs <- list()
      chiCIs <- c(1,1)
    } else if (chi < 1e-10) {
      chiCIs <- c(0,uniroot(crit.val,c(chi,1))$root)
    } else {
      chiCIs <- c(uniroot(crit.val,c(1e-100,chi))$root,uniroot(crit.val,c(chi,1))$root)
    }
    etaCIs <- c(f.func(c(chat,CIl),crit.lev.p),f.func(c(chat,CIu),crit.lev.p))

    # Have started using uniroot instead of uniroot.all. The latter command
    # uses a strange method that is hard to use here. I am only currently
    # finding one root using this method, will need to extend in future

  }

  dep.list <- list()
  dep.list$par <- c(chat,etahat)
  dep.list$chi <- chi
  dep.list$chiCIs <- chiCIs
  dep.list$par.fix.mle <- c(chat.mle, etahat.mle)
  dep.list$chi.fix.mle <- chi.mle
  dep.list$par.ad <- c(chat.ad, etahat.ad)
  dep.list$chi.ad <- chi.ad
  dep.list$etaCIs<- etaCIs

  return(dep.list)

}
