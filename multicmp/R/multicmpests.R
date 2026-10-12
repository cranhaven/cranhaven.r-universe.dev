#' Bivariate COM-Poisson Parameter Estimation
#'
#' \code{multicmpests} computes the maximum likelihood estimates of a bivariate COM-Poisson distribution (based on the model described in Sellers et al. (2016)) for given count data and conducts a test for significant data dispersion, relative to a bivariate Poisson model.
#'     The bivariate Poisson case is addressed via the bivpois package by Karlis and Ntzoufras (2009).
#'
#' @param data A two-column dataset of counts.
#' @param max Truncation term for infinite summation associated with the Z function. See Sellers et al. (2016) for details.
#' @param startvalues A vector of starting values for maximum likelihood estimation. The values are read as follows: c(lambda, nu, p00, p10, p01, p11).
#'     The default is c(1,1, 0.25, 0.25, 0.25, 0.25).
#' @return \code{multicmpests} will return a list of four elements: $par (Parameter Estimates), $negll (Negative Log-Likelihood), $LRTbpd (Dispersion Test Statistic), and
#'     $pbpd (Dispersion Test P-Value).
#'     
#' @references 
#'     Sellers KF, Morris DS, Balakrishnan N (2016) Bivariate Conway-Maxwell-Poisson Distribution: Formulation, Properties, and Inference, Journal of Multivariate Analysis 150:152-168.
#'     
#'     Karlis D., Ntzoufras I. (2009) bivpois: Bivariate Poisson Models Using the EM Algorithm, Version 0.50-3.1. http://cran.wustl.edu/web/packages/bivpois/index.html
#'
#' @examples 
#'     x1 <- c(3,2,5,4,1)
#'     x2 <- c(0,4,1,0,1)
#'     ex.data <- cbind(x1,x2)
#'     
#'     # starting close to the optimum for sake of run time
#'     multicmpests(ex.data, startvalues = c(12.5 , 1.7 , 0, 0.25, 0.75, 0)) 
#' 
#' @import utils
#' @import numDeriv
#' @import stats
#'
#' @export

####################################################################
## Program name: multicmpests.R                                   ##
## Authors: Kimberly Sellers and Darcy Steeg Morris               ##
## Date: 8/20/2015                                                ##
## Decription: Estimates BCMP parameters via ML.                  ##
## Inputs: Data (x & y).                                          ##
## Output: MLEs (lambda, nu, p00, p10, p01 and p11), negative LL. ##
####################################################################

## Added 3/22/17: LRT (nu=1) and bivariate Poisson model ##

multicmpests <- function(data,max = 100,startvalues=NULL) {
	if (dim(data)[2] != 2) {
		stop('data must have 2 columns')
	}
	x <-  data[,1]
	y <- data[,2]
	maxit <- max
	# Likelihood Function #
	minusloglike <- function(par){
		lambda <- par[1]
		nu <- par[2]
		p <- par[3:6]
		nll <- -1*sum(log(dbivCMP(lambda=lambda,nu=nu,bivprob=p,x=x,y=y,maxit=maxit)))
		return(nll)
	}
	
	invisible(capture.output(bp <- simple.bp(x,y)))
	
	# Starting Values #
	if (is.null(startvalues)) {
		p10 = .25
		p01 = .25
		p00 = .25
		p11 = .25
		lambda_start = 1
		nu_start = 1
	}
	if (is.null(startvalues)==0) {
		p10 = startvalues[5]
		p01 = startvalues[4]
		p00 = startvalues[3]
		p11 = startvalues[6]
		lambda_start = startvalues[1]
		nu_start = startvalues[2]
	}
	
	# NLMINB to find MLEs #
	cat("Iterating...", "\n")
	BCMPests <- nlminb(start=c(lambda_start,nu_start,p00,p01,p10,p11), minusloglike, lower = c(rep(0,6)), upper = c(Inf,Inf,Inf,Inf,Inf,Inf),control=list(trace=10,iter.max=1000))
	
	LRT_bpd <- -2*(bp$loglikelihood[length(bp$loglikelihood)] - (-1*BCMPests$obj))
	p_bpd <- 1 - pchisq(abs(LRT_bpd),df=1)
	
	# prepare tabular data for printing
	cat("\n", "The parameter estimates ($par) are as follows:","\n")
	par.est <- data.frame("Parameter" = c("lambda", "nu", "p00", "p10", "p01", "p11")
	                     , "MLE" = c(BCMPests$par[1:2],BCMPests$par[3:6]/sum(BCMPests$par[3:6])) 
	                     )
	print(par.est, row.names = F)

	cat("\n", "Log-likelihood ($negll):", BCMPests$obj , "\n")
	
	cat("\n" , "Dispersion hypothesis test statistic ($LRTbpd) and p-value ($pbpd):","\n")
	hyp.tst <- data.frame("Likelihood ratio test" = LRT_bpd, "p-value" = p_bpd)
	print(hyp.tst, row.names = F)
	
	cat("\n")
	
	# quietly return list of results
	invisible(list(par=c(BCMPests$par[1:2],BCMPests$par[3:6]/sum(BCMPests$par[3:6])),negll=BCMPests$obj,LRTbpd=LRT_bpd,pbpd=p_bpd))
}


#################################################################################
## Program name: suma.R                                                        ##
## Authors: Kimberly Sellers and Darcy Steeg Morris                            ##
## Date: 8/20/2015                                                             ##
## Decription: Calculates terms of the inner sum (over a) of the BCMP density. ##
## Inputs: BCMP parameters (bivprob) and outer sum summation index (n).        ##
#################################################################################

suma <- function(bivprob, x, y, n){
  p <- bivprob/sum(bivprob)
  result <- rep(0,length(x))
  for (i in 1:length(x)) {
    a <- (n-x[i]-y[i]):n
    resulti <- rep(0, length(a))
    for (place in 1:length(a)) {
      terms <- c(a[place], n-a[place]-y[i], n-a[place]-x[i], x[i]+y[i]+a[place]-n)
      if(any(terms < 0)) resulti[place] <- 0
      else resulti[place] <- dmultinom(terms, prob=c(p[1],p[3],p[2],p[4]))
    }
    result[i] = sum(resulti) 
  }
  return(result)
}


#' The Bivariate Conway-Maxwell-Poisson Distribution
#'
#' Density for the Bivariate Conway-Maxwell-Poisson (CMP) distribution
#'
#' @param lambda Mean/rate parameter under Poisson model.
#' @param nu  Dispersion parameter.
#' @param bivprob Bivariate probabilities, {p00, p01, p10, p11}.
#' @param x x values
#' @param y y values
#' @param maxit Number of terms used to truncate infinite sum calculations.
#'     
#' @references 
#' Sellers KF, Morris DS, Balakrishnan N (2016) Bivariate Conway-Maxwell-Poisson Distribution: Formulation, Properties, and Inference, Journal of Multivariate Analysis 150:152-168.
#' 
#' @examples 
#' dbivCMP(lambda=10, nu=1, bivprob=c(0.4, 0.2, 0.3, 0.1), x=2, y=3, maxit = 100) 
#' #this is equivalent to the pmf P(X=2,Y=3) of a bivariate Poisson 
#' ##with lambda1=3, lambda2=2, lambda3=1
#' 
#' @import utils
#' @import numDeriv
#' @import stats
#'
#' @export
######################################################################
## Program name: dbivCMP.R                                          ##
## Authors: Kimberly Sellers and Darcy Steeg Morris                 ##
## Date: 8/20/2015                                                  ##
## Decription: Calculates BCMP density.                             ##
## Inputs: BCMP parameters (lambda, nu & bivprob) and data (x & y). ##
## Output: P(X=x,Y=y)                                               ##
######################################################################

dbivCMP <- function(lambda, nu, bivprob, x, y, maxit) {
  
  # lambda (constant) -- mean under Poisson model
  # nu (constant) -- dispersion parameter
  # bivprob -- c(p00, p01, p10, p11)
  # maxit -- stopping point making infinite sum finite
  
  # Compute the Z function #
  forans <- rep(0,maxit+1)
  for (j in 1:maxit) {
    temp <- rep(0,j)
    for (i in 1:j) {
      temp[i] <- lambda/(i^nu)
    }
    forans[j+1] <- prod(temp)
  }
  forans[1] <- 1
  z = sum(forans)
  
  # Calculate the (in)finite sum #
  forans <- matrix(0,nrow=length(x),ncol=maxit+1)
  for (n in 1:maxit){
    temp <- rep(0,n)
    for (i in 1:n){
      temp[i] <- lambda/(i^nu)
    }
    forans[,n+1] <- prod(temp)*suma(bivprob, x, y, n)  
  }
  forans[,1] <- suma(bivprob, x, y, 0)
  ans <- (1/z) * apply(forans,1,sum)
  return(ans)
}

"pbivpois" <-
  function(x, y=NULL, lambda = c(1, 1, 1), log=FALSE) {
    # ------------------------------------------------------------------------------
    # Karlis and Ntzoufras (2003, 2004)
    # EM algorithms for Bivariate Poisson Models
    # ------------------------------------------------------------------------------
    # x      : matrix or vector of length n
    # y      : vector of length n. If x is matrix then it is not used
    # lambda : parameters of the bivariate poisson distribution
    # log    : argument controlling the calculation of the log-probability or the 
    #          probability function. 
    # ------------------------------------------------------------------------------
    #	
    if ( is.matrix(x) ) {
      var1<-x[,1]
      var2<-x[,2]
    }
    else if (is.vector(x)&is.vector(y)){
      if (length(x)==length(y)){
        var1<-x
        var2<-y
      }
      else{
        stop('lengths of x and y are not equal')
      }	
    }
    else{
      stop('x is not a matrix or x and y are not vectors')
    }
    n <- length(var1)
    logbp<-vector(length=n)
    #
    for (k in 1:n){
      x0<-var1[k]
      y0<-var2[k]
      xymin<-min( x0,y0 )
      lambdaratio<-lambda[3]/(lambda[1]*lambda[2])
      #	
      i<-0:xymin
      sums<- -lgamma(var1[k]-i+1)-lgamma(i+1)-lgamma(var2[k]-i+1)+i*log(lambdaratio)
      maxsums <- max(sums)
      sums<- sums - maxsums
      logsummation<- log( sum(exp(sums)) ) + maxsums 
      logbp[k]<- -sum(lambda) + var1[k] * log( lambda[1] ) + var2[k] * log( lambda[2] ) + logsummation 
    }
    if (log) { result<-    logbp }
    else     { result<-exp(logbp)  }
    result
    #	end of function bivpois
  }

"simple.bp" <-
  function(x, y, ini3=1.0, maxit=300, pres=1e-8)
  {
    #
    # ------------------------------------------------------------------------------
    # Karlis and Ntzoufras (2003, 2004)
    # (last revision 25/8/2005)
    # Athens University of Economics and Business
    #
    # EM algorithms for Bivariate Poisson Models
    # ------------------------------------------------------------------------------
    #
    # x       : matrix or vector of length n
    # y       : vector of length n. If x is matrix then it is not used
    # ini3    : initial value for lambda3
    # maxit   : maximum number of iterations 
    # pres    : precision of the relative likelihood difference after which EM stops
    # ------------------------------------------------------------------------------
    # Data length
    #
    #
    if ( is.matrix(x) ) {
      var1<-x[,1]
      var2<-x[,2]
    }
    else if (is.vector(x)&is.vector(y)){
      if (length(x)==length(y)){
        var1<-x
        var2<-y
      }
      else{
        stop('lengths of x and y are not equal')
      }	
    }
    else{
      stop('x is not a matrix or x and y are not vectors')
    }
    
    #
    #
    #
    n<-length(var1)
    #
    # initial values
    s<-rep(0,n)
    like<-1:n*0
    zero<- ( var1==0 )|( var2==0 )
    #
    #
    #
    # Initial values for lambda
    
    lambda3<- ini3
    lambda1<- max( 0.1, mean(var1)-lambda3 )
    lambda2<- max( 0.1, mean(var2)-lambda3 )
    #
    #
    difllike<-1000.0
    loglike0<-1000.0
    i<-0
    loglike<-rep(0,maxit)
    while ( (difllike>pres) && (i <= maxit) ) {
      i<-i+1
      #####   E step  ######
      for (j in 1:n) {
        if (zero[j]) {
          s[j]<-0;
          like[j]<- log(dpois(var1[j], lambda1)) + log(dpois(var2[j], lambda2))-lambda3;
        }
        else {
          lbp1<- pbivpois( var1[j]-1, var2[j]-1, lambda=c(lambda1,lambda2,lambda3), log=TRUE );
          lbp2<- pbivpois( var1[j]  , var2[j]  , lambda=c(lambda1,lambda2,lambda3) , log=TRUE );
          
          s[j]<-exp( log(lambda3) + lbp1 - lbp2 );
          like[j]<-lbp2;
        }
      }
      ##### end of E step  ######
      x1<-var1-s
      x2<-var2-s
      loglike[i]<-sum(like)
      difllike<-abs( (loglike0-loglike[i])/loglike0 )
      loglike0<-loglike[i]
      #
      #
      #####   M step  ######
      #
      # 	fit model on lambda3
      lambda1<-mean(x1)
      lambda2<-mean(x2)
      lambda3<-mean(s)
      #####   end of M step  ######
      printpars<-c(i,lambda1, lambda2, lambda3, loglike[i] )
      names(printpars)<-c('Iter.', 'lambda1', 'lambda2', 'lambda3','loglike' )
      print( round(printpars ,3 ) )
      cat( 'Relative Difference in Loglike:', difllike, '\n' ) 
    }
    #
    #	calculation of BIC and AIC of Bivariate Poisson model
    noparams<- 3 
    AIC<- -2*loglike[i] + noparams * 2
    BIC<- -2*loglike[i] + noparams * log(2*n)
    #	
    #		
    #	Calculation of BIC, AIC of Poisson saturated model
    x.mean<-var1
    x.mean[var1==0]<-1e-12
    y.mean<-var2
    y.mean[var2==0]<-1e-12
    AIC.sat <-  sum( log( dpois( var1 , x.mean ) ) + log( dpois(var2 , y.mean) ) )
    BIC.sat <-  -2 * AIC.sat + (2*n)* log(2*n)
    AIC.sat <-  -2 * AIC.sat + (2*n)* 2
    #
    #		
    #	Calculation of BIC, AIC of simple Poisson model
    x.mean<-mean(var1)
    y.mean<-mean(var2)
    AIC.pois <-  sum(log( dpois( var1 , x.mean ) ) + log( dpois( var2 , y.mean ) ))
    BIC.pois <-  -2 * AIC.pois + 2* log(2*n)
    AIC.pois <-  -2 * AIC.pois + 2* 2
    
    AICtotal<-c(AIC.sat, AIC.pois, AIC)
    BICtotal<-c(BIC.sat, BIC.pois, BIC )
    
    names(AICtotal)<- c( 'Saturated', 'DblPois', 'BivPois' )
    names(BICtotal)<- c( 'Saturated', 'DblPois', 'BivPois' )
    #
    # Calculation of fitted values
    result<-list(lambda=c(lambda1, lambda2, lambda3),loglikelihood=loglike[1:i],
                 parameters=noparams, AIC=AICtotal, BIC=BICtotal ,iterations=i )
    #
    result
    #
    #
  }
