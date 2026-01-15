






mleLogis = function(X){

  nLeng = length(X)
  Dual = function(param){
    tempsum=0
    for (i in 1:nLeng){
      normx = (X[i]-param[1])/param[2]
      tempsum = tempsum + normx - log(param[2]) -2*log(1+exp(normx))
    }
    return(-tempsum)
  }
  return(Dual)

}

GetCauchyScale = function(XVec, mu){

  al = 0.5
  quant = quantile(XVec, al )
  Qval = quant[[1]]

  sig = (al-mu)/( tan( pi*( pcauchy(Qval, 0,1) - 0.5 ) ) )

  return( abs(sig)+0.0001)
}


MLECauchyObjFunction = function(XVec){
  nLength = length(XVec)

  Dual = function(x){
    tempsum = -nLength*log(pi*x[2])
    for(i in 1:nLength){
      tempsum = tempsum - log(1+( (XVec[i]-x[1] ) / x[2]  )^2)
    }
    return(-tempsum)
  }
  return(Dual)
}

Inv_Gumbel = function(x, mu, sig){

  return(mu-sig*log(-log(x)))

}

Generate_Gumbel = function(n, mu, sig){

  uVec = runif(n)

  xVec = Inv_Gumbel(uVec, mu, sig)
  return(xVec)

}





SortX = function(X, distr, bFast=FALSE){

  if(distr=="Normal"){

    muhat = mean(X)
    sigmahat = sd(X)


  }else if(distr=="Logistic"){

    if(bFast){
      muhat = mean(X)
      sdval = sd(X)

      sigmahat = sqrt(3)*sdval/pi

    }else{

      x01 = median(X)
      x02 = sqrt(3)/pi*sd(X)

      startVal = c(x01, x02)
      mle_method = optim(startVal, mleLogis(X))
      mle_par = mle_method$par

      muhat = mle_par[1]
      sigmahat = mle_par[2]


    }


  }else if(distr=="Cauchy"){

    if(bFast){
      muhat = median(X)
      sigmahat = GetCauchyScale(X, muhat)

    }else{

      x01 = median(X)

      quan3 = quantile(X, 0.75)
      quan1 = quantile(X, 0.25)

      x02 = (quan3[[1]]-quan1[[1]])/2

      ####################
      x0 = c(x01, x02)
      LBV = c(-Inf, 0)
      UBV = c(Inf, Inf)

      MRe = solnp( pars=x0, fun=MLECauchyObjFunction(X), eqfun=NULL, eqB=NULL, ineqfun=NULL, ineqLB = NULL,
                   ineqUB = NULL, LB = LBV, UB = UBV)

      CauchyMle = MRe$pars

      muhat = CauchyMle[1]
      sigmahat = CauchyMle[2]

    }


  }else if(distr=="Gumbel"){

    rho = 0.57721
    s = sd(X)
    xbar = mean(X)

    sigmahat = sqrt(6)*s/pi
    muhat = xbar-sigmahat*rho

  }

  X = (X-muhat)/sigmahat

  lst = list(Sorted_X = sort(X), muhat=muhat, sigmahat=sigmahat)

  return(lst)

}





#' Implementing Khmaladze Martingale Transformation.
#'
#' Performs goodness-of-fit test through Khmaladze matringale transformation
#'@param X  a random sample of n observations
#'@param strDistr  a null distribution for the hypothesis test: Normal, Cauchy, Logistic, or Gumbel.
#'@param bEstimation  a logical value which specifies whether or not to estimate parameters. The default value is TRUE. For FALSE, (\eqn{\mu}) and (\eqn{\sigma}) will be set as 0 and 1, respectively.
#'@param bFast_Estimation  a logical value which specifies whether or not to use the maximum likelihood estimator (\eqn{\hat{\theta}}) for the location and scale parameters The default value is FALSE.
#'@param bParallel a logical value which specifies whether or not to use the parallel computing. The default value is FALSE.
#'@param nThreads the number of threads when bParallel is TRUE. The default value is 16.

#'@return A list of the following values:
#'\describe{
#'\item{opt_x}{ opt.x is the value of x where the optimum of the objective function - which is also the test statistic - occurs.}
#'
#'\item{test_stat}{ test.stat is the test statistic obtained through Khmaladze martingale transformation.}
#'
#'\item{mu}{ the point estimate for the location parameter mu}
#'
#'\item{sigma}{ the point estimate for the scale parameter sigma}
#'}
#'
#'@examples
#'####################

#'n=20
#'mu0=2; sigma0=1
#'X = rnorm(n, mu0, sigma0)
#'
#'
#'Run_KMT(X, strDistr="Normal")
#'


#'@references
#'[1] Khmaladze, E.V., Koul, H.L. (2004). Martingale transforms goodness-of-fit tests in regression models. Ann. Statist., 32. 995-1034
#'@references
#'[2] E.V. Khmaladze, H.L. Koul (2009). Goodness-of-fit problem for errors in nonparametric regression: distribution free approach. Ann. Statist., 37(6A) 3165-3185.
#'@references
#'[3] Kim, Jiwoong (2020). Implementation of a goodness-of-fit test through Khmaladze martingale transformation. Comp. Stat., 35(4): 1993-2017
#'@export
#'@importFrom Rcpp evalCpp
#'@useDynLib GofKmt



Run_KMT = function(X, strDistr="Normal", bEstimation=FALSE, bFast_Estimation=FALSE, bParallel=FALSE, nThreads=16){

  
  # tmp_txt = paste0("SMat = ", strDistr, "_table$SMat")
  # eval(parse(text=tmp_txt))
  # 
  # tmp_txt = paste0("xVec =", strDistr, "_table$xVec")
  # eval(parse(text=tmp_txt))
  # 
  
  if(strDistr=="Normal"){
    tbl = Normal_table
  }else if(strDistr=="Logistic"){
    tbl = Logistic_table
  }else if(strDistr=="Cauchy"){
    tbl = Cauchy_table
  }
  
  SMat = tbl$SMat
  xVec = tbl$xVec
  
  if(strDistr=="Logistic"){
    #tmp_txt = paste0("ReVec = ", strDistr, "_table$ReVec")
    #eval(parse(text=tmp_txt))

    ReVec = tbl$ReVec
  }else{
    ReVec = c(1)
  }

  if(strDistr=="Gumbel"){

    #tmp_txt = paste0("VMat = ", strDistr, "_table$VMat")
    #eval(parse(text=tmp_txt))

    VMat = tbl$VMat
  }else{
    VMat = matrix(1, 3,3)
  }


  if(bEstimation==TRUE){
    lst = SortX(X, strDistr, bFast_Estimation)
    Sorted_X = lst$Sorted_X
    muhat = lst$muhat
    sighat = lst$sigmahat

  }else{
    Sorted_X = sort(X)
    muhat = 0
    sighat = 1
    
  }



  ans = KMT_beta(strDistr, SMat, VMat, ReVec, xVec, Sorted_X, bParallel=bParallel, nThreads=nThreads)

  Optimal_x = ans[1]
  Optimal_y = ans[2]
   
  
  lst = list(opt_x = Optimal_x, test_stat = Optimal_y, mu=muhat, sigma=sighat)
  
  return(lst)

}

