






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





