


###### For other test: KS, AD, CvM etc

Fnx = function(ZVec){
  
  n = length(ZVec)
  dual = function(x){
    out = 0
    for(i in 1:n){
      Zi = ZVec[i]
      if(Zi<=x){
        out = out+1
      }else{
        break
      }
      
    }
    return(out/n)
  }
  
  return(dual)
}







KS_Dn = function(Z, Distr){
  
  n = length(Z)
  
  
  Vec = rep(0, times=(2*n))
  del1=0.00001
  
  
  for(i in 1:(2*n)){
    
    if(i%%2!=0){
      Zi = Z[(i+1)/2]-del1
    }else{
      Zi = Z[(i/2)]
    }
    
    #print("Zi is")
    #print(Zi)
    
    FnVal = Fnx(Z)(Zi)
    if(Distr=="Normal"){
      Fx = pnorm(Zi, 0 ,1)
      
    }else if(Distr=="Logistic"){
      Fx = plogis(Zi, 0 ,1)
      
    }else if(Distr=="Cauchy"){
      Fx = pcauchy(Zi, 0 ,1)
    }else if(Distr=="Gumbel"){
      Fx = pgumbel(Zi, 0 ,1)
    }
    Vec[i] = abs(FnVal-Fx)
  }
  
  return(max(Vec))
  
}

#' Run other GOF tests
#' 
#' Run the Kolmogorov-Smirnov (KS) and Kuiper tests.
#'@param Z a normalized random sample of n observations.
#'@param Distr a null distribution of the GOF test. It should be one of "Normal", "Logistic", "Cauchy", or "Gumbel

#'@return A vector of the KS and Kuiper test statistics.
#'
#'
#'@examples
#' 
#' n=10
#' mu=2
#' sigma=1
#' 
#' X = rnorm(n, mu, sigma)
#' 
#' muhat = mean(X)
#' sighat = sd(X)
#' 
#' Z = (X-muhat)/sighat 
#' null_distr="Normal"
#' Vec = KS(Z, null_distr)
#' KS_teststat = Vec[1]
#' Kuiper_teststat = Vec[2]
#' 
#' KS_teststat;Kuiper_teststat
#'
#'
#'@export 



KS = function(Z, Distr){
  
  n = length(Z)
  DplusVec = rep(0, times=n)
  DminusVec = rep(0, times=n)
  
  for(k in 1:n){
    Zk = Z[k]
    
    if(Distr=="Normal"){
      dp = k/n-pnorm(Zk, 0, 1)
      dm = pnorm(Zk, 0, 1) - (k-1)/n
    }else if(Distr=="Logistic"){
      dp = k/n-plogis(Zk, 0, 1)
      dm = plogis(Zk, 0, 1) - (k-1)/n
    }else if(Distr=="Cauchy"){
      dp = k/n-pcauchy(Zk, 0, 1)
      dm = pcauchy(Zk, 0, 1) - (k-1)/n
    }else if(Distr=="Gumbel"){
      dp = k/n-pgumbel(Zk, 0, 1)
      dm = pgumbel(Zk, 0, 1) - (k-1)/n
    }
    
    DplusVec[k] = dp
    DminusVec[k] = dm
    
  }
  
  Dplus = max(DplusVec)
  Dminus = max(DminusVec)
  
  val1 = Dplus+Dminus
  val2 = KS_Dn(Z, Distr)
  
  cVec = c(val1, val2)
  return( cVec)
}


#' Run other GOF tests
#' 
#' Run the Cramer-von Mises (CvM) and Watson tests.
#'@param Z a normalized random sample of n observations.
#'@param Distr a null distribution of the GOF test. It should be one of "Normal", "Logistic", "Cauchy", or "Gumbel

#'@return A vector of the CvM and Watson test statistics.
#'
#'
#'@examples
#' 
#' n=10
#' mu=2
#' sigma=1
#' 
#' X = rnorm(n, mu, sigma)
#' 
#' muhat = mean(X)
#' sighat = sd(X)
#' 
#' Z = (X-muhat)/sighat 
#' null_distr="Normal"
#' Vec = CvM(Z, null_distr)
#' CvM_teststat = Vec[1]
#' Watson_teststat = Vec[2]
#' 
#' CvM_teststat;Watson_teststat
#'
#'
#'@export 

CvM = function(Z, Distr){
  
  n = length(Z)
  
  out = 0
  out2 = 0
  for(k in 1:n){
    Zk = Z[k]
    
    if(Distr=="Normal"){
      out = out + ( pnorm(Zk, 0, 1) - (2*k-1)/(2*n) )^2
      out2 = out2+pnorm(Zk, 0, 1)
    }else if(Distr=="Logistic"){
      out = out + ( plogis(Zk, 0, 1) - (2*k-1)/(2*n) )^2
      out2 = out2+plogis(Zk, 0, 1)
    }else if(Distr=="Gumbel"){
      out = out + ( pgumbel(Zk, 0, 1) - (2*k-1)/(2*n) )^2
      out2 = out2+pgumbel(Zk, 0, 1)
      
    }else if(Distr=="Cauchy"){
      out = out + ( pcauchy(Zk, 0, 1) - (2*k-1)/(2*n) )^2
      out2 = out2+pcauchy(Zk, 0, 1)
      
    }
    
  }
  out = out+1/(12*n)
  out2 = out - (out2/n-0.5)^2*n   ##Watson
  
  ans = c(out, out2)
  return(ans)
}

#' Run other GOF tests
#' 
#' Run the Anderson-Darling (AD) test.
#'@param Z a normalized random sample of n observations.
#'@param Distr a null distribution of the GOF test. It should be one of "Normal", "Logistic", "Cauchy", or "Gumbel

#'@return A test statistic of the AD test.
#'
#'
#'@examples
#' 
#' n=10
#' mu=2
#' sigma=1
#' 
#' X = rnorm(n, mu, sigma)
#' 
#' muhat = mean(X)
#' sighat = sd(X)
#' 
#' Z = (X-muhat)/sighat 
#' null_distr="Normal"
#' AD_teststat = AD(Z, null_distr)
#' AD_teststat
#'@export 


AD = function(Z, Distr){
  
  n = length(Z)
  
  out = 0
  
  del1=10^(-5)
  for(k in 1:n){
    Zk = Z[k]
    Zk2 = Z[n+1-k]
    #out = out + (2*k-1)*log(plogis(Zk,0,1) ) + (2*n-2*k+1)*log(1 - plogis(Zk,0,1) )
    
    if(Distr=="Normal"){
      out = out + (2*k-1)* ( log(pnorm(Zk,0,1)+del1 ) +  log(1 - pnorm(Zk2,0,1) ) +del1  )
      
    }else if(Distr=="Logistic"){
      out = out + (2*k-1)* ( log(plogis(Zk,0,1)+del1 ) +  log(1 - plogis(Zk2,0,1) ) +del1  )
      
    }else if(Distr=="Gumbel"){
      out = out + (2*k-1)* ( log(pgumbel(Zk,0,1)+del1 ) +  log(1 - pgumbel(Zk2,0,1) )  +del1 )
      
    }else if(Distr=="Cauchy"){
      out = out + (2*k-1)* ( log(pcauchy(Zk,0,1)+del1 ) +  log(1 - pcauchy(Zk2,0,1) )  +del1 )
      
    }
    
  }
  
  out = -n-out/n
  return(out)
  
}


########################