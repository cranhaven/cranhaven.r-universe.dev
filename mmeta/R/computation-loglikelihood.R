

  
                                                                           
#### Add location of math formula here

##################################################################################################
### Purpose: Compute the log marginalized likelihood function from Sarmanov beta distribution 
### Input: 1)mypar: log(a1,b1,a2,b2), eta
###        2)mydata: n1,y1,y2,n2
### Output:  loglikilhood
### Author:  Sheng Luo, Yong Chen, Xiao Su, Haitao Chu
### Data:    7/13/2012
###################################################################################################
logLikSar <- function(parms_transformed, mydat) { 

  parms_original <- parmsToOriginal(parms_transformed)
  a1 <- parms_original[1]; 
  b1 <- parms_original[2]
  a2 <- parms_original[3]
  b2 <- parms_original[4]
  rho <- parms_original[5]
  
  # mu1, mu2: means
  mu1 <- a1/(a1+b1); mu2 <- a2/(a2+b2)  
  # delta1, delta2: standard deviations
  delta1 <- sqrt(mu1*(1-mu1)/(a1+b1+1))
  delta2 <- sqrt(mu2*(1-mu2)/(a2+b2+1))
  
  temp1 <- (lgamma(a1+mydat$y1) + lgamma(b1+mydat$n1-mydat$y1)
            + lgamma(a2+mydat$y2) + lgamma(b2+mydat$n2-mydat$y2)
            + lgamma(a1+b1) + lgamma(a2+b2))
  temp2 <- (lgamma(a1) + lgamma(b1) + lgamma(a2) + lgamma(b2)
            + lgamma(a1+b1+mydat$n1) + lgamma(a2+b2+mydat$n2))
  temp3 <- (log(1+rho/delta1/delta2
                *(mydat$y1-mydat$n1*mu1)
                *(mydat$y2-mydat$n2*mu2)
                /(a1+b1+mydat$n1)/(a2+b2+mydat$n2)))
  myLogLik <- sum(temp1) - sum(temp2) + sum(temp3)
  
  return(myLogLik)  
}

################################################################################################# 
### Purpose: Compute the log marginalized likelihood function from independent beta distribution
### Input: 1)mypar: log(a1,b1,a2,b2), eta
###        2)mydata: n1,y1,y2,n2
### Output:  loglikilhood
### Author:  Sheng Luo, Yong Chen, Xiao Su, Haitao Chu
### Data:    7/13/2012
##################################################################################################
logLikIndep <- function(parms_transformed, mydat) {

  parms_original <- parmsToOriginal(parms_transformed)
  a1 <- parms_original[1]; 
  b1 <- parms_original[2]
  a2 <- parms_original[3]
  b2 <- parms_original[4]
  
  ## reference 1: formula XXX
  temp1 <- (lgamma(a1+mydat$y1) + lgamma(b1+mydat$n1-mydat$y1)
            + lgamma(a2+mydat$y2) + lgamma(b2+mydat$n2-mydat$y2)
            + lgamma(a1+b1) + lgamma(a2+b2))
  temp2 <- (lgamma(a1) + lgamma(b1) + lgamma(a2) + lgamma(b2)
            + lgamma(a1+b1+mydat$n1) + lgamma(a2+b2+mydat$n2))
  
  myLogLik <- sum(temp1 - temp2)
  return(myLogLik)  
}





