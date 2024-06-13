
################################################################################################
### Purpose: compute kth moment of OR/RR for independent/Sarmanov model: Wrapper function
### Input:   k, hyperparemeters(a1,b1,a2,b2,rho),measure,model
### Output:  the kth moment
### Note:    Implemented by "moment.OR.inde","moment.OR.sar","moment.RR.inde","moment.RR.sar "
### Author:  Sheng Luo, Yong Chen, Xiao Su, Haitao Chu
### Data:    7/13/2012
################################################################################################ 
kthMomentExact <- function(parms_prior = parms_prior, data = data ,measure=measure, model=model, kth = kth){
  a1 <- parms_prior$a1
  b1 <- parms_prior$b1
  a2 <- parms_prior$a2
  b2 <- parms_prior$b2
  rho <- parms_prior$rho
  y1 <- data$y1
  n1 <- data$n1
  y2 <- data$y2
  n2 <- data$n2

  if(model=="Independent") {
    if (measure=="OR")  result <- kthMomentORInde(kth = kth, 
                                                 a1 =a1, b1 = b1, a2 = a2, b2 = b2,
                                                 y1 = y1 , n1 = n1, y2=y2, n2=n2)
    if (measure=="RR")  result <- kthMomentRRInde(kth = kth, 
                                                 a1 =a1, b1 = b1, a2 = a2, b2 = b2,
                                                 y1 = y1 , n1 = n1, y2=y2, n2=n2)
  }
  if (model=="Sarmanov") {
    if (measure=="OR")  result <- kthMomentORSar(kth = kth, 
                                                a1 =a1, b1 = b1, a2 = a2, b2 = b2,
                                                y1 = y1 , n1 = n1, y2=y2, n2=n2, 
                                                rho = rho)
    if (measure=="RR")  result <- kthMomentRRSar(kth = kth, 
                                                a1 =a1, b1 = b1, a2 = a2, b2 = b2,
                                                y1 = y1 , n1 = n1, y2=y2, n2=n2, 
                                                rho = rho)
  }
  return(result)
}


###################################################################################
### Purpose: compute kth moment of OR for independent model
### Input:   k, hyperparemeters(a1, b1, a2, b2)
### Output:  the kth moment
### Author:  Sheng Luo, Yong Chen, Xiao Su, Haitao Chu
### Data:    7/13/2012
################################################################################### 
kthMomentORInde <- function(kth = kth, 
                            a1 =a1, b1 = b1, a2 = a2, b2 = b2,
                            y1 = y1 , n1 = n1, y2=y2, n2=n2) {
  alpha1 <- y1+a1; beta1 <- n1-y1+b1
  alpha2 <- y2+a2; beta2 <- n2-y2+b2  
  mylog <- ((lgamma(alpha1-kth)+lgamma(beta1+kth)+lgamma(alpha2+kth)+lgamma(beta2-kth))
            -(lgamma(alpha1)+lgamma(beta1)+lgamma(alpha2)+lgamma(beta2)))
  return(exp(mylog))
}

###################################################################################
### Purpose: compute kth moment of OR for Sarmanov model
### Input:   k, hyperparemeters(a1,b1,a2,b2)
### Output:  the kth moment
### Author:  Sheng Luo, Yong Chen, Xiao Su, Haitao Chu
### Data:    7/13/2012
################################################################################### 
kthMomentORSar <- function(kth = kth, 
                           a1 =a1, b1 = b1, a2 = a2, b2 = b2,
                           y1 = y1 , n1 = n1, y2=y2, n2=n2, 
                           rho = rho) {
  myOmega <- omegaCal(y1, n1, y2, n2, a1, b1, a2, b2, rho)
  #alpha1 <- y1+a1; beta1 <- n1-y1+b1
  #alpha2 <- y2+a2; beta2 <- n2-y2+b2
  return(myOmega$omega1*kthMomentORInde(kth = kth, 
                                     a1 =a1, b1 = b1, a2 = a2, b2 = b2,
                                     y1 = y1 , n1 = n1, y2=y2, n2=n2)
         +myOmega$omega2*kthMomentORInde(kth = kth, 
                                      a1 =a1+1, b1 = b1, a2 = a2, b2 = b2,
                                      y1 = y1 , n1 = n1, y2=y2, n2=n2)
         +myOmega$omega3*kthMomentORInde(kth = kth, 
                                      a1 =a1, b1 = b1, a2 = a2+1, b2 = b2,
                                      y1 = y1 , n1 = n1, y2=y2, n2=n2)
         +myOmega$omega4*kthMomentORInde(kth = kth, 
                                      a1 =a1+1, b1 = b1, a2 = a2+1, b2 = b2,
                                      y1 = y1 , n1 = n1, y2=y2, n2=n2))
  
}

###################################################################################
### Purpose: compute kth moment of RR for independent model
### Input:   k, hyperparemeters(a1,b1,a2,b2)
### Output:  the kth moment
### Author:  Sheng Luo, Yong Chen, Xiao Su, Haitao Chu
### Data:    7/13/2012
################################################################################### 
kthMomentRRInde <- function(kth = kth, 
                            a1 =a1, b1 = b1, a2 = a2, b2 = b2,
                            y1 = y1 , n1 = n1, y2=y2, n2=n2) {
  alpha1 <- y1+a1; beta1 <- n1-y1+b1
  alpha2 <- y2+a2; beta2 <- n2-y2+b2  
  mylog <- ((lgamma(alpha1-kth)+lgamma(beta1)+lgamma(alpha2+kth)+lgamma(beta2))
            -(lgamma(alpha1)+lgamma(beta1)+lgamma(alpha2)+lgamma(beta2)))
  return(exp(mylog))
}                                  
###################################################################################
### Purpose: compute kth moment of RR for Sarmanov model
### Input:   k, hyperparemeters(a1,b1,a2,b2)
### Output:  the kth moment
### Author:  Sheng Luo, Yong Chen, Xiao Su, Haitao Chu
### Data:    7/13/2012
###################################################################################                                                             
kthMomentRRSar <- function(kth = kth, 
                           a1 =a1, b1 = b1, a2 = a2, b2 = b2,
                           y1 = y1 , n1 = n1, y2=y2, n2=n2, 
                           rho = rho){
  myOmega <- omegaCal(y1, n1, y2, n2, a1, b1, a2, b2, rho)
  return(myOmega$omega1*kthMomentRRInde(kth = kth, 
                                     a1 =a1, b1 = b1, a2 = a2, b2 = b2,
                                     y1 = y1 , n1 = n1, y2=y2, n2=n2)
         +myOmega$omega2*kthMomentRRInde(kth = kth, 
                                      a1 =a1 + 1, b1 = b1, a2 = a2, b2 = b2,
                                      y1 = y1 , n1 = n1, y2=y2, n2=n2)
         +myOmega$omega3*kthMomentRRInde(kth = kth, 
                                      a1 =a1, b1 = b1, a2 = a2 + 1, b2 = b2,
                                      y1 = y1 , n1 = n1, y2=y2, n2=n2)
         +myOmega$omega4*kthMomentRRInde(kth = kth, 
                                      a1 =a1 + 1, b1 = b1, a2 = a2 + 1, b2 = b2,
                                      y1 = y1 , n1 = n1, y2=y2, n2=n2))  
  

  
  
  }        