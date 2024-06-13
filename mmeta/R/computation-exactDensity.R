





densityMeasureGrids <- function(parms_prior = parms_prior, data = data, 
                                 model = model, measure = measure,
                                from = from, to = to, 
                                num_grids = num_grids){
  
  density_grids = rep(0, num_grids * 1.5 )
  theta_grids = seq(from = from, to = to, by = (to - from)/(num_grids * 1.5 ))
  

  for(i in 1: num_grids){
    density_grids[i] <- densityMeasure(theta = theta_grids[i],
                                     a1=parms_prior$a1,
                                     b1=parms_prior$b1,
                                     a2=parms_prior$a2,
                                     b2=parms_prior$b2,
                                     rho=parms_prior$rho,
                                     y1=data$y1,
                                     y2=data$y2,
                                     n1=data$n1, 
                                     n2=data$n2, 
                                     measure=measure,
                                     model=model
                                     )
    
  }
  
  density_grids <- density_grids[!is.na(density_grids)]
  length_density_grids <- length(density_grids)
  theta_grids <- theta_grids[!is.na(density_grids)]
  length_theta_grids <- length(theta_grids)
  return(
    list(y = density_grids[1: min(num_grids, length_density_grids)],
         x = theta_grids[1:min(num_grids, length_density_grids)])
  )
}




###################################################################################
### Purpose: Calulate density given hyperparemeters and model
### input: hyperparameters(a1,b1,a2,b2), data(y1,n1,y2,n2), other(measure,model),
###        theta (comparative measure)
### Output: density(at theta)
### Note: the function uses functions  "dens.sar" and "dens.inde"
### Author:  Sheng Luo, Yong Chen, Xiao Su, Haitao Chu
### Data:    7/13/2012
###################################################################################
densityMeasure <- function(theta=theta,a1=a1,b1=b1,a2=a2,b2=b2,rho=rho,
                            y1=y1,y2=y2,n1=n1, n2=n2,
                            measure=measure,model=model) {
  if (model=="Sarmanov"){ 
    density_measure <- densitySarmanovMeasure(theta=theta,a1=a1,b1=b1,a2=a2,b2=b2,rho=rho,
                                       y1=y1,y2=y2,n1=n1,n2=n2,
                                       measure=measure)
  }
  if (model=="Independent"){
    density_measure <- densityIndendentMeasure(theta=theta,a1=a1,b1=b1,a2=a2,b2=b2,
                                              y1=y1,y2=y2,n1=n1,n2=n2,
                                              measure=measure) 
      
  }
         
  return(density_measure)
}




###################################################################################
### Purpose: compute the densty of RR/RR for independent model
### input: alpha1, beta1, alpha2, beta2, theta, meansure
### Output: the density at theta
### Note: this function is used by "OR.dens.inde","RR.dens.inde"
### Author:  Sheng Luo, Yong Chen, Xiao Su, Haitao Chu
### Data:    7/13/2012
###################################################################################
densityIndendentMeasure <- function(theta=theta,a1=a1,b1=b1,a2=a2,b2=b2,
                                    y1=y1,y2=y2,n1=n1,n2=n2,
                                    measure=measure) {
  alpha1 <- y1+a1; beta1 <- n1-y1+b1
  alpha2 <- y2+a2; beta2 <- n2-y2+b2
  if (measure=="OR")  result <- densityIndendentOR(alpha1,beta1,alpha2,beta2,theta)
  if (measure=="RR")  result <- densityIndendentRR(alpha1,beta1,alpha2,beta2,theta)
  if (measure=="RD")  result <- NA
  
  return(result)
}

###################################################################################
### Purpose: compute the densty of OR for independent model
### input: alpha1, beta1, alpha2, beta2, theta
### Output: the density at theta
### Author:  Sheng Luo, Yong Chen, Xiao Su, Haitao Chu
### Data:    7/13/2012
###################################################################################
densityIndendentOR <- function(alpha1, beta1, alpha2, beta2, theta) {
  if(theta>=1) {
    mylog <- suppressWarnings(
          ((-1-beta2)*log(theta)
              -(lgamma(alpha1)+lgamma(beta1)+lgamma(alpha2)+lgamma(beta2)+lgamma(alpha1+beta1+alpha2+beta2))
              +(lgamma(alpha1+beta1)+lgamma(alpha2+beta2)+lgamma(alpha1+alpha2)+lgamma(beta1+beta2))
              +log(hypergeoFun(alpha2+beta2, beta1+beta2, alpha1+alpha2+beta1+beta2, 1-(1/theta))))
    )
    result <- exp(mylog)
  }
  if(theta<1) {
    mylog <- suppressWarnings(
      ((-1+alpha2)*log(theta)
                -(lgamma(alpha1)+lgamma(beta1)+lgamma(alpha2)+lgamma(beta2)+lgamma(alpha1+beta1+alpha2+beta2))
                +(lgamma(alpha1+beta1)+lgamma(alpha2+beta2)+lgamma(alpha1+alpha2)+lgamma(beta1+beta2))
                +log(hypergeoFun(alpha2+beta2, alpha1+alpha2, alpha1+alpha2+beta1+beta2, 1-theta)))    
    )
    result <- exp(mylog)
  }
  
  if(!is.nan(result))
    return(result)
  else return(NA)
}

###################################################################################
### Purpose: compute the densty of RR for independent model
### input: alpha1, beta1, alpha2, beta2, theta
### Output: the density at theta
### Author:  Sheng Luo, Yong Chen, Xiao Su, Haitao Chu
### Data:    7/13/2012
###################################################################################
densityIndendentRR <- function(alpha1, beta1, alpha2, beta2, theta){            
  if(theta < 1){
    mylog <- suppressWarnings(
       ((-1+alpha2)*log(theta)
                -(lgamma(alpha1)+lgamma(beta1)+lgamma(alpha2)
                  +lgamma(beta2)+lgamma(alpha1+alpha2+beta1))
                +(lgamma(alpha1+beta1)+lgamma(alpha2+beta2)
                  +lgamma(alpha1+alpha2)+lgamma(beta1))
                +log(hypergeoFun(1-beta2,alpha1+alpha2,alpha1+alpha2+beta1,theta)))
    )
    result <- exp(mylog)
  }
  if(theta >= 1){
    mylog <- suppressWarnings(
          ((-1-alpha1)*log(theta)
                -(lgamma(alpha1)+lgamma(beta1)+lgamma(alpha2)
                  +lgamma(beta2)+lgamma(alpha1+alpha2+beta2))
                +(lgamma(alpha1+beta1)+lgamma(alpha2+beta2)
                  +lgamma(alpha1+alpha2)+lgamma(beta2))
                +log(hypergeoFun(1-beta1,alpha1+alpha2,alpha1+alpha2+beta2,1/theta)))
    )
    result <- exp(mylog)
  }
  if(!is.nan(result))
    return(result)
  else return(NA)
}

#########################################################################################
### Purpose: compute the densty of OR and RR under Sarmanov beta distribution
### input: hyperparameters (a1, b1, a2, b2, rho), data (y1, n1, y2, n2), measure
###        theta
### Output: the density at theta
### Note: The function uses functions "OR.dens.sar","RR.dens.sar"
### Author:  Sheng Luo, Yong Chen, Xiao Su, Haitao Chu
### Data:    7/13/2012
#########################################################################################
densitySarmanovMeasure <- function(theta=theta,a1=a1,b1=b1,a2=a2,b2=b2,rho=rho,
                     y1=y1,y2=y2,n1=n1,n2=n2,
                     measure=measure) {                                                     
  
  alpha1 <- y1+a1; beta1 <- n1-y1+b1
  alpha2 <- y2+a2; beta2 <- n2-y2+b2
  
  myOmega <- omegaCal(y1, n1, y2, n2, a1, b1, a2, b2, rho)
  omega1 <- myOmega$omega1
  omega2 <- myOmega$omega2
  omega3 <- myOmega$omega3
  omega4 <- myOmega$omega4
  if (measure=="OR")
    result <- densitySarmanovOR(alpha1,beta1,alpha2,beta2,theta,omega1,omega2,omega3,omega4)
  if (measure=="RR") 
    result <- densitySarmanovRR(alpha1,beta1,alpha2,beta2,theta,omega1,omega2,omega3,omega4)
  if (measure=="RD")  
    result <- NA
  return(result)
}



###################################################################################
### Purpose: compute the densty of OR under Sarmanov beta distribution
### input: alpha1, beta1, alpha2, beta2, theta, omega1, omega2, omega3, omega4
### Output: the density at theta
### Author:  Sheng Luo, Yong Chen, Xiao Su, Haitao Chu
### Data:    7/13/2012
###################################################################################
densitySarmanovOR <- function(alpha1,beta1,alpha2,beta2,theta,
                        omega1,omega2,omega3,omega4) {
  return(omega1*densityIndendentOR(alpha1,beta1,alpha2,beta2,theta)
         +omega2*densityIndendentOR(alpha1+1,beta1,alpha2,beta2,theta)
         +omega3*densityIndendentOR(alpha1,beta1,alpha2+1,beta2,theta)
         +omega4*densityIndendentOR(alpha1+1,beta1,alpha2+1,beta2,theta))                 
}

###################################################################################
### Purpose: compute the densty of RR under Sarmanov beta distribution
### input: alpha1, beta1, alpha2, beta2, theta, omega1, omega2, omega3, omega4
### Output: the density at theta
### Author:  Sheng Luo, Yong Chen, Xiao Su, Haitao Chu
### Data:    7/13/2012
###################################################################################
densitySarmanovRR <- function(alpha1,beta1,alpha2,beta2,theta,
                        omega1,omega2,omega3,omega4) {
  results <- (omega1*densityIndendentRR(alpha1,beta1,alpha2,beta2,theta)
              +omega2*densityIndendentRR(alpha1+1,beta1,alpha2,beta2,theta)            
              +omega3*densityIndendentRR( alpha1,beta1,alpha2+1,beta2,theta)            
              +omega4*densityIndendentRR(alpha1+1,beta1,alpha2+1,beta2,theta))
  return(results)
}










