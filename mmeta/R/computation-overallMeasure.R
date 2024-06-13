
#### Add location of math formula here



overallMeasureCal <- function(parm, hessian_log , measure = measure, alpha = alpha ){
  a1 <- parm[1] 
  b1 <- parm[2]
  a2 <- parm[3] 
  b2 <- parm[4]
  ### model depends on the dim of hessian
  if (measure=="OR") ## hessian is in log scale
    return(overallOR(a1, b1, a2, b2, hessian_log,alpha=alpha))
  if (measure=="RR")
    return(overallRR(a1, b1, a2, b2, hessian_log,alpha=alpha))
  if (measure=="RD")
    return(overallRD(a1, b1, a2, b2, hessian_log,alpha=alpha))
}



#############################################################################################
### Purpose: Compute point estiamte and wald CI of the overall OR
### Input:   MLE(a1,a2,b1,b2) in log scale, hessian matrix(in log scale), significance level
### Output:  list contains point estimate and wald CI in original scale
### Note:    This function is used in wrapper function "overall". Variance is computated
###          via Delta method.
### Author:  Sheng Luo, Yong Chen, Xiao Su, Haitao Chu
### Data:    7/13/2012
############################################################################################# 
overallOR <- function(a1, b1, a2, b2, hessian_log, alpha=alpha) {
  logOR <- log((a2/b2)/(a1/b1))
  covmat_logOR <- inverseMatrixFunc(-hessian_log)  
  
  if (nrow(hessian_log)== NUM_PARAMETERS_INDEPENDENT) myD <- matrix(c(-1, 1, 1, -1), nrow=1)  else
    myD <- matrix(c(-1, 1, 1, -1, 0), nrow=1)
  #
  logOR_Var <- as.numeric(myD %*% covmat_logOR %*% t(myD))
  logOR_left <- logOR-qnorm(1-alpha/2)*sqrt(logOR_Var)
  logOR_right <- logOR +qnorm(1-alpha/2)*sqrt(logOR_Var)
  return(list(overall=exp(logOR),
              CI=exp(c(logOR_left,logOR_right))))
}                                                     

#############################################################################################
### Purpose: Compute point estiamte and wald CI of the overall RR
### Input:   MLE(a1,a2,b1,b2) in log scale, hessian matrix(in log scale), significance level
### Output:  list contains point estimate and wald CI in original scale
### Note:    This function is used in wrapper function "overall". Variance is computated
###          via Delta method.
### Author:  Sheng Luo, Yong Chen, Xiao Su, Haitao Chu
### Data:    7/13/2012
#############################################################################################
overallRR <- function(a1, b1, a2, b2, hessian_log,alpha=alpha) {
  logRR <- log((a2/(a2+b2))/(a1/(a1+b1)))
  covmat_logRR <- inverseMatrixFunc(-hessian_log)
  if (nrow(hessian_log)== NUM_PARAMETERS_INDEPENDENT)
    myD <- matrix(c(-b1/(a1+b1), b1/(a1+b1), b2/(a2+b2), -b2/(a2+b2)), nrow=1)
  if (nrow(hessian_log)==NUM_PARAMETERS_SARMANOV)    
    myD <- matrix(c(-b1/(a1+b1), b1/(a1+b1), b2/(a2+b2), -b2/(a2+b2), 0), nrow=1)  
  
  logRR_Var <- as.numeric(myD %*% covmat_logRR %*% t(myD))
  logRR_left<- logRR - qnorm(1-alpha/2)*sqrt(logRR_Var)                       
  logRR_right <- logRR + qnorm(1-alpha/2)*sqrt(logRR_Var)                      
  return(list(overall=exp(logRR_Var), CI=exp(c(logRR_left,logRR_right))))
}

#############################################################################################
### Purpose: Compute point estiamte and wald CI of the overall RD
### Input:   MLE(a1,a2,b1,b2) in log scale, hessian matrix(in log scale), significance level
### Output:  list contains point estimate and wald CI in original scale
### Note:    This function is used in wrapper function "overall". Variance is computated
###          via Delta method.
### Author:  Sheng Luo, Yong Chen, Xiao Su, Haitao Chu
### Data:    7/13/2012
#############################################################################################
overallRD <- function(a1, b1, a2, b2, hessian_log,alpha=alpha) {
  myRD <- a2/(a2+b2)-a1/(a1+b1) 
  myVar <- inverseMatrixFunc(-hessian_log)
  if (nrow(hessian_log)==NUM_PARAMETERS_INDEPENDENT)
    myD <- matrix(c(-a1*b1/(a1+b1)^2, a1*b1/(a1+b1)^2, a2*b2/(a2+b2)^2,
                    -a2*b2/(a2+b2)^2), nrow=1)
  else myD <- matrix(c(-a1*b1/(a1+b1)^2, a1*b1/(a1+b1)^2, a2*b2/(a2+b2)^2,
                       -a2*b2/(a2+b2)^2, 0), nrow=1)
  myRD_Var <- as.numeric(myD %*% myVar %*% t(myD))
  myRD_left <- max(myRD-qnorm(1-alpha/2)*sqrt(myRD_Var), -1)
  myRD_right<- min(myRD+qnorm(1-alpha/2)*sqrt(myRD_Var), 1)
  # output of RD is in log scale
  return(list(overall=myRD, CI=c(myRD_left,myRD_right)))  
}





