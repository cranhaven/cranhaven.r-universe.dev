# This file contains some useful functions 


# To calculate the cumulative beta from beta


Calc_Cbeta <- function(beta){ 
  
  # beta = data.frame(nrow = Sample_Size + 1,ncol = p +2), names(beta) = c("time","X_1",...,"X_(p+1)")
  # prepare to restore cumulative beta
  
  p = ncol(beta) - 2
  CP_nrow = nrow(beta)-1
  Cbeta = matrix(0, nrow = nrow(beta), ncol = ncol(beta))
  Cbeta = as.data.frame(Cbeta)
  names(Cbeta) = names(beta)
  Cbeta[1,] = 0
  Cbeta[,1] = beta[,1]
  
  # computing cumulative beta
  
  for (i in (1:CP_nrow)){
    for (j in (1:(p+1))){
      #Cbeta[i+1,j+1] = Cbeta[i,j+1] + (Cbeta[i+1,1]-Cbeta[i,1])*beta[i+1,j+1]
      Cbeta[i+1,j+1] = Cbeta[i,j+1] + beta[i+1,j+1]
    }
  }
  return(Cbeta)
}

# Calculate the survival rate

Calc_SurvR <- function(subject,beta){ 
  
  # subject = p+1 - vector
  # beta = as.data.frame(matrix(nrow = Sample_Size + 1,ncol =1+p+1)), the first column is time, others are coefficients
  
  # settings of the cumulative beta
  
  Cbeta = Calc_Cbeta(beta)
  p = ncol(beta)-2
  CP_nrow = nrow(beta)-1
  time_grid = beta[,1]
  #compute Mostly likelihood
  
  SurvR = matrix(0, nrow = CP_nrow + 1, 2)
  SurvR[1,] = 1
  SurvR[,1] = time_grid
  
  for (i in (1:CP_nrow)){
    SurvR[i+1,2] = exp(-sum(Cbeta[i+1,(2:(p+2))] * subject))
  }
  SurvR = as.data.frame(SurvR)
  names(SurvR) = c("time","SurvivalRate")
  
  return(SurvR) 
}
