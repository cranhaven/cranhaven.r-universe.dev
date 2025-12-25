
#' @title Calc_Cbeta
#' @description  Computing cumulative beta from beta (the magnitude of the jumps of the coefficient function)
#' 
#' @param beta The dataframe storing the magnitude of the jumps beta of the coefficients of the additive hazard.  The first column should be the survival time.
#' @return Return the cumulative beta by the giving beta.
#' @examples 
#' beta  = data.frame(time = c(1,2,3,4), beta = c(5,6,7,8))
#' cbeta = Calc_Cbeta(beta)





Calc_Cbeta <- function(beta){ # beta = data.frame(nrow = Sample_Size + 1,ncol = p +2), names(beta) = c("time","X_1","X_(p+1)")
  # prepare to restore cumulative beta
  
  CP_nrow = nrow(beta)-1
  p = ncol(beta)-1-1
  Cbeta = matrix(0, nrow = CP_nrow + 1, ncol = p+2)
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

