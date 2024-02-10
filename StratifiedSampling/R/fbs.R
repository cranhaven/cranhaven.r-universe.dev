#' @title  Fast Balanced Sampling
#' @name fbs
#' @description 
#' 
#' This function implements the method proposed by Hasler and Tillé (2014). It should be used for selecting a sample from highly stratified population.
#'
#' @param X A matrix of size (\eqn{N} x \eqn{p}) of auxiliary variables on which the sample must be balanced.
#' @param strata A vector of integers that specifies the stratification.
#' @param pik A vector of inclusion probabilities.
#'
#' @details 
#' Firstly a flight phase is performed on each strata. Secondly, several flight phases are applied by adding one by one the stratum. By doing this, some strata are managed on-the-fly. Finally, a landing phase is applied by suppression of the variables. If the number of element selected in each stratum is not equal to an integer, the function can be very time-consuming.
#'
#' @return A vector with elements equal to 0 or 1. The value 1 indicates that the unit is selected while the value 0 is for rejected units.
#'
#' @references 
#' Hasler, C. and Tillé Y. (2014). Fast balanced sampling for highly stratified population. \emph{Computational Statistics and Data Analysis}, 74, 81-94
#' 
#' @author Raphaël Jauslin \email{raphael.jauslin@@unine.ch}
#' 
#' @examples
#' 
#' N <- 100
#' n <- 10
#' x1 <- rgamma(N,4,25)
#' x2 <- rgamma(N,4,25)
#'
#' strata <- rep(1:n,each = N/n)
#'
#' pik <- rep(n/N,N)
#' X <- as.matrix(cbind(matrix(c(x1,x2),ncol = 2)))
#' 
#' s <- fbs(X,strata,pik)
#' 
#' t(X/pik)%*%s
#' t(X/pik)%*%pik
#' 
#' Xcat <- disj(strata)
#' 
#' t(Xcat)%*%s
#' t(Xcat)%*%pik
#' 
#'

#' @export
fbs <- function(X,strata,pik){
  
  H <- as.numeric(ncat(as.matrix(strata)))
  pik_tmp <- pik
  EPS <- 1e-8
  
  ##----------------------------------------------------------------
  ##                  Flightphase on each strata                   -
  ##----------------------------------------------------------------
  
  
  for(k in 1:H){
    # pik_tmp[strata == k] <- sampling::fastflightcube(cbind(pik[which(strata == k)],as.matrix(X[which(strata == k),])),
    #                                                pik[strata == k],
    #                                                comment = FALSE)
    pik_tmp[strata == k] <- ffphase(as.matrix(cbind(pik[which(strata == k)],as.matrix(X[which(strata == k),]))),
                                                   pik[strata == k])
    
  }
  sum(pik_tmp)
  
  ##----------------------------------------------------------------
  ##          Flightphase on the uninon of strata U1 -- Uk         -
  ##----------------------------------------------------------------
  i <- which(pik_tmp > EPS & pik_tmp < (1-EPS))
  if(length(i) != 0){
    Xnn <- disj(strata)
    for(k in 1:H){
      # print(k)
      
      i <- which(strata <= k & (pik_tmp > EPS & pik_tmp < (1-EPS)))
      
      # strata_tmp2 <- Xnn[i,1:k]
      # strata_tmp2 <- disjMatrix(as.matrix(strata[i]))
      
      strata_tmp2 <- disj(strata[i])
      strata_tmp2 <- strata_tmp2*pik_tmp[i]
      
      X_tmp <- as.matrix((X[i,]*pik_tmp[i]/pik[i]))
      
      # pik_tmp[i] <- sampling::fastflightcube(as.matrix(cbind(X_tmp,strata_tmp2)),
      #                                        pik_tmp[i],
      #                                        1,
      #                                        comment = FALSE)
      if(length(i) > 1){
        pik_tmp[i] <- ffphase(as.matrix(cbind(X_tmp,strata_tmp2)),
                              pik_tmp[i]) 
      }
    
    }
  }
  sum(pik_tmp)

  ##---------------------------------------------------------------
  ##              Landing by suppression of variables             -
  ##---------------------------------------------------------------
  
  i <- which(pik_tmp > EPS & pik_tmp < (1-EPS))

  
  if(length(i) != 0){
    strata_tmp3 <- as.matrix(Xnn)
    pik_tmp <- landingRM(as.matrix(cbind(pik_tmp,strata_tmp3, X/pik)),
                          pik_tmp)
  # pik_tmp[i] <- landingRM(as.matrix(cbind(Xnn[i,], X[i,])),
  #                         pik_tmp[i],
  #                         pik[i])
  
  }
  return(round(pik_tmp,10))
}
