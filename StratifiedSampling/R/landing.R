#' @title Landing by suppression of variables
#' @name landingRM
#' @param A matrix of auxiliary variables on which the sample must be balanced. (The matrix should be divided by the original inclusion probabilities.)
#' @param pikstar vector of updated inclusion probabilities by the flight phase. See \code{\link{ffphase}}
#' @param EPS epsilon value
#'
#' @return A vector with elements equal to 0 or 1. The value 1 indicates that the unit is selected while the value 0 is for rejected units.
#' 
#' @description
#' This function performs the landing phase of the cube method using suppression of variables proposed by Chauvet and Tillé (2006).
#'
#' @author Raphaël Jauslin \email{raphael.jauslin@@unine.ch}
#'
#' @seealso \code{\link{fbs}}, \code{\link{balstrat}}.
#' 
#' @references
#' Chauvet, G. and Tillé, Y. (2006). A fast algorithm of balanced sampling. \emph{Computational Statistics}, 21/1:53-62
#'
#' @examples
#' N <- 1000
#' n <- 10
#' p <- 4
#' pik <- rep(n/N,N)
#' X <- cbind(pik,matrix(rgamma(N*p,4,25),ncol= p))
#' pikstar <- ffphase(X,pik) 
#' s <- landingRM(X/pik*pikstar,pikstar)
#' sum(s)
#' t(X/pik)%*%pik
#' t(X/pik)%*%pikstar
#' t(X/pik)%*%s
#' @export
landingRM <- function(A,pikstar,EPS = 1e-7){


  ##----------------------------------------------------------------
  ##                          Initializing                         -
  ##----------------------------------------------------------------

  EPS = 1e-6
  N = nrow(A)
  i = which(pikstar > EPS & pikstar < (1 - EPS))
  i_size = length(i)
  
  pikland <- pikstar[i]

  Aland <- A[i,]
  Nland = length(pikland)
  nland = sum(pikland)
  p <- ncol(Aland)
  
  
  j <-  which(pikland > EPS & pikland < (1 - EPS))
  j_size <- length(j)
  

  ##---------------------------------------------------------------
  ##                          Main loop                           -
  ##---------------------------------------------------------------
  
  
  ####### COMMENT TO CHECK THAT LANDING WORKS
  # print(Aland[j,]/pikland[j]) # should have 1 on the first columns 
  # print(Aland)
  for(k in 0:(p-1)){
    
    # Bland <- Aland[j,]*pikland[j] # ffphase need X instead of A so why * by pikland
    Bland <- Aland[j,] # ffphase need X instead of A so why * by pikland
    Bland <- Bland[,1:(p-k)]
    kern <- MASS::Null(Bland)
    if(length(kern) != 0){
      pik_tmp <- pikland[j] # keep old pik to update A 
      
      ####### COMMENT TO CHECK THAT LANDING WORKS 
      # print(sum(pik_tmp))
      # print(Bland/pikland[j])
      
      pikland[j] <- ffphase(as.matrix(Bland),pikland[j]) # need X 
      Aland[j,] <- (Aland[j,]/pik_tmp)*pikland[j] # update A
      j = which(pikland > EPS & pikland < (1 - EPS)) # new j
      
      ####### COMMENT TO CHECK THAT LANDING WORKS
      # print(sum(pikland[j]))
      
      # break if no longer unit that need to be put equal to 0 or 1
      if(length(j) < EPS){
        break;
      }
    }
  }
  
  pikstar[i] = pikland
  i <- which(pikstar > EPS & pikstar < (1 - EPS))
  
  # cat("pikstar after dropping variables:", pikstar[i],"\n")
  # cat("sum pikstar after dropping variables:", sum(pikstar[i]),"\n")
  
  if(length(i) != 0){
    stop("error you still have, after landing, at least one unit that have inlcusion probability not equal to 0 or 1. Check that you have put the vector of inclusion probabilities as first column on the auxiliary variables.")
  }
  
  return(round(pikstar,6))
}

