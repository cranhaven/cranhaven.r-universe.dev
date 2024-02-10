#' @title Stratified Sampling
#' @name stratifiedcube
#' @description  
#' 
#' This function implements a method for selecting a stratified sample. It really improves the performance of the function \code{\link{fbs}} and \code{\link{balstrat}}.
#'
#' @param X A matrix of size (\eqn{N} x \eqn{p}) of auxiliary variables on which the sample must be balanced.
#' @param strata A vector of integers that specifies the stratification..
#' @param pik A vector of inclusion probabilities.
#' @param EPS epsilon value
#'
#' @details 
#' 
#' The function is selecting a balanced sample very quickly even if the sum of inclusion probabilities within strata are non-integer. The function should be used in preference. Firstly, a flight phase is performed on each strata. Secondly, the function \code{\link{findB}} is used to find a particular matrix to apply a flight phase by using the cube method proposed by Chauvet, G. and Tillé, Y. (2006). Finally, a landing phase is applied by suppression of variables.
#' 
#' @seealso \code{\link{fbs}}, \code{\link{balstrat}}, \code{\link{landingRM}}, \code{\link{ffphase}}
#'
#' @return A vector with elements equal to 0 or 1. The value 1 indicates that the unit is selected while the value 0 is for rejected units.
#' @export
#'
#' @references 
#' Chauvet, G. and Tillé, Y. (2006). A fast algorithm of balanced sampling. \emph{Computational Statistics}, 21/1:53-62
#'
#' @examples
#' N <- 100
#' n <- 10
#' p <- 4
#' X <- matrix(rgamma(N*p,4,25),ncol = p)
#' strata <- as.matrix(rep(1:n,each = N/n))
#' pik <- rep(n/N,N)
#' 
#' s <- stratifiedcube(X,strata,pik)
#' 
#' t(X/pik)%*%s
#' t(X/pik)%*%pik
#' 
#' Xcat <- disj(strata)
#' 
#' t(Xcat)%*%s
#' t(Xcat)%*%pik
#' 
stratifiedcube <- function(X,strata,pik,EPS = 1e-7){
  
  ##----------------------------------------------------------------
  ##                        Initialization                         -
  ##----------------------------------------------------------------
  strata <- as.matrix(strata)
  A = X/pik
  pikstar <- pik
  p = ncol(X)
  nstrata <- length(unique(strata))
  
  ##----------------------------------------------------------------
  ##                  Flightphase on each strata                   -
  ##----------------------------------------------------------------
  
  for(k in 1:nstrata){
    pikstar[strata == k] <-ffphase(as.matrix(cbind(pikstar[strata == k],X[which(strata == k),])),
                                   pikstar[strata == k])
  }
  
  ###################### CHECK
  # t(X/pik)%*%pikstar
  # t(X/pik)%*%pik
  # t(Xcat)%*%pik
  # t(Xcat)%*%pikstar
  
  ##----------------------------------------------------------------
  ##                Number of non 0-1 inclusion prob               -
  ##----------------------------------------------------------------
  
  i <- which(pikstar > EPS & pikstar < (1-EPS))
  i_size = length(i)
  i_size
  
  
  ##----------------------------------------------------------------
  ##            flight phase with categorical variable             -
  ##----------------------------------------------------------------
  
  if(i_size > 0 ){
    while(i_size > 0){
    
      ##------ Remove unique category
      
      uCat <- i[duplicated(strata[i,]) | duplicated(strata[i,], fromLast = TRUE)]
      if(length(uCat) == 0){
        break;
      }
      
      ##------ Find B
      A_tmp <- as.matrix(X[uCat,]/pik[uCat])
      B <- findB(A_tmp,as.matrix(strata[uCat,]))
      B <- cbind(B$X,B$Xcat)
      ##------ onestep and check if null
      tmp <-  onestep(B,pikstar[uCat[1:nrow(B)]],EPS)
      if(is.null(tmp)){
        break;
      }else{
        pikstar[uCat[1:nrow(B)]] <- tmp
      }
      
      ##------ update i
      
      i <- which(pikstar > EPS & pikstar < (1-EPS))
      i_size = length(i)
      # print(sum(pikstar))
    }
    
    
    # ##----------------------------------------------------------------
    # ##          end of flight phase on strata categories             -
    # ##----------------------------------------------------------------
    # Sometimes some stata does could not be balanced at the end and so we 
    # drop some auxiliary variable such that we could have only one unit that
    # are not put equal to 0 or 1 within each strata
    #
    
    p <- ncol(X)
    k = 1
    while(length(uCat) != 0){
      ##------ Find B
      if(k == p){
        B <- as.matrix(pikstar[uCat])/pikstar[uCat]
      }else{
        A_tmp <- as.matrix(as.matrix(X[uCat,1:(p-k)])/pik[uCat])
        B <- findB(A_tmp,as.matrix(strata[uCat,]))
        B <- cbind(B$X,B$Xcat)
      }
      
      # print(pikstar[uCat[1:nrow(B)]])
      tmp <-  onestep(B,pikstar[uCat[1:nrow(B)]],EPS)
      # print(tmp)
      if(!is.null(tmp)){
        pikstar[uCat[1:nrow(B)]] <- tmp  
      }
      i <- which(pikstar > EPS & pikstar < (1-EPS))
      i_size = length(i)
      uCat <- i[duplicated(strata[i,]) | duplicated(strata[i,], fromLast = TRUE)]
      k = k + 1
    }
  }
  
  #---------------- check
  # Xcat <- disjMatrix(strata)
  # 
  # print(t(X/pik)%*%pik)
  # print(t(X/pik)%*%pikstar)
  # print(t(Xcat)%*%pik)
  # print(t(Xcat)%*%pikstar)
  # print(length(i))
  # print(sum(pikstar))
   
  
  # ##----------------------------------------------------------------
  # ##            Landing on unit that are alone in the strata       -
  # ##----------------------------------------------------------------
  
  
  
  if(length(i) > 0){
    pikstar[i] <- landingRM(cbind(pikstar[i],X[i,]/pik[i]),pikstar[i],EPS)    
  }
  
  
  return(round(pikstar,10))
}

