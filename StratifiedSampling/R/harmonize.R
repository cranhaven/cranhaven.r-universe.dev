#' @title Harmonization by calibration
#' @name harmonize
#' @description 
#' This function harmonize the two weight schemes such that the totals are equal.
#'
#' @param X1 A matrix, the matching variables of sample 1.
#' @param d1 A numeric vector that contains the initial weights of the sample 1.
#' @param id1 A character or numeric vector that contains the labels of the units in sample 1.
#' @param X2 A matrix, the matching variables of sample 2.
#' @param d2 A numeric vector that contains the initial weights of the sample 1.
#' @param id2 A character or numeric vector that contains the labels of the units in sample 2.
#' @param totals An optional numeric vector that contains the totals of the matching variables.
#'
#' @details All details of the method can be seen in the manuscript: Raphaël Jauslin and Yves Tillé (2021) <arXiv:>.
#'
#' @return A list of two vectors, the new weights of sample 1 (respectively new weights of sample 2).
#' @export
#' 
#'
#' @examples
#' 
#' #--- SET UP
#' 
#' N = 1000
#' p = 5
#' X = array(rnorm(N*p),c(N,p))
#' 
#' n1=100
#' n2=200
#' 
#' s1 = sampling::srswor(n1,N)
#' s2 = sampling::srswor(n2,N)
#' 
#' id1=(1:N)[s1==1]
#' id2=(1:N)[s2==1]
#' 
#' d1=rep(N/n1,n1)
#' d2=rep(N/n2,n2)
#' 
#' X1 = X[s1==1,]
#' X2 = X[s2==1,]
#' 
#' re <- harmonize(X1,d1,id1,X2,d2,id2)
#' 
#' colSums(re$w1*X1)
#' colSums(re$w2*X2)
#' 
#' #---  if the true totals is known
#' 
#' totals <- c(N,colSums(X))
#' re <- harmonize(X1,d1,id1,X2,d2,id2,totals)
#' 
#' colSums(re$w1*X1)
#' colSums(re$w2*X2)
#' colSums(X)
#' 
harmonize <- function(X1,d1,id1,X2,d2,id2,totals)
{
  
  # number of units in each sample
  n1 <- nrow(X1)
  n2 <- nrow(X2)
  
  # add constant vector to ensure same sum
  XX1=cbind(rep(1,n1),X1)
  XX2=cbind(rep(1,n2),X2)
  
  # we can specify the desired total (for example if we know the totals of the population)
  if(missing(totals)){
    n12=length(intersect(id1,id2))
    a=(n1-n12)/(n1+n2-2*n12)  
    totals=a*colSums(d1*XX1)+(1-a)*colSums(d2*XX2)
  }
  
  # calibration with sampling package
  w1=d1*calibRaking(as.matrix(XX1),d1,totals,q = rep(1,length(d1)))$g
  w2=d2*calibRaking(as.matrix(XX2),d2,totals,q = rep(1,length(d2)))$g
  
  # w1=d1*calibRaking(as.matrix(XX1),d1,totals,q = rep(1,length(d1)))
  # w2=d2*calibRaking(as.matrix(XX2),d2,totals,q = rep(1,length(d2)))
  
  return(list(w1=w1,w2=w2))
}

