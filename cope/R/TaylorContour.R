#' Computes tail probabilities of a Gaussian field on a contour with Taylor's 
#' method.
#'
#' @param x x-Coordinates of the grid on which the data is observed.
#' @param y y-Coordinates of the grid on which the data is observed.
#' @param cont The contour of f at value level
#' @param R An array of dimension c(length(x),length(y),n) containing the 
#'          realizations of the field.
#' @importFrom stats na.omit pnorm
#' @return A function g that computes for u>0 the probility that the supremum of
#'         the field exceeds u. 
TaylorContour = function(x, y, cont, R){
  
  # cont = contourLines(x,y,f,levels=level,nlevels=1)
  # Is the contour empty?
  if(length(cont) == 0) return(function(u) return(0))
  
  n = dim(R)[3]
  SC = vector("list",length(cont))
  
  for(i in 1:length(cont)) 
    for(j in 1:n) 
      SC[[i]] = cbind(SC[[i]],
                      fields::interp.surface(list(x=x,y=y,z=R[,,j]),
                                     cbind(cont[[i]]$x,cont[[i]]$y)))
  
  # clean up NAs due to extrapolation
  SC = sapply(SC, na.omit)
  
  #Gives the EC of one component.
  EC = function(X){
    if(!is.matrix(X)) return(0) # Pathological cases.
    if(all(X[1,] == X[nrow(X),])) return(0)
    return(1)
  }
  #Euler characterstic of the simplicial complex.
  mu0 = sum(sapply(SC,EC))
  
  #Gives the length of one component.
  L = function(X){
    if(!is.matrix(X)) return(0) # Pathological cases.
    if(nrow(X) == 1) return(0)
    X = X/sqrt(rowSums(X^2))
    sum(sqrt(rowSums(diff(X)^2)))
  }
  
  mu1 = sum(sapply(SC,L))
  
  rho =function(u){c(exp(-u^2/2)/(2*pi),1-pnorm(u))} 
  
  g = function(u){sum(c(mu1,mu0)*rho(u))}
  Vectorize(g)
}