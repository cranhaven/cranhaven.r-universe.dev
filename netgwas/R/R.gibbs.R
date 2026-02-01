#-------------------------------------------------------------------------------#
# Package: Network-Based Genome-Wide Association Studies                        #
# Authors: Pariya Behrouzi, Danny Arends                                        #
# maintainer: <pariya.Behrouzi@gmail.com>                                       #
# Date: Nov 21th 2017                                                           #
#-------------------------------------------------------------------------------#

calcRcore <- function(i, n, p, gibbs.iter, mc.iter, theta, lower.upper, verbose = TRUE){
	z.gibbs <- rtmvnorm.sparseMatrix(n=mc.iter, H= theta , lower=lower.upper$lower[i,], upper=lower.upper$upper[i,], burn.in=gibbs.iter)
    summed <- matrix(0, p, p)
    for(iteration in 1: mc.iter)
    { 
      summed <- summed + (z.gibbs[iteration,] %*% t(z.gibbs[iteration,]))
    }
    R <- summed /  mc.iter
    return(R)
}
   
calculate.R.internal = function(y, theta=NULL, lower.upper=NULL, gibbs.iter=1000, mc.iter=1000, ncores = 4, verbose = TRUE)
{
  if(missing(y)) stop("argument \"y\" is missing, with no default")
  S <- 0
  p <- ncol(y)
  n <- nrow(y)
  if(missing(lower.upper)) lower.upper <- lower.upper(y)
  if(ncores > 1){
    cl <- makeCluster(ncores)
    scovs <- parLapply(cl = cl, 1:n, function(i) { 
        calcRcore(i, n, p, gibbs.iter, mc.iter, theta, lower.upper, verbose); 
    })
    stopCluster(cl)
  }else{
    scovs <- lapply(1:n, function(i){ calcRcore(i, n, p, gibbs.iter, mc.iter, theta, lower.upper); })
  }
  
  for(i in 1:n){ 
    S <- S + scovs[[i]]
  }
  ES  <- cov2cor(S/n)
  rm(S)
  return(ES)
}

R.gibbs = function(y, theta, gibbs.iter=1000, mc.iter=500, ncores=NULL, verbose = TRUE)
{
  if(is.null(ncores)) ncores= detectCores() - 1
  if(missing(theta)) 
  {
	theta <- sparseMatrix(i = 1:ncol(y), j = 1:ncol(y), x = 1)
  }else{
	theta <- as(theta, "dgTMatrix") 
	theta <- as(theta, "sparseMatrix") 
  }
  lower.upper <- lower.upper(y)
  ES <- calculate.R.internal(y, theta, lower.upper, gibbs.iter, mc.iter, ncores, verbose)
  return(ES = ES)
 }
