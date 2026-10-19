#' Creates a prior probability density function
#'
#' This function accepts the return list of [fitCopula] and
#' creates a density function from it.
#'
#' @export
#' @importFrom VineCopula RVinePDF
#' @param Copula a list, as returned by [fitCopula]
#' @return a function that maps parameters (a vector) to probability density values (scalar)
#' @examples
#' x<-rnorm(300,mean=1,sd=2)
#' X<-matrix(x,100,3)
#' C<-fitCopula(X)
#' d<-dCopulaPrior(C)
#' print(d(c(1,2,3)))
#' print(prod(sapply(c(1,2,3),FUN=dnorm,mean=1,sd=2)))
dCopulaPrior <- function(Copula){
	U <- Copula$U
	Y <- Copula$Y
	Z <- Copula$Z
	copula <- Copula$copula
	np <- ncol(U)
	priorPDF <- function(inX){
		if (!is.matrix(inX)) {
			inX <- t(inX)
		}
		apply(inX,1,
			function(inx){
				if (any(is.na(inx))) return(0)
				lbU <- sapply(seq(np), \(i) min(U[,i]) - 0.5*c(-1,1) %*% range(U[,i]))
				ubU <- sapply(seq(np), \(i) max(U[,i]) + 0.5*c(-1,1) %*% range(U[,i]))
				ed <- sapply(
					seq(np),
					\(i)
						approx(
						c(unique(U[,i]), lbU[i], ubU[i]),
						c(unique(Z[,i]), 0, 1),
						xout=inx[i]
					)$y
				)
				mpdf <- sapply(
					seq(np),
					\(i) approx(
						c(unique(U[,i]), lbU[i], ubU[i]),
						c(unique(Y[,i]), 0, 0),
						xout=inx[i]
					)$y
				)
				if (any(is.na(ed)) || any(is.na(mpdf))){ # outside of copula defined limits
					return(0)
				}
				return(RVinePDF(ed, copula, verbose = TRUE)*prod(mpdf))
			}
		)
	}
	return(priorPDF)
}

#' rCopulaPrior returns a function that generates random values from the copula model
#'
#' The returned function generates n random vectors, as rows of a matrix.
#'
#' @importFrom VineCopula RVineSim
#' @export
#' @param Copula the return value of fitCopula()
#' @return a matrix of random values
#' @examples
#' rprior <- rNormalPrior(c(-1,0,1),c(1,2,3))
#' C <- fitCopula(rprior(1000))
#' D <- rCopulaPrior(C)
#' print(cov(D(100)))
#' print(D(10))
rCopulaPrior <- function(Copula){
  copula <- Copula$copula
  Z <- Copula$Z
  U <- Copula$U
  np <- ncol(Z)
  rprior <- function(npc){
    R <- RVineSim(npc, copula)
    prePar <- matrix(0, npc, np)
    for(i in 1:np){
      prePar[,i] <- spline(unique(Z[,i]),unique(U[,i]),xout=R[,i])$y
    }
    return(prePar)
  }
  return(rprior)
}

#' dUniformPrior creates a uniform density function
#'
#' The returned density function takes vectors of the same size as ll
#' and ul. It returns the product of the component's one-dimensional
#' uniform distributions.
#'
#' @export
#' @param ll lower limit of the random variables (a vector)
#' @param ul upper limit of the random variables (same size vector as ll)
#' @return a probability density function on vectors with the same length as ll and ul.
#' @examples
#' dup<-dUniformPrior(ll=c(0,1,2),ul=c(1,2,3))
#' dup(c(0.5,1.5,2.5))
dUniformPrior <- function(ll,ul){
	dprior <- function(x){
		if (!is.matrix(x)) x <- t(x)
		return(apply(x,1,\(r) prod(dunif(r,min=ll,max=ul))))
	}
	return(dprior)
}

#' rUniformPrior returns a random vector generator
#'
#' The return value is a function that generates random vectors of the
#' same size as ll and ul from a uniform distribution within the
#' limits defined by ul and ll. The random vectors are returned as n
#' rows of a matrix, where n is the only argument of the returned
#' function.
#'
#' @export
#' @param ll lower limit of the random variables (a vector)
#' @param ul upper limit of the random variables (same size vector as
#'     ll)
#' @return a uniform random vector generating function: runiform(n),
#'     where n is the requested number of vectors (rows)
#' @examples
#' rup<-rUniformPrior(ll=c(0,1,2),ul=c(1,2,3))
#' rup(12)
rUniformPrior <- function(ll,ul){
  np <- length(ll)
  rprior <- function(n){
    r <- matrix(runif(n*np,min=ll,max=ul),n,np,byrow=TRUE)
    return(r)
  }
  return(rprior)
}



#' dNormalPrior creates the density function of a multivariate normal
#' distribution with independent components
#'
#' The returned density function takes vectors of the same size as mean and sd.
#' It returns the product of the components' one-dimensional normal distribution,
#' with mean "mean" and standard deviation "sd".
#'
#' @export
#' @param mean mean of the random variables (a vector)
#' @param sd standard deviation of the random variables (same size vector as mean)
#' @return a probability density function on vectors with the same length as mean and sd.
#' @examples
#' dnp<-dNormalPrior(mean=c(0,1,2),sd=c(1,2,3))
#' dnp(c(0.5,1.5,2.5))
dNormalPrior <- function(mean,sd){
	dprior <- function(x){
		if (!is.matrix(x)) x <- t(x)
		return(mvtnorm::dmvnorm(x,mean=mean, sigma = diag(sd)))
	}
	return(dprior)
}

#' gNormalPrior creates the gradient function of a multivariate normal
#' distribution with independent components, in log-space
#'
#' The returned density function takes vectors of the same size as mean and sd.
#' It returns the gradient of the logarithm of the multivariate normal distribution,
#' with mean `mean` and standard deviation `sd`.
#'
#' @export
#' @param mean mean of the random variables (a vector)
#' @param sd standard deviation of the random variables (same size vector as mean)
#' @return a probability density function on vectors with the same length as mean and sd.
#' @examples
#' gnp <- gNormalPrior(mean=c(0,1,2),sd=c(1,2,3))
#' gnp(c(0.5,1.5,2.5))
gNormalPrior <- function(mean,sd){
	dprior <- function(x){
		return(
			-1*(x-mean)/(sd^2)
		)
	}
	return(dprior)
}


#' rNormalPrior returns a random vector generator
#'
#' The return value is a function that generates random vectors of the
#' same size as mean and sd from a multivariate normal distribution with 
#' independent components with mean "mean" and standard deviation "sd".
#' The random vectors are returned as n
#' rows of a matrix, where n is the only argument of the returned
#' function.
#'
#' @export
#' @param mean mean of the random variables (a vector)
#' @param sd standard deviation of the random variables (same size vector as
#'     mean)
#' @return an independent multivariate normal random vector generating function: rprior(n),
#'     where n is the requested number of vectors (rows)
#' @examples
#' rnp<-rNormalPrior(mean=c(0,1,2),sd=c(1,2,3))
#' rnp(12)
rNormalPrior <- function(mean,sd){
  np <- length(mean)
  rprior <- function(n){
    r <- matrix(rnorm(n*np,mean=mean,sd=sd),n,np,byrow=TRUE)
    return(r)
  }
  return(rprior)
}
