#' @title Estimates the parameters of a grid type copula
#' @description This function estimates grid type copulas by one of the
#' following methods: maximum likelihood or least squares (See reference).
#' @return Returns a list with a matrix with the density over the grid,
#' a matrix with the quantity of data over the grid, the number of subintervals
#' for the U2 variable,
#' the number of subintervals for the U1 variable.
#' @param X a matrix of size nx2 with the observed values, optional if U is
#' provided.
#' @param U a matrix of size nx2 with the values in the domain of
#' copula, optional if X is provided. If not provided, U is calculated using
#' the empirical cumulative distribution function "ecdf()".
#' @param k positive integer indicating the number of subintervals for the U2 variable.
#' @param m positive integer indicating the number of subintervals for the U1 variable.
#' @param method Method that uses, least squares "ls" or maximum likelihood "ml", by default "ml".
#' @param D.ini optional, a matrix with the initial values of the density
#' copula only for the method of maximum likelihood.
#' @param criterion If the values of k and m are not specified, they will be
#' obtained by the Akaike Information Criterion "AIC" or Bayesian Information
#' Criterion "BIC", by default "AIC". This option evaluates identical values for
#' k and m.
#' @references {@misc{https://doi.org/10.48550/arxiv.2010.15709,
#' doi = {10.48550/ARXIV.2010.15709},
#' url = {https://arxiv.org/abs/2010.15709},
#' author = {Pfeifer, Dietmar and Strassburger, Doreen and Philipps, Joerg},
#' keywords = {Methodology (stat.ME), Risk Management (q-fin.RM), FOS: Computer and information sciences, FOS: Computer and information sciences, FOS: Economics and business, FOS: Economics and business, 62H05, 62H12, 62H17, 11K45},
#' title = {Modelling and simulation of dependence structures in nonlife insurance with Bernstein copulas},
#' publisher = {arXiv},
#' year = {2020},
#' copyright = {arXiv.org perpetual, non-exclusive license}
#' }
#' }
#' @examples
#' n <- 500
#' x <- rgamma(n,4,1/2)
#' e <- rnorm(n,0,.3)
#' y <- sin(x+e)
#' Fx <- ecdf(x)
#' Fy <- ecdf(y)
#' u <- Fx(x)
#' v <- Fy(y)
#' df <- cbind(u,v)
#' k <- 5
#' m <- 4
#' copula.grid <- estimate.gridCopula(U = df, k = k, m = m , method = "ml")
#' print(copula.grid$Density)
#' @export

estimate.gridCopula <- function(X=NULL, U=NULL, k=NULL, m=NULL, method="ml",
                                D.ini=NULL, criterion="AIC") {
  if(is.null(X) & is.null(U)) {
    stop("A dataset is required")
  } else {
    if(is.null(U)) {
      U <- 0*X
      Fx.hat <- ecdf(x=X[, 1])
      U[, 1] <- Fx.hat(X[, 1])
      Fx.hat <- ecdf(x=X[, 2])
      U[, 2] <- Fx.hat(X[, 2])
    }
  }

	if(method=="ls") {
	  XIC <- c()
		if(is.null(k) & is.null(m)) {
			for(i in 2:20) {
				copula <- calculate.ls(U=U, k=i, m=i)
				if(criterion == "AIC") {
				  XIC[i-1] <- aic.grid(copula)
				} else if(criterion == "BIC") {
				  XIC[i-1] <- bic.grid(copula)
				}
			}
			k <- which.min(XIC) + 1
			m <- k
		} else if(!is.null(k) & is.null(m)) {
		  m <- k
		} else if(is.null(k) & !is.null(m)) {
		  k <- m
		}
		k <- round(k,0)
		m <- round(m,0)
		result <- calculate.ls(U=U, k=k, m=m)
	} else if(method=="ml") {
	  XIC <- c()
		if(is.null(k) & is.null(m)) {
		  for(i in 2:20) {
		    copula <- calculate.ml(U=U, k=i, m=i, D.ini=D.ini)
		    if(criterion == "AIC") {
		      XIC[i-1] <- aic.grid(copula)
		    } else if(criterion == "BIC") {
		      XIC[i-1] <- bic.grid(copula)
		    }
		  }
		  k <- which.min(XIC)+1
		  m <- k
		} else if(!is.null(k) & is.null(m)) {
		  m <- k
		} else if(is.null(k) & !is.null(m)) {
		  k <- m
		}
	  k <- round(k,0)
	  m <- round(m,0)
	  result <- calculate.ml(U=U, k=k, m=m, D.ini=D.ini)
	}
  result$X = X
  result$U = U
  return(result)
}
