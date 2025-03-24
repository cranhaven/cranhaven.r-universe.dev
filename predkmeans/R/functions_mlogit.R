###############################
## Functions for mlogit  ##
###############################
##
## This file contains:
##
##	mlogit()
##  print.mlogit()
##  summary.mlogit()
##  print.summary.mlogit()
##  logLikeMultinomial()
##  gradientMultinomial()
##  hessianMultinomial()
##  logLikeMultinomial_Rcpp()
##  gradientMultinomial_Rcpp()
##  hessianMultinomial_Rcpp()

##' @name mlogit
##' @title Multinomial Logistic Regression
##
##' @description Solves a multinomial logistic problem using 
##'		Newton-Raphson method
##
##' @details The optimization is done using the \code{\link{maxNR}} function from the \code{maxLik} package.  The log-likehood function, along with its gradient and hessian, are implemented as C++ functions (via the \code{RcppArmadillo} package).
##  Input:
##' @param Y  A matrix of the outcomes, with K columns for 
##'		the K groups.  Row sums of the matrix should be equal
##'		to one, but entries do not have to be 0/1 (but they
##'		should be positive). i.e. this is a matrix of hard or
##'		soft assignments to K categories. The first column is
##'		used as the reference category.
##' @param X  matrix of covariates for regression. Should have
##'		the same number of rows (observations) as Y. Coefficients
##'		for all parameters in X are computed for K-1 groups. 
##'		The coefficients  corresponding to the first column of Y
##'		are assumed to be zero.
##' @param beta  starting values for the optimziation.  Should be
##'		given as a matrix of column vectors, each vector a
##'		different starting value.  If null, defaults to zeros.
##' @param add.intercept   a logical indicator of whether an
##'		intercept column should be added to X
##' @param betaOnly  logical indicator of whether only the
##'		parameter estimates beta should be returned. Otherwise,
##'		beta is returned along with fitted objects. See Output.
##' @param tol.zero   the tolerance threshold for considering a
##' 	fitted value as equal to zero.  Used for warning about
##'		fitted values of 0/1.  Is NOT part of the optimization
##'		control parameters.
##' @param verbose   logical indicator that controls whether text
##'		indicating progress is output to display
##' @param suppressFittedWarning   indicator of whether or not
##'		warnings about fitted values of 1 are returned
##' @param maxNR.print.level  numeric value giving the level of
##'		output produced by maxNR.  see \code{?maxNR} for details.
##'		Defaults to 0.
##' @param iterlim    iteration limit for maxNR. Defaults to 150.
##' @param checkY indicator for whether Y should be checked to be a valid assignment matrix.  Set to \code{FALSE} if using decimal values in Y.
##
##
##' @seealso \code{\link{predkmeans}}
##' @author Joshua Keller
##' @export
##' @family mlogit methods
##' @useDynLib predkmeans
##' @importFrom Rcpp sourceCpp
##
##  Output:
##' @return A list containing the following:
##'
##' \item{beta}{ a p x K matrix of parameter estimates corresponding to the K columns of Y and p covariates in X}
##'  \item{fitted01}{indicator of whether fitted values of 1 were present.}
##'  \item{fitted}{the fitted probabilities}
##'  \item{res.best}{ the best result from the maxNR fit}
##' 	\item{status}{ small data frame summarizing the status of the fits}
##'   \item{res. all}{ a list containing the results from all maxNR fits}
##'
##' @examples
##' n <- 2000
##' X <- cbind(1,
##'            matrix(rnorm(2*n), nrow=n, ncol=2),
##'            rbinom(n, size=1, prob=0.3))
##' beta <- cbind(rep(0, 4),
##'               c(0.5, 1, 0, -1),
##'               c(0, 2, 2, 0))
##' probs <- exp(X %*% beta)
##' probs <- probs/rowSums(probs)
##' Y <- t(apply(probs, 1, function(p) rmultinom(1, 1, p)))
##' mfit <- mlogit(Y=Y, X=X, betaOnly=TRUE)
##' mfit
##
mlogit <- function(Y, X, beta=NULL, add.intercept=FALSE, betaOnly=FALSE, tol.zero=1e-8, verbose=T, suppressFittedWarning=FALSE, maxNR.print.level=0, iterlim=150, checkY=TRUE){
	
	X <- as.matrix(X)
	if (is.null(colnames(X))) colnames(X) <- paste0("X", 1:ncol(X))
	
	# Checking Y matrix
	if (!is.matrix(Y)){
		ynames <-  as.character(sort(unique(Y)))
		Y <- model.matrix(~0 + as.factor(Y))
		colnames(Y) <- ynames
	}
	if (checkY){
		if(any(rowSums(Y)>1) || !all(Y %in% c(0, 1))){
			stop("Y is not matrix of valid assignments. Please provide\na matrix of 0's and 1's, with no more than one 1 per row.\nOr use parameter 'checkY=FALSE' to override this check.")
		}
	}
	if (is.null(colnames(Y))) colnames(Y) <- paste0("Y", 1:ncol(Y))
	if (add.intercept) {
		X <- cbind(1, X)
		colnames(X)[1] <- "Intercept"
		cat("Adding intercept to X matrix \n")
	}

	K <- ncol(Y)
	p <- ncol(X)
	
	if (is.null(beta)){
		beta <- rep(0, times=(K-1)*p)
	}
	beta <- as.matrix(beta)
	
	nInits <- ncol(beta)  #Number of initial conditions to try

	conv <- rep(FALSE, nInits)
	convNumDeriv <- conv	
	res <- vector("list", nInits)
	LLvalue <- rep(NA, nInits)	

	for (i in 1:nInits){
		if (verbose){
			message(paste0("Optimization using starting value ", i, "/", nInits))
		}
		beta.start <- beta[,i]
	
		res[[i]] <- maxLik::maxNR(fn=function(beta) logLikeMultinomial_Rcpp(beta, Y, X, K, p), grad=function(beta) gradientMultinomial_Rcpp(beta, Y, X, K, p), hess=function(beta) hessianMultinomial_Rcpp(beta, Y, X, K, p),  start= beta.start, print.level= maxNR.print.level, iterlim= iterlim)
		# } else {
		# res[[i]] <- maxLik::maxNR(fn=function(beta) logLikeMultinomial(beta, Y, X), grad=function(beta) gradientMultinomial(beta, Y, X), hess=function(beta) hessianMultinomial(beta, Y, X),  start= beta.start, print.level= maxNR.print.level, iterlim= iterlim)

		# }
		hessianND <- FALSE
		try(hessianND <- all(eigen(res[[i]]$hessian)$values <0), silent=T)
		# Added stopping code for relative convergence (code=8)
		res[[i]]$NRconv <- res[[i]]$code==1 || res[[i]]$code==2 || res[[i]]$code==8
		conv[i] <- res[[i]]$NRconv && hessianND
		LLvalue[i] <- res[[i]]$maximum
		res[[i]]$conv <- conv[i]		
	}
	
	best.ind <- which.max(LLvalue)
	res.best <- res[[best.ind]]
	status <- data.frame(LLvalue=LLvalue, convergence=sapply(res, function(x) x$NRconv), conv=conv)
	
	beta <- cbind(0, matrix(res.best$estimate, nrow=p, ncol=K-1))
	rownames(beta) <- colnames(X)
	colnames(beta) <- colnames(Y)

	# Check that fitted probabilities are 0/1
	eta <- X %*% beta
#	mu <- exp(eta)/rowSums(exp(eta))
	mu <- apply(eta, 2, function(x) 1/rowSums(exp(eta-x)))
	
	prob01check <- all(apply(mu>(1-tol.zero), 1, any))
	
	if (prob01check && !suppressFittedWarning){
		warning("All observations have fitted probabilites of 0 and 1")
	}
	
	if (betaOnly){
			return(beta)
	} else {
		out <- list(beta=beta, fitted01=prob01check, fitted=mu, res.best=res.best, status=status, res.all=res)
		class(out) <- "mlogit"
		return(out)
	}
}	
	

##' @export
print.mlogit <- function(x, ...){
	K <- ncol(x$beta)
	p <- nrow(x$beta)
	cat("Mlogit optimization with\n", K, "categories and\n", p, "variables\n")
#	print(x$beta)
	
	cat("Optimization has", ifelse(x$status$conv," "," not "),"converged\n", sep="")
	invisible(x)
}##print.mlogit()



##' @export
summary.mlogit <- function(object, ...){
	class(object) <- "summary.mlogit"
	object
}## summary.mlogit()


##' @export
print.summary.mlogit <- function(x, betarowthresh=10, ...){
	K <- ncol(x$beta)
	p <- nrow(x$beta)
	cat("Mlogit optimization with\n", K, "categories and\n", p, "variables\n")
	if(x$status$conv) {
		cat("Method converged with LL value of ", x$status$LLvalue, ".\n",sep="")		
	} else {
		cat("Method did not converge. LL value at exit:", x$status$LLvalue,".\n")		
	}
	cat("Estimated coefficients are:\n")
	if (nrow(x$beta) < betarowthresh) {
		print(x$beta)	
	} else {
		print(x$beta[1:betarowthresh,])
		cat("...\n")
	}
}



	
logLikeMultinomial <- function(beta, Y, X){
	# Assumes first column of Y is reference group
	K <- ncol(Y)
	p <- ncol(X)
	beta <- matrix(beta, nrow=p, ncol=K-1)
	Xbeta <- cbind(0, X %*% beta)
	XbetaMax <- apply(Xbeta, 1, max)
	LL <- sum(rowSums(Y * Xbeta) - (XbetaMax + log(rowSums(exp(Xbeta - XbetaMax)))))
	LL
}

gradientMultinomial <- function(beta, Y, X){
	# Assumes first column of Y is reference group
	K <- ncol(Y)
	p <- ncol(X)
	beta <- matrix(beta, nrow=p, ncol=K-1)
	# Compute probabilities
	#	eta <- exp(X %*% beta)
	#	prob <- eta/(1 + rowSums(eta))
	logeta <- X %*% beta
	prob <- apply(logeta, 2, function(x) 1/(exp(-x) + rowSums(exp(logeta-x))))
	g <- as.vector(crossprod(X, Y[,-1] - prob))
	g
}

hessianMultinomial <- function(beta, Y, X){
	# Assumes first column of Y is reference group
	K <- ncol(Y)
	p <- ncol(X)
	beta <- matrix(beta, nrow=p, ncol=K-1)
	# Compute probabilities
#	eta <- exp(X %*% beta)
#	prob <- eta/(1 + rowSums(eta))
	logeta <- X %*% beta
	prob <- apply(logeta, 2, function(x) 1/(exp(-x) + rowSums(exp(logeta-x))))
	H <- matrix(nrow=0, ncol=p*(K -1 ))
	for (k1 in 1:(K-1)){
		H1 <- matrix(nrow=p, ncol=0)
		p1 <- prob[,k1]
	for (k2 in 1:(K-1)){
		if (k1==k2) { Hn <- crossprod(x=X, y= p1*(1 - prob[,k2])*X) 
			} else {
				Hn <- crossprod(x=X, y=-p1*prob[,k2] * X)
			}
		H1 <- cbind(H1, Hn)
	} 
	H <- rbind(H, H1)
	}
	-H
}



logLikeMultinomial_Rcpp <- function(beta, Y, X, K=ncol(Y), p=ncol(X)){
	# Assumes first column of Y is reference group
#	K <- ncol(Y)
#	p <- ncol(X)
	beta <- matrix(beta, nrow=p, ncol=K-1)

#	LL <- sum(rowSums(Y[,-1] * (X %*% beta)) - log(1 + rowSums(exp(X%*% beta))))
#	LL <- loglikeCpp(X, beta, Y[,-1, drop=FALSE])
	LL <- loglikeCpp(X, beta, Y[, -1, drop=FALSE], n=as.integer(nrow(Y)))
	LL
}


gradientMultinomial_Rcpp <- function(beta, Y, X, K=ncol(Y), p=ncol(X)){
	# Assumes first column of Y is reference group
#	K <- ncol(Y)
#	p <- ncol(X)
	beta <- matrix(beta, nrow=p, ncol=K-1)
	# Compute probabilities
#	eta <- exp(X %*% beta)
#	prob <- eta/(1 + rowSums(eta))
##	logeta <- X %*% beta
##	prob <- apply(logeta, 2, function(x) 1/(exp(-x) + rowSums(exp(logeta-x))))
	g <- as.vector(gradientMultinomialCpp(X=X, b=beta, y=Y[,-1, drop=FALSE], k=K))
	g
}


hessianMultinomial_Rcpp <- function(beta, Y, X, K=ncol(Y), p=ncol(X)){
	# Assumes first column of Y is reference group
#	K <- ncol(Y)
#	p <- ncol(X)
	beta <- matrix(beta, nrow=p, ncol=K-1)
	# H <- hessianMultinomialCpp(X=X, b=beta, y=Y[,-1, drop=FALSE], p=p,k=K)
	H <- hessianMultinomialCpp(X=X, b=beta, y=Y, p=p,k=K)
	-H
}
	