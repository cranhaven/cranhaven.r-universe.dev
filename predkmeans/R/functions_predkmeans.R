###############################
## Functions for predkmeans  ##
###############################
##
## This file contains:
##
##	predkmeans()
##	print.predkmeans()
##	summary.predkmeans()
##	print.summary.predkmeans()
##	relevel.predkmeans()
##	getExpMahal()
##  getH()
##	getMu()
##	getSigma2()


##
##' @name predkmeans
## 
##' @title Predictive K-means Clustering
##
##' @description Uses a Mixture-of-experts algorithm to find 
##' cluster centers that are influenced by prediction covariates.
##
## Inputs:
##' @param X An \code{n} by \code{p} matrix or data frame of data to be clustered.
##' @param R Covariates used for clustering. Required unless doing k-means
##'		clustering (i.e. \code{sigma2=0} and \code{sigma2fixed=TRUE}).
##' @param K Number of clusters
##' @param mu starting values for cluster centers. If NULL (default), 
##'  	then value is chosen according to \code{muStart}.
##' @param muStart Character string indicating how initial value
##'  	of mu should be selected. Only used if mu=NULL.  Possible
##' 	values are \code{"random"} or \code{"kmeans"} (default).
##' @param sigma2 starting value of sigma2. If set to \code{0} and
##'		\code{sigma2fixed=TRUE}, the standard k-means is done
##'		instead of predictive k-means.
##' @param sigma2fixed Logical indicating whether sigma2
##'		should be held fixed.  If FALSE, then
##'		sigma2 is estimated using Maximum Likelihood.
##' @param maxitEM Maximum number of EM iterations for
##' 	finding the Mixture of Experts solution. If doing regular
##'		k-means, this is passed as \code{iter.max}.
##' @param tol convergence criterion
##' @param maxitMlogit Maximum number of iterations in the
##'   	mlogit optimization (nested within EM algorithm)
##' @param muRestart  Gives max number of attempts at picking
##'    	starting values. Only used when muStart='random'.
##'		If selected starting values for mu are constant
##'     within each cluster, then the starting values
##'     are re-selected up to muRestart times.
##' @param  convEM  controls the measure of convergence for the 
##'     EM algorithm.  Should be one of "mu", "gamma", or
##'	    "both".  Defaults to "both."  The EM algorithm 
##'     stops when the Frobenius norm of the change in mu,
##'		the change in gamma, or the change in mu and the change in gamma
##'		is less than 'tol'.
##' @param  verbose numeric vector indicating how much output to produce
##' @param nStarts number of times to perform EM algorithm 
##' @param returnAll A list containing all \code{nStarts} solutions is
##'		included in the output.
##' @param ... Additional arguments passed to \code{\link{mlogit}}
##
##' @details 
##'		A thorough description of this method is provided in Keller et al. (2017).
##' 		The algorithm for sovling the mixture of Experts model is 
##'		based upon the approach presented by Jordan and Jacobs (1994). 
##'
##'		If \code{sigma2} is 0 and \code{sigm2fixed} is TRUE, then standard k-means clustering (using \code{\link{kmeans}}) is done instead.
##'
##
##'	@return  An object of class \code{predkmeans}, containing the following elements:
##' \item{res.best}{A list containing the results from the best-fitting solution to the Mixture of Experts problem: 
##'  \describe{
##'   \item{mu}{Maximum-likelihood estimate of intercepts from normal mixture model. These are the cluster centers.}
##'   \item{gamma}{Maximum-likelihood estimates of the mixture coefficeints.}
##'   \item{sigma2}{If \code{sigma2fixed=FALSE}, the maximum likelihood estimate of \code{sigma2}}
##'   \item{conv}{Indicator of covergence.}
##'   \item{objective}{Value of the log-likelihood.}
##'   \item{iter}{Number of iterations.}
##'   \item{mfit}{A subset of output from \code{\link{mlogit}}.}
##' }}
##' \item{center}{Matrix of cluster centers}
##' \item{cluster}{Vector of cluster labels assigned to observations}
##' \item{K}{Number of clusters}
##'	\item{sigma2}{Final value of sigma^2.}
##' \item{wSS}{Mean within-cluster sum-of-squares}
##' \item{sigma2fixed}{Logical indicator of whether sigma2 was held fixed}
##list(mu=mu, gamma=gamma, sigma2=sigma2, conv=converged, objective= obj, iter=iter, mfit=mfit[c("beta", "fitted01", "fitted", "res.best", "status")])
##
##
##
##' @seealso \code{\link{predictML.predkmeans}, \link{predkmeansCVest}}
##' @references Keller, J.P., Drton, M., Larson, T., Kaufman, J.D., Sandler, D.P., and Szpiro, A.A. (2017). Covariate-adaptive clustering of exposures for air pollution epidemiology cohorts. \emph{Annals of Applied Statistics}, 11(1):93--113.
##' @references Jordan M. and Jacobs R. (1994). Hierarchical mixtures of
##'		experts and the EM algorithm. \emph{Neural computation 6}(2),
##'		 181-214.
##
##' @importFrom stats kmeans model.matrix
##' @author Joshua Keller
##' @export
##
##' @examples
##' n <- 200
##' r1 <- rnorm(n)
##' r2 <- rnorm(n)
##' u1 <- rbinom(n, size=1,prob=0)
##' cluster <- ifelse(r1<0, ifelse(u1, "A", "B"), ifelse(r2<0, "C", "D"))
##' mu1 <- c(A=2, B=2, C=-2, D=-2)
##' mu2 <- c(A=1, B=-1, C=-1, D=-1)
##' x1 <- rnorm(n, mu1[cluster], 4)
##' x2 <- rnorm(n, mu2[cluster], 4)
##' R <- model.matrix(~r1 + r2)
##' X <- cbind(x1, x2)
##' pkm <- predkmeans(X=cbind(x1, x2), R=R, K=4)
##' summary(pkm)
predkmeans <- function(X, R, K, mu=NULL, muStart=c("kmeans","random"), sigma2=0,  sigma2fixed=FALSE,maxitEM=100, tol=1e-5, convEM=c("both", "mu", "gamma"), nStarts=1, maxitMlogit=500,verbose=0, muRestart=1000, returnAll=FALSE, ...) {
	
	if(is.null(K)){
		stop("K not provided.  Please provide.")
	}
	
	
	# Check that arguments are correct
	muStart <- match.arg(muStart)
	convEM <- match.arg(convEM)
	
	n <- nrow(X)
	if (n<K){
		stop("Cannot select more clusters than observations")
	}

	# If sigma2 is 0 and fixed, then return kmeans result
	if (sigma2==0 && sigma2fixed){
			res.best <- kmeans(x=X, centers=K, nstart=nStarts, iter.max=maxitEM)
	centers <- res.best$centers
	tr.assign <- res.best$cluster
	wSS <- res.best$tot.withinss
	out <- list(res.best= res.best, centers=centers, cluster=tr.assign, K=K, sigma2=0, wSS=wSS, sigma2fixed= sigma2fixed)
		class(out) <- "predkmeans"
	return(out)
	}
	
	# Check dimensions, wait until now, in case doing k-means
	# and R not needed
	if (n!=nrow(R)){
		stop("Number of outcome observations does not match number of covariate observations.")
	} 
	
	R <- as.matrix(R)
	
	d <- ncol(X) # Dimension of outcome
	if (is.null(colnames(X))) colnames(X) <- paste0("X", 1:d)
	p <- ncol(R)
	mu_orig <- mu # Store original value of mu
	results <- vector("list", nStarts)
	for (i in 1:nStarts){
		if (verbose>0) cat("Fit", i, "of", nStarts, "\n")
		
		# Initialize mu
		mu <- mu_orig
		if (is.null(mu)){
			if (muStart=="random"){
				all_same <- TRUE
				all_same_iter <- 1
				while(all_same){
					all_same_iter <- all_same_iter + 1
					if (all_same_iter > muRestart){
						stop(paste0("Picked ", muRestart, " random sets of starting centers and all were equal.  Check whether data is identical.  If necessary, change seed or increase  'all_same_iter_limit'."))
					}
					mu <- X[sample(1:nrow(X), size=K), ]
		 			all_same <- all(apply(mu, 2, function(x) diff(range(x))<.Machine$double.eps ^ 0.5))
				}
			} else if (muStart=="kmeans"){
				mu <- stats::kmeans(X, centers=K)$centers
			}
			rownames(mu) <- 1:K	
		} # if (is.mull(mu))

		# Compute initial values for h (Step E-1)
		h0 <- assignCluster(X, centers=mu)
		h0 <- stats::model.matrix(~0 + factor(h0, levels=1:K))
		#colnames(h0) <- paste0("C", 1:K)
		colnames(h0) <- 1:K
				
		# Compute mu, gamma, and sigma2 (Step M-1)
		mu <- getMu(X, h0)
		if(!sigma2fixed){
			sigma2 <- getSigma2(X, mu, h0)
		}
		mfit0 <- mlogit(h0, R, add.intercept=F, verbose=verbose>3, suppressFittedWarning=TRUE, iterlim=maxitMlogit)
		gamma <- mfit0$beta # cbind(C1=0, mfit0$beta)
		
		converged <- 0
		iter <- 0
		convCheck <- .Machine$integer.max	
		if (verbose>2) cat("Mixture of Experts code running with\n", K, " experts each finding a mean in ", d, " variables.\nThe gating networks each have ", p, " parameters\n", sep="")							
		# EM algorithm
		while (!converged){
			iter <- iter + 1
			if (iter> maxitEM) {
				if (verbose>1) cat("Stopping: Reached maximum iterations\n")
				converged <- 9
				break
			}
			
			# E-step
			h <- getH(X, R, gamma, mu, sigma2=sigma2)
			# M-step, Part 1a: mu
			mu_new <- getMu(X, h)
			# M-step, Part 1b: sigma2
			if(!sigma2fixed){
				sigma2 <- getSigma2(X, mu_new, h)
			}
			# M-step, Part 2: gamma
			mfit <- mlogit(Y=h, X=R, beta=as.vector(gamma[,-1]), add.intercept=F, iterlim= maxitMlogit, verbose=verbose>3, checkY=FALSE, ...)	
			# Quesiton: Should I stop freeze the parameters for a certain cluster, if 
			# I have achieved all 0/1's?
			if (!mfit$res.best$conv) {
				if (verbose>1) message("EM-algorithm: mlogit optimization error in updating gamma.")
				converged <- 8
				break
			} else {
				gamma_new <- mfit$beta # cbind(0, mfit$beta)
			}
			# Check for convergence		
			convCheck <- switch(convEM, mu=norm(mu_new - mu, type="F") <tol,
				gamma= norm(gamma_new - gamma, type="F") <tol,
				both=norm(mu_new - mu, type="F") <tol && norm(gamma_new - gamma, type="F") < tol)
	
			 # Force at least three iterations?
			if(convCheck && iter>2) {  
				converged <- 1
				convCheck <- norm(mu_new - mu, type="F")
			}
			gamma <- gamma_new
			mu <- mu_new
		
		}	## while (!converged)								

		# Compute the objective function value
		#Uproxy <- exp(R %*% gamma)/rowSums(exp(R %*% gamma))
		Uproxy <- R %*% gamma
		Uproxy <- apply(Uproxy, 2, function(x) 1/rowSums(exp(Uproxy-x)))
		
		# Objective is the log likelihood
		obj <- 0
		for (kk in 1:nrow(X)){
			q <- sweep(mu, 2, X[kk,])
			obj <- obj + log(sum(sigma2^(-d/2)*exp(-1/(2*sigma2)*rowSums(q*q)) * Uproxy[kk,]))
		}				
				
		results[[i]] <- list(mu=mu, gamma=gamma, sigma2=sigma2, conv=converged, objective= obj, iter=iter, h=h, mfit=mfit[c("beta", "fitted01", "fitted", "res.best", "status")])			
			
		if (verbose>1) {
			cat("EM Algorithm completed\n")
			cat("Convergence code:", converged, "\n")
			cat("Number of EM iterations:", iter, "\n")
			cat("Norm of last mu update:", convCheck, "\n")
			cat("Value of objective:", obj, "\n")
			cat("Sigma^2:", sigma2, "\n\n")
		}
	}

	best.fit <- which.max(lapply(results, function(x) x$objective))	
	# if(!results[[best.fit]]$mfit$status$conv){
		# warning("Best Fit has mlogit optimization error.")
	# }
	res.best <- results[[best.fit]]
	centers <- res.best $mu
	tr.assign <- assignCluster(X, centers= centers)
	wSS <- sum((X -  centers[tr.assign,])^2)
	out <- list(res.best= res.best, centers=centers, cluster=tr.assign, K=K, sigma2= res.best$sigma2, wSS=wSS, sigma2fixed= sigma2fixed)

	if (returnAll){
		out <- c(out, res.all=list(results), best.fit=best.fit)
	}
	class(out) <- "predkmeans"
	
	out
}## predkmeans()




###############################
## S3Methods for predkmeans  ##
###############################


##' @export
print.predkmeans <- function(x, ...){
	if(!inherits(x,"predkmeans")){
		stop("x must be of (or inherit) class 'predkmeans'.")
	}
	cat("Predictive k-means object with\n" )
	cat("    ", x$K, "Clusters\n")
	cat("    ", ncol(x$centers), "Variables\n")
#	cat("Convergence status: ", x$res.best$conv)
	invisible(x)
}##print.predkmeans()



##' @export
summary.predkmeans <- function(object, ...){
	class(object) <- "summary.predkmeans"
	object
}##summary.predkmeans()

##' @export
print.summary.predkmeans <- function(x, ...){
	cat("Predictive k-means object with\n" )
	cat("    ", x$K, "Clusters\n")
	cat("    ", ncol(x$centers), "Variables\n")
	cat("Convergence status: ", ifelse(inherits(x$res.best, "kmeans"), 1, x$res.best$conv), "\n")
	cat("Sigma^2 = ", round(x$sigma2, 2), " (Fixed = ", x$sigma2fixed, ")\n", sep="")
	cat("Within-cluster Sum-of-Squares (wSS) = ", round(x$wSS, 2), "\n")
	cat("Cluster centers are:\n")
	print(x$centers)
}




##' @name relevel.predkmeans
##' @title Re-order cluster labels 
##' @description Function for re-ordering the order of clusters in a predkmeans object.
##
##' @param x object of class \code{predkmeans}
##' @param ref New reference group ("Cluster 1"). Only used if \code{order} is NULL.
##' @param order New order of clusters. 
##' @param ... Ignored additional arguments.
##
##' @details The elements of the \code{order} argument should refer
##'		to the current position of clusters, with the position
##'		giving the new order. So \code{c(3, 1, 2)} moves 1 to 2, 2 to 3, and 3 to 1.
##
##' @author Joshua Keller
##' @export
##' @family methods for predkmeans objects
##' @importFrom stats relevel
##' @examples
##' n <- 200
##' r1 <- rnorm(n)
##' r2 <- rnorm(n)
##' u1 <- rbinom(n, size=1,prob=0)
##' cluster <- ifelse(r1<0, ifelse(u1, "A", "B"), ifelse(r2<0, "C", "D"))
##' mu1 <- c(A=2, B=2, C=-2, D=-2)
##' mu2 <- c(A=1, B=-1, C=-1, D=-1)
##' x1 <- rnorm(n, mu1[cluster], 4)
##' x2 <- rnorm(n, mu2[cluster], 4)
##' R <- model.matrix(~r1 + r2)
##' X <- cbind(x1, x2)
##' pkm <- predkmeans(X=cbind(x1, x2), R=R, K=4)
##' table(pkm$cluster)
##'
##' # Move cluster '4' to be first
##' pkm2 <- relevel(pkm, ref=4)
##' table(pkm2$cluster)
##' # Re-order based upon number of observations in each cluster
##' pkm3 <- relevel(pkm, order=order(table(pkm$cluster), decreasing=TRUE))
##' table(pkm3$cluster)
relevel.predkmeans <- function(x, ref=NULL, order=NULL, ...) {
	if(!inherits(x, "predkmeans")){
		stop("x must be of class predkmeans.")
	}
	
	if (is.null(order)){
		if (is.null(ref)){
			return(x)
		}
		if (length(ref)>1){
			warning("Only first element of 'ref' used.")
			ref <- ref[1]
		}
		ref <- suppressWarnings(as.integer(ref))
		if (is.na(ref)){
			stop("Cannot convert 'ref' to integer.")
		}
		order <- 1:x$K
		order <- c(ref, order[-ref])
	}

	xcluster <- x$cluster
	for (k in 1:x$K){
		x$cluster[xcluster==order[k]] <- k
	}
	x$centers <- x$centers[order, ]
	rownames(x$centers) <- 1:x$K
	if (inherits(x$res.best,"kmeans")){
		x$res.best$cluster <- x$cluster
		x$res.best$centers <- x$centers
		x$res.best$withinss <- x$res.best$withinss[order]
		x$res.best$size <- x$res.best$size[order]		
	} else {
		x$res.best$mu <- x$res.best$mu[order,]
		rownames(x$res.best$mu) <- 1:x$K
		x$res.best$gamma <- x$res.best$gamma[,order]
		colnames(x$res.best$gamma) <- 1:x$K
		x$res.best$h <- x$res.best$h[,order]
		colnames(x$res.best$h) <- 1:x$K
		x$res.best$mfit$beta <- x$res.best$mfit$beta[,order]
		colnames(x$res.best$mfit$beta) <- 1:x$K
		x$res.best$mfit$fitted <- x$res.best$mfit$fitted[,order]
		colnames(x$res.best$mfit$fitted) <- 1:x$K
		x$res.best$mfit$res.best$note <- "Cluster have been re-ordered. See re-ordering sequence in 'reorder' object."
		x$res.best$mfit$res.best$reorder <- c(x$res.best$mfit$res.best$reorder, list(order))
	}

	return(x)
}## relevel.predkmeans
	


###############################################
# For each row Xi of X, this computes 
#      exp(-1/(2*sigma2) * (Xi - mu)^T(Xi - mu))
# This is computed for the matrix X at once, rather than row by row
getExpMahal <- function(X, mu, sigma2) {
	q <- sweep(X, 2, mu)
	exp(-1/(2*sigma2)*rowSums(q*q))
}

###############################################
# Computes h, the expected value of z (the latent variable
# indicating cluster membership) conditional on the outcomes
# X and the current parameter values of gamma and mu
#
# Input:
#      X -- Matrix of outcome (e.g., AQS) data.  This should have n rows of
#             observations and d columns of observed variables (e.g., pollutants)
#      R -- Matrix of covariate information (e.g., GIS variables).  This should
#             have n rows of observations and p number of covariate columns.
#             This matrix is taken as-is, so if an intercept is desired it should already
#             be included as a column of R.
#      gamma -- Matrix of parameter estimates for the multinomial logistic
#                        regression of the geographic covariates.  Should contain
#                        p rows of parameters for each of K columns (corresponding
#                        to the cluster centers
#      mu -- Matrix of cluster centers.  Should have d columns for each of 
#                K rows
#             
getH <- function(X, R, gamma, mu, sigma2=sigma2){
	Px <- apply(mu, 1, function(m) getExpMahal(X, m, sigma2=sigma2))
#	Rg <- exp(R %*% gamma)/rowSums(exp(R %*% gamma))
	Rg <- R %*% gamma	
	Rg <- apply(Rg, 2, function(x) 1/rowSums(exp(Rg-x)))
	num <- Px*Rg
	h <- num/rowSums(num)
	h
}

##############################################
# Computes the weighted centers of clusters for the EM
# implementation of the Mixture of Experts problem.
getMu <- function(X, h){
	mu <- apply(h, 2, function(hk) colSums(hk*X)/sum(hk))
	t(mu)
}

getSigma2 <- function(X, mu, h){
	K <- ncol(h)
	p <- ncol(mu)
	mahal <- apply(mu, 1, function(m) rowSums(sweep(X, 2, m)^2))
	num <- sum(h*mahal)
	denom <- p*sum(h)
	num/denom
}

