
get.cluster.pred <- function(R, beta){
	eta.pred <- R %*% beta
	mu.pred <- exp(eta.pred)/rowSums(exp(eta.pred))
	cluster.pred <- apply(mu.pred, 1, which.max)
	return(cluster.pred)
}


##' @name assignCluster
##' @title Make Cluster Assignments
##' @description Assigns observation to the nearest cluster center, using squared Euclidean distance.
##
##'	@param X matrix of observations
##' @param centers matrix of cluster centers
##' @return A vector of cluster labels
##' @author Joshua Keller
##' @export
##' @examples
##' X <- matrix(rnorm(100*5), nrow=100, ncol=5)
##' centers <- matrix(runif(3*5), nrow=3, ncol=5)
##' 
##' cl <- assignCluster(X, centers)
##' table(cl)
assignCluster <- function(X, centers){
	k <- nrow(centers)
    cl_dist <- matrix(NA, nrow=dim(X)[1], ncol=k)
    ones <- rep(1, dim(X)[2])
    for (cl in 1:k){		
        C <- matrix(data=unlist(rep(centers[cl,], each=dim(X)[1])), nrow=dim(X)[1], byrow=F, dimnames=dimnames(X))
        cl_dist[,cl] <- (X-C)^2 %*% ones
    }
    labels <- apply(cl_dist, 1, which.min)
    names(labels) <- rownames(X)
    return(labels)
}




##' @name createPCAmodelmatrix
##' @title Create Principal Component Analysis (PCA) scores matrix
##' @description Wrapper function for creating PCA scores to be used
##'		in a regression analysis.
##
##' @details This is a wrapper around \code{\link{prcomp}}, which does 
##'			the necessary computation.
##' @param data Matrix or data frame of data
##' @param ncomps Number of PCA components to return.
##' @param covarnames Names of variables or column numbers in \code{data}
##'		on which the PCA is to be run.
##'	@param center Logical indicator of whether \code{data} should be centered. Passed to \code{\link{prcomp}}.
##'	@param scale Logical indicator of whether \code{data} should be scaled. Passed to \code{\link{prcomp}}.
##' @param matrixonly Logical indicator of whether only the model matrix should
##'		be returned, or the full output from \code{\link{prcomp}}.
##
##' @return If \code{matrixonly=TRUE}, a matrix of PCA scores. Otherwise a list containing two elements: \code{X},  a matrix of scores, and \code{pca}, the output from \code{prcomp}.
##' @export
##' @importFrom stats prcomp
##' @author Joshua Keller
##' @seealso \code{\link{createTPRSmodelmatrix}}, \code{\link{predkmeansCVest}}
##' @examples
##' n <- 100
##' d <- 15
##' X <- matrix(rnorm(n*d), ncol=d, nrow=n)
##' X <- as.data.frame(X)
##' mx <- createPCAmodelmatrix(data=X, ncomps=2)
createPCAmodelmatrix <- function(data, ncomps, covarnames=colnames(data), center=TRUE, scale=TRUE, matrixonly=TRUE){
	if (ncomps>ncol(data)) stop("ncomps too large for data provided.")
	pca <- prcomp(data[,covarnames, drop=FALSE], center=center, scale=scale)
	X <- pca$x[,1:ncomps, drop=FALSE]
	if (matrixonly) {
		return(X)
	} else {
		return(list(X=X, pca=pca))
	}
}

##' @name createTPRSmodelmatrix
##' @title Create matrix of Thin-Plate Regression Splines (TPRS) 
##' @description Wrapper function for creating a matrix of thin-plate regression splines (TPRS)
##'		to be used in a regression analysis.
##
##' @param data Matrix or data frame of data
##' @param df Degrees of freedom for thinplate splines. This does not include an intercept, so the \code{k} argument of \code{s()} is \eqn{k = df + 1}.
##' @param covarnames Names of other covariates to be included in the model
##'		matrix.
##'	@param xname Name of variable the provides the x-coordinate of location.
##'	@param yname Name of variable the provides the y-coordinate of location.
##' @param matrixonly Logical indicator of whether only the model matrix should
##'		be returned, or the full output from \code{\link{gam}}.
##' @param TPRSfx Should the TPRS degrees of freedom be fixed. Passed as the \code{fx} argument to \code{s()}.
##
##' @export
##' @importFrom stats formula
##' @author Joshua Keller
##' @seealso \code{\link{createPCAmodelmatrix}}, \code{\link{predkmeansCVest}}
##' @examples
##' n <- 200
##' x <- runif(n=n, 0, 100)
##' y <- runif(n=n, 0, 100)
##' d <- data.frame(x=x, y=y)
##' mx <- createTPRSmodelmatrix(data=d, df=5)
##
createTPRSmodelmatrix <- function(data, df=5, covarnames=NULL, xname="x", yname="y", TPRSfx=TRUE, matrixonly=TRUE){
if(!requireNamespace("mgcv", quietly=TRUE)){
	stop("mgcv is required to create TPRS objects.  Please install it.", call.=FALSE)
}	
	TPRSk <- df + 1 # Add 1 for the intercept term
	# Create the formula
	f <- ""
	if (length(covarnames)>0){
		f <- paste("+ ", paste0(covarnames, collapse=" + "))
	}
	f <- formula(paste0(xname,"~s(", xname,", ", yname,", fx=", TPRSfx, ", k=", TPRSk, ")", f))
	# Fit a GAM to get the model matrix
	gamfit <- mgcv::gam(f, data=as.data.frame(data))
	X  <- model.matrix(gamfit)

	if(matrixonly){
		return(X)
	} else {
		return(list(X=X, gamfit=gamfit))
	}
}