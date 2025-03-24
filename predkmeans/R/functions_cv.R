#####################################
## Functions for CV of predkmeans  ##
#####################################
##
## This file contains:
##
##	predkmeansCVest()
##	predkmeansCVpred()
##  print.predkmeansCVest()
##  print.predkmeansCVpred()
##  createCVgroups()

##
##' @name predkmeansCVest
## 
##' @title Cross-validation of Predictive K-means Clustering
##
##' @description Performs cross-validation of predictive k-means clustering and cluster prediction.
##
## Inputs:
##' @param X Outcome data
##' @param R Covariates. Coerced to data frame.
##' @param K Number of clusters
##' @param cv.groups A list providing the cross-validation
##'		groups for splitting the data. groups for splitting the data.
##'		 Alternatively, a single number giving the number of groups into
##'  which the data are randomly split. A value of '0' implies leave-one-out.
##'  Defaults to 10.
##'	@param scale Should the outcomes be re-scaled within each training
##'		group?
##'	@param covarnames Names of covariates to be included directly.
##' @param PCA Logical indicator for whether PCA components should be computed
##'		from R.
##'	@param PCAcontrol Arguments passed to \code{\link{createPCAmodelmatrix}}. This includes \code{ncomps}.
##' @param TPRS Logical indicator for whether thin-plate regression
##'		splines should be created and added to covariates.
##'	@param TPRScontrol Arguments passed to \code{\link{createTPRSmodelmatrix}}. This includes \code{df}.
##' @param sigma2 starting value of sigma2. Setting \code{sigma2=0} and 
##'		\code{sigma2fixed=TRUE} results in regular k-means clustering.
##' @param sigma2fixed Logical indicating whether sigma2
##'		should be held fixed.  If FALSE, then
##'		sigma2 is estimated using Maximum Likelihood.
##' @param returnAll A list containing all \code{nStarts} solutions is
##'		included in the output.
##' @param ... Additional arguments passed to either \code{\link{predkmeans}} or the prediction method.
##
##' @details These wrappers are designed to simplify cross-validation of a dataset. For models including thin-plate regression splines (TPRS) or principal component analysis (PCA) scores, these functions will re-evaluate the TPRS basis or PCA decomposition on each training set.
##
##' @family 'predkmeans methods'
##' @seealso \code{\link{predkmeans}}, \code{\link{createPCAmodelmatrix}}, \code{\link{createTPRSmodelmatrix}}
##
##' @author Joshua Keller
##' @export
##' @examples 
#' n <- 200
#' r1 <- rnorm(n)
##' r2 <- rnorm(n)
##' u1 <- rbinom(n, size=1,prob=0)
##' cluster <- ifelse(r1<0, ifelse(u1, "A", "B"), ifelse(r2<0, "C", "D"))
##' mu1 <- c(A=2, B=2, C=-2, D=-2)
##' mu2 <- c(A=1, B=-1, C=-1, D=-1)
##' x1 <- rnorm(n, mu1[cluster], 4)
##' x2 <- rnorm(n, mu2[cluster], 4)
##' R <- model.matrix(~r1 + r2)
##' X <- cbind(x1, x2)
##' pkmcv <- predkmeansCVest(X=cbind(x1, x2),
##'                          R=R, K=4, nStarts=4, cv.groups= 5,
##'                          TPRS=FALSE, PCA=FALSE, covarnames=colnames(R))
##' pkmcv
predkmeansCVest <- function(X, R, K, cv.groups=10, sigma2=0,  sigma2fixed=FALSE, scale=TRUE, covarnames=colnames(R), PCA=FALSE, PCAcontrol=list(covarnames=colnames(R), ncomps=5), TPRS=FALSE,TPRScontrol=list(df=5, xname="x", yname="y"), returnAll=FALSE, ...){ 
	
	R <- as.data.frame(R)
	fncall <- match.call()
	if(is.null(K)){
		stop("K not provided.  Please provide.")
	}
	
	# Fill in defaults for TPRScontrol
	TPRScontrol.default <- eval(formals(predkmeansCVest)$TPRScontrol)
	for (i in names(TPRScontrol.default)){
		if (i %in% names(TPRScontrol)) next;
		TPRScontrol[[i]] <- 	TPRScontrol.default[[i]]
	}
	# Fill in defaults for PCAcontrol
	PCAcontrol.default <- eval(formals(predkmeansCVest)$PCAcontrol)
	for (i in names(TPRScontrol.default)){
		if (i %in% names(PCAcontrol)) next;
		PCAcontrol[[i]] <- 	PCAcontrol.default[[i]]
	}
		
if (is.numeric(cv.groups) && length(cv.groups)==1){
	cv.groups  <- createCVgroups(x=X[, 1], k= cv.groups, useNames=TRUE)
} else if (!is.list(cv.groups)){
	stop("cv.groups must be positive integer or list. Other formats not yet implemented")
}

if (is.null(rownames(X))){
	rownames(X) <- 1:nrow(X)
}
ids <- rownames(X)


setup <- vector("list", length(cv.groups))
pkm <- vector("list", length(cv.groups))

for (i in 1:length(cv.groups)){
	test.set <- cv.groups[[i]]
	training.set <- setdiff(ids, test.set)

	training.data <- X[training.set, , drop=FALSE]
	test.data <- X[test.set, , drop=FALSE]
    if (scale){
        training.data <- scale(training.data)
        test.data <- scale(test.data, center=attributes(training.data)[["scaled:center"]], scale=attributes(training.data)[["scaled:scale"]])
        scale.center <- attributes(training.data)[["scaled:center"]]
        scale.scale <- attributes(training.data)[["scaled:scale"]]
    } else {
        scale.center <- NULL
        scale.scale <- NULL
    }      

	# Create Training and Test Covariates
	training.covars <- R[training.set, covarnames, drop=FALSE]
	test.covars <- R[test.set, covarnames, drop=FALSE]
	if (PCA){
		pcafit <- do.call(createPCAmodelmatrix, args=c(list(data=R[training.set, ,drop=FALSE], matrixonly=FALSE), PCAcontrol))
		training.covars <- cbind(training.covars, pcafit$X)
		test.covars <- cbind(test.covars , predict(pcafit$pca, newdata= R[test.set,])[, 1:ncol(pcafit$X), drop=FALSE])
	}
	if (TPRS){
	    tprs.train <- do.call(createTPRSmodelmatrix, args=c(list(data=cbind(training.covars, R[training.set,]),  covarnames=colnames(training.covars),  matrixonly=FALSE), TPRScontrol))
	    training.covars <- tprs.train$X
	    test.covars <- predict(tprs.train$gamfit, newdata=cbind(R[test.set,], test.covars), type="lpmatrix")
	} else{
		training.covars <- cbind(1, training.covars)	
		names(training.covars)[1] <- "(Intercept)"
		training.covars <- as.matrix(training.covars)
		test.covars <- cbind(1, test.covars)	
		names(test.covars)[1] <- "(Intercept)"
		test.covars <- as.matrix(test.covars)
	}
		
	pres.i <- predkmeans(X=training.data, R=training.covars, K=K, sigma2=sigma2, sigma2fixed=sigma2fixed,...)
	pkm[[i]] <- pres.i
	pkm[[i]]$test.set <- test.set
	pkm[[i]]$training.set <- training.set	
	pkm[[i]]$scale.center <- scale.center
	pkm[[i]]$scale.scale <- scale.scale

	setup[[i]] <- list(training.set=training.set, test.set=test.set, scale.center=scale.center, scale.scale=scale.scale)
}
		
out <- list(call=fncall, pkm=pkm, setup=setup, PCA=PCA, PCAcontrol=PCAcontrol, TPRS=TPRS, TPRScontrol=TPRScontrol, covarnames=covarnames, X=X, R=R, K=K, cvGroups=cv.groups)
class(out) <- "predkmeansCVest"
return(out)
}	


##
##' @name predkmeansCVpred
##' @rdname predkmeansCVest
## @title Prediction from Cross-validation of Predictive K-means Clustering
##
## @description Performs cross-validation of predictive-kmeans on a dataset.
##
##' @param object A \code{predkmeansCVest} object. 
##	@param X Matrix of observations
## @param R matrix of covariates.
##' @param method Character string indicating which prediciton method should be used. Optins are \code{ML}, \code{MixExp}, and \code{SVM}. See \code{\link{predictML}} for more information.
## @param ... Additional arguments passed to the prediction method.
##
##' @export
##
predkmeansCVpred <- function(object, X=object$X, R=object$R, method=c("ML", "MixExp", "SVM"),  ...) {

	if(!inherits(object, "predkmeansCVest")) stop("problem!")
method <- match.arg(method)

pkm <- vector("list", length(object$setup))
metrics <- vector("list", length(object$setup))

for (i in 1:length(object$setup)){
	test.set <- object$setup[[i]]$test.set
	training.set <- object$setup[[i]]$training.set

	training.data <- X[training.set, , drop=FALSE]
	test.data <- X[test.set, , drop=FALSE]
    if (!is.null(object$setup[[i]]$scale.center)){
        test.data <- scale(test.data, center=object$setup[[i]]$scale.center, scale=object$setup[[i]]$scale.scale)
    }
    
   	# Create Training and Test Covariates
	training.covars <- R[training.set, object$covarnames, drop=FALSE]
	test.covars <- R[test.set, object$covarnames, drop=FALSE]
	if (object$PCA){
		pcafit <- do.call(createPCAmodelmatrix, args=c(list(data=R[training.set, ,drop=FALSE], matrixonly=FALSE), object$PCAcontrol))
		training.covars <- cbind(training.covars, pcafit$X)
		test.covars <- cbind(test.covars , predict(pcafit$pca, newdata= R[test.set,])[, 1:ncol(pcafit$X), drop=FALSE])
	}
	if (object$TPRS){
	  	tprs.train <- do.call(createTPRSmodelmatrix, args=c(list(data=cbind(training.covars, R[training.set,]),  covarnames=colnames(training.covars),  matrixonly=FALSE), object$TPRScontrol))
	    training.covars <- tprs.train$X
	    test.covars <- predict(tprs.train$gamfit, newdata=cbind(R[test.set,], test.covars), type="lpmatrix")
	} else{
		training.covars <- cbind(1, training.covars)	
		names(training.covars)[1] <- "(Intercept)"
		training.covars <- as.matrix(training.covars)
		test.covars <- cbind(1, test.covars)	
		names(test.covars)[1] <- "(Intercept)"
		test.covars <- as.matrix(test.covars)
	}  

	predfn <- switch(method, ML=predictML.predkmeans, MixExp=predictMixExp.predkmeans, SVM=predictSVM.predkmeans)
	
	

	predobj <- object$pkm[[i]]
	pkm[[i]] <- predfn(object= predobj, Rstar=training.covars, R=test.covars,...)

	metrics[[i]] <- unlist(predictionMetrics(centers= predobj $centers, cluster.pred=pkm[[i]]$test.pred, X=test.data, labels=FALSE))
}

	out <- list(res=pkm, method=method)
	out$metrics <- simplify2array(metrics)

class(out) <- "predkmeansCVpred"
return(out)
}	


##' @export
print.predkmeansCVest <- function(x, ...){
	if(!inherits(x,"predkmeansCVest")){
		stop("x must be of class predkmeansCVest.")
	}
	cat("Cross-validation fit for predictive k-means object with\n" )
	cat("    ", x$K, "Clusters\n")
	cat("    ", length(x$setup), "CV Groups\n")
	cat("Model has:\n")
	if(x$PCA) cat("    ", x$PCAcontrol$ncomps, "PCA components\n")
	if(x$TPRS) cat("    ", x$TPRScontrol$df, "df TPRS\n")
	if(!is.null(x$covarnames)) cat("    ", length(x$covarnames), "Covariates\n")

	invisible(x)
}##print.predkmeansCVest()


##' @export
print.predkmeansCVpred <- function(x, ...){
	if(!inherits(x,"predkmeansCVpred")){
		stop("x must be of class predkmeansCVpred.")
	}
	cat("Cross-validation predictions for predictive k-means object.\n" )
	cat("Predictions computed for", ifelse(x$type=="predkmeans", "predictive", "regular"), "k-means centers,\n")
	cat("using ", x$method, ".\n", sep="")
	invisible(x)
}##print.predkmeansCVpred()



##' @export
summary.predkmeansCVpred <- function(object, ...){
	class(object) <- "summary.predkmeansCVpred"
	object
}##summary.predkmeansCVpred()

##' @export
print.summary.predkmeansCVpred <- function(x, ...){
	cat("Cross-validation predictions for predictive k-means object.\n" )
	cat("Predictions computed for", ifelse(x$type=="predkmeans", "predictive", "regular"), "k-means centers,\n")
	cat("using ", x$method, ".\n", sep="")
	cat("Prediction metrics are:\n")
	print(rowMeans(x$metrics))
}



## Helper function for creating CV groups
#' @name createCVgroups
#' @title Creating k-fold Cross-Validation Groups
#' @description Splits a vector of observation names or indices into a list of k groups, to be used as cross-validation (CV) test groups.
#' @param x vector of observation ID's (character or numeric) to split into cv groups.
#' @param n number of observations to split into cv groups. Defaults to the length of \code{x}, but can also be provided instead of \code{x}.
#' @param k number of cross-validation groups. Must be less than or equal to \code{n}.
#' @param useNames logical indicator of whether the names of 'x' should be used to identify observations within cv groups.
#' @return A list of length \code{k} giving the IDs of observations within each test group.
#' @export
#' @author Joshua Keller
#' @seealso predkmeansCVest predkmeansCVpred
#' @examples
#' # 5-fold groups
#' cv5 <- createCVgroups(n=100, k=5)
#' cv5
#' 
#' # Leave-one-out
#' cvLOO <- createCVgroups(n=100, k=0)
#' cvLOO
createCVgroups <- function(x=NULL, n=length(x), k=10, useNames=TRUE){
	if (is.null(x)){
		x <- 1:n
	}
	if (length(x)!=n) {
		stop("'x' must have length 'n'.")
	}
	if (k==0){
		k <- n
	}
	if (!is.numeric(k) || k<0 || k>n){
		stop("Invalid values of 'k'. Must be between 0 (for leave-one-out CV) and 'n'.")
	}
	dummyorder <- sample(1:n, size=n)
	if (useNames && !is.null(names(x))) {
	    names(dummyorder) <- names(x)[dummyorder]
	}
	cv.groups <- split(dummyorder, f=ceiling(seq_along(dummyorder)/(n/k)))
	cv.groups
}
