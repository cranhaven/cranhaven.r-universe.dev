##################################################
## Functions for predicting cluster membership  ##
##################################################
##
## This file contains:
##
##	predictML.predkmeans()
##	predictSVM.predkmeans()
##  predictionMetrics()
##
##


predictML <- function(x) UseMethod("predictML")
predictSVM <- function(x) UseMethod("predictSVM")
predictMixExp <- function(x) UseMethod("predictSVM")



##' @name predictML.predkmeans
##' @aliases predictML.predkmeans predictML 
##' @title Prediction of Cluster Membership
##
##' @description Predicts cluster membership using either multinomial logistic
#'    regression or SVMs.
# 
#' @details Function for predicting cluster membership in clusters identified by k-means or predictive k-means using multinomial logistic regression or support vector machines (SVMs).  For multinomial logitic regression, parameter estimation is handled by \code{mlogit}.  The SVMs are fit using \code{best.svm} from \code{e1071} package.
##'
##' Because this prediction includes return information about cluster assignment
##' and prediction model parameters, this method is deliberately distinct from
##' the generic \code{predict} functions.
##'
##' The \code{predictMixExp} funciton provides predictions from 
##' the 'working' cluster assignments created as part of the 
##' mixture of experts algorithm from \code{predkmeans}.
#
# INPUT:
#' @param   object A predkmeans object, from which the cluster centers will be extracted.
##' @param 	centers	Matrix of cluster centers, assumed to be K-by-p
##' @param 	K Number of clusters
##' @param	R 	matrix of covariates for observations to be predicted at.  
##' @param	Rstar matrix of covariates at training locations
#' @param Xstar matrix of observation at training locations.  Either this or \code{tr.assign} is required.	
#' @param  tr.assign   vector of cluster assignments at training locations.  By default, extracted from \code{object}.
#' @param muStart starting value for cluster centers in mlogit optimization (IDEA: change to pull from predkmeans object?).  If not provided, starting values are selected randomly. 
##' @param maxitMlogit Maximum number of iterations for \code{mlogit} in prediction
##' @param verbose integer indicating amount of output to be displayed
##' @param nMlogitStarts number of mlogit starts to use in estimation of parameters
##' @param mlogit.control list of control parameters to be passes to \code{mlogit}
##' @param ... Unused additional arguments
##
# Output
#' @return A list containing some or all of the following elements:
#'	\item{tr.assign}{Cluster assignments at training locations}
#' 	\item{mlfit}{A subset of the mlogit object returned by the function of that name}
#' \item{beta}{Estimated model parameters}
#' \item{test.pred}{Predicted cluster assignments at test locations}
##
##
##' @author Joshua Keller
##' @export
##' @method predictML predkmeans
##' @importFrom stats predict model.matrix rnorm
##' @seealso \code{\link{mlogit}}, \code{\link{predkmeans}}, \code{\link{predictionMetrics}}
##' @family methods for predkmeans objects
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
##' R <- cbind(1, r1, r2)
##' X <- cbind(x1, x2)
##' pkm <- predkmeans(X=cbind(x1, x2), R=R, K=4)
##' n_pred <- 50
##' Rnew <- cbind(1, r1=rnorm(n_pred), r2=rnorm(n_pred))
##' pkmPred <- predictML(pkm, R=Rnew, Rstar=R)
##' pkmPred$test.pred 
predictML.predkmeans <- function(object=NULL, centers=object$centers, K=nrow(centers), R,  Rstar, Xstar=NULL, tr.assign=object$cluster, muStart ="random", maxitMlogit =500, verbose=1, nMlogitStarts=1,  mlogit.control=list(suppressFittedWarning=TRUE),...){

if (!is.null(object)){
	if(!inherits(object, c("predkmeans"))){
		stop("Input 'object' must be of class predkmeans.")
	}
}
if (K > nrow(centers)){
	stop("K must be less than or equal to number of centers provided")
}
if (K < nrow(centers)){
	warning("K is less than number of centers provided. Using first K centers.")
	centers <- centers[1:K, ]
}
if (is.null(tr.assign)) {
	if (is.null(Xstar)){
		stop("Requires either assigned clusters 'tr.assign' or data 'Xstar' for training observations")
	} else {
		tr.assign <- assignCluster(Xstar, centers= centers)
	}
}

mm.tr.assign <- model.matrix(~0 + factor(tr.assign, levels=1:K)) 
colnames(mm.tr.assign) <- paste0("C", 1:K)
if (muStart=="random"){
	beta.init <- matrix(rnorm((K-1)*ncol(Rstar)* nMlogitStarts), nrow=(K-1)*ncol(Rstar))
} else {
	beta.init <- muStart
}
mlfit <- do.call(mlogit, args=c(list(Y= mm.tr.assign, X=Rstar, beta=beta.init, iterlim=maxitMlogit, verbose=verbose), mlogit.control))
beta <- mlfit$beta
test.pred <- get.cluster.pred(R= R, beta=beta)


out <- list(tr.assign= tr.assign, mlfit=mlfit[c("beta", "fitted", "res.best", "status")], beta=beta, test.pred = test.pred)
return(out)
}


##' @rdname predictML.predkmeans
##' @aliases predictSVM.predkmeans predictSVM 
##
##' @param svm.control list of options for \code{best.svm} 
##
##' @export
# @importFrom e1017 best.svm
# Function for predicting cluster membership in clusters
# identified by k-means or predictive k-means
# using SVMs
#
# INPUT:
#	centers -- Matrix of cluster centers, assumed to be K-by-p
#	R -- matrix of covariates for observations to be predicted at.  
#	X -- matrix of observations at prediction locations (optional -- include if 
#			prediction metrics are desired)
#	
#	Rstar -- matrix of covariates at training locations
#   tr.assign -- Assignments of training data
#   
predictSVM.predkmeans <- function(object=NULL,
centers=object$centers, R,  Rstar, K=nrow(centers), Xstar=NULL, tr.assign =object$cluster,  svm.control=list(gamma=c(1/(2:1), 2), cost=seq(20, 100, by=20)), ...){

if(!requireNamespace("e1071", quietly=TRUE)){
	stop("e1071 is required for SVM prediction.  Please install it.", call.=FALSE)
}


if (!is.null(object)){
	if(!inherits(object, c("predkmeans"))){
		stop("Input 'object' must be of class predkmeans.")
	}
}
if (K > nrow(centers)){
	stop("K must be less than or equal to number of centers provided")
}
if (K < nrow(centers)){
	warning("K is less than number of centers provided. Using first K centers.")
	centers <- centers[1:K, ]
}
if (is.null(tr.assign)) {
	if (is.null(Xstar)){
		stop("Requires either assigned clusters 'tr.assign' or data 'Xstar' for training observations")
	} else {
		tr.assign <- assignCluster(Xstar, centers= centers)
	}
}

if (!all(colnames(R) %in% colnames(Rstar))) {
	stop("Covariate mismatch")
}

	if ("(Intercept)" %in% colnames(Rstar)) {
		Rstar <- Rstar[,-which(colnames(Rstar)=="(Intercept)"), drop=FALSE]
		R <- R[,-which(colnames(R)=="(Intercept)"), drop=FALSE]
	}

	svm.model <- do.call(e1071::best.svm, args=c(list(x= Rstar, y=factor(tr.assign, levels=1:K)), svm.control))
	# Save space, this is a large call, since it explicitly includes the entire variable structure/objects
	svm.model$call <- NULL 
	test.pred <- predict(svm.model, newdata= R)


out <- list(tr.assign= tr.assign, svm.model=svm.model, test.pred = test.pred)
return(out)
}



##' @rdname predictML.predkmeans
##' @aliases predictMixExp.predkmeans predictMixExp
##
##
##' @export
# Function for using fitted values from predictive k-means
# fits to make predictions
#
# Need to add error checking for R
# INPUT:
#	object -- A `predkmeans' object.
#	R -- matrix of covariates for observations to be predicted at.  
#	Rstar -- Not used; included as formal argument for programming convenience.
#   
predictMixExp.predkmeans <- function(object, R, Rstar=NULL, ...){

if (!is.null(object)){
	if(!inherits(object, c("predkmeans"))){
		stop("Input 'object' must be of class predkmeans.")
	}
}

	Rg <- R %*% object$res.best$gamma
	Rg <- apply(Rg, 2, function(x) 1/rowSums(exp(Rg-x)))
	test.pred <- apply(Rg, 1, which.max)


# Return as list to match other functions
return(list(test.pred=test.pred))
}







##' @name predictionMetrics
##' @title Measures of Prediction Performance
##' @description Computes several measures of performance for cluster label prediction.
##' @param centers Matrix of Cluster centers
##' @param cluster.pred Vector of predicted cluster membership. Should be integers
##'			or names corresponding to rows of \code{centers}.
##'	@param X Matrix of observations at prediction locations.
##' @param labels Logical indicating whether cluster prediction and 
##			assignment labels should be returned.
##
##' @return A list with the following elements:
##' \item{MSPE}{Mean squared prediction error. Sum of squared distances between observations and predicted cluster centers.}
##' \item{wSS}{Within-cluster sum-of-squares.  Sum of squared distances between observations at prediction locations and best (i.e. closest) cluster center.}
##' \item{MSME}{Mean squared misclassification error.  Sum of squared distances between predicted cluster center and best (i.e. closest) cluster center.}
##' \item{pred.acc}{Proportion of cluster labels correctly predicted.}
##' \item{cluster.pred}{Predicted cluster assignments (same as argument provided).}
##' \item{cluster.assign}{Integer vector of 'best' cluster assignments (i.e. assignment to closest cluster center)}
##' @export
##'	@author Joshua Keller
##' @seealso \code{\link{predictML}}
##' @references Keller, J.P., Drton, M., Larson, T., Kaufman, J.D., Sandler, D.P., and Szpiro, A.A. (2017). Covariate-adaptive clustering of exposures for air pollution epidemiology cohorts. \emph{Annals of Applied Statistics}, 11(1):93--113.
##' @examples
##' n <- 100
##' d <- 5 # Dimension of exposure
##' K <- 3 # Number of clusters
##' X <- matrix(rnorm(n*d), ncol=d, nrow=n)
##' centers <- matrix(runif(d*K), nrow=K, ncol=d)
##' cluster_pred <- sample(1:K, size=n, replace=TRUE)
##' metrics <- predictionMetrics(centers, cluster.pred=cluster_pred, X=X)
##' metrics[c("MSPE", "wSS", "MSME", "pred.acc")]
predictionMetrics <- function(centers, cluster.pred, X, labels=TRUE){		
	if (!inherits(X,"matrix")) X <- as.matrix(X)
	if (!inherits(centers,"matrix")) centers <- as.matrix(centers)
	if(ncol(centers)!=ncol(X)) stop("Number of columns in 'centers' and 'X' should be the same.")
	if(length(cluster.pred)!=nrow(X)) stop("Length of cluster.pred should equal number of rows in X.")
	K <- nrow(centers)
	if (any(!unique(cluster.pred) %in% c(1:K, rownames(centers)))) stop("cluster.pred must be integers within 1 to K or row names of 'centers'.")
	

	cluster.assign <- assignCluster(X, centers= centers)
	pred.acc <- mean(cluster.pred == cluster.assign)
	MSME <- sum((centers[cluster.pred,] -  centers[cluster.assign,])^2)/nrow(X)
	wSS <- sum((X - centers[cluster.assign,])^2)/nrow(X)
	MSPE <- sum((X - centers[cluster.pred,])^2)/nrow(X)
	
	if (labels) {
		res <- list(MSPE=MSPE, wSS=wSS, MSME=MSME, pred.acc= pred.acc, cluster.pred= cluster.pred, cluster.assign= cluster.assign)	
	} else {
		res <- list(MSPE=MSPE, wSS=wSS, MSME=MSME, pred.acc= pred.acc)	
	}
	res
} # predictionMetrics



##' @export
predictML <- function(object,...) UseMethod("predictML")

##' @export
predictSVM <- function(object,...) UseMethod("predictSVM")

##' @export
predictMixExp <- function(object,...) UseMethod("predictMixExp")

