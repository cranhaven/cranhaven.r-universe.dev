#' Runs v-fold cross validation with LLM
#'
#' In \emph{v}-fold cross validation, the data are divided into \emph{v} subsets of
#' approximately equal size. Subsequently, one of the \emph{v} data parts is
#' excluded while the remaider of the data is used to create a logitleafmodel
#' object. Predictions are generated for the excluded data part. The process
#' is repeated \emph{v} times.
#'
#' @param X Dataframe containing numerical independent variables.
#' @param Y Numerical vector of dependent variable. Currently only binary classification is supported.
#' @param cv An integer specifying the number of folds in the cross-validation.
#' @param threshold_pruning Set confidence threshold for pruning. Default 0.25.
#' @param nbr_obs_leaf The minimum number of observations in a leaf node. Default 100.
#' @return An object of class llm.cv, which is a list with the following components:
#' \item{foldpred}{a data frame with, per fold, predicted class membership probabilities for the left-out observations}
#' \item{pred}{a data frame with predicted class membership probabilities.}
#' \item{foldclass}{a data frame with, per fold, predicted classes for the left-out observations.}
#' \item{class}{a data frame with the predicted classes.}
#' \item{conf}{the confusion matrix which compares the real versus the predicted class memberships based on the class object.}
#' @export
#' @references Arno De Caigny, Kristof Coussement, Koen W. De Bock, A New Hybrid Classification Algorithm for Customer Churn Prediction Based on Logistic Regression and Decision Trees, European Journal of Operational Research (2018), doi: 10.1016/j.ejor.2018.02.009.
#' @author Arno De Caigny, \email{a.de-caigny@@ieseg.fr}, Kristof Coussement, \email{k.coussement@@ieseg.fr} and Koen W. De Bock, \email{kdebock@@audencia.com}
#' @seealso \code{\link{predict.llm}}, \code{\link{table.llm.html}}, \code{\link{llm}}
#' @examples
#' ## Load PimaIndiansDiabetes dataset from mlbench package
#' if (requireNamespace("mlbench", quietly = TRUE)) {
#'   library("mlbench")
#' }
#' data("PimaIndiansDiabetes")
#' ## Create the LLM with 5-cv
#' Pima.llm <- llm.cv(X = PimaIndiansDiabetes[,-c(9)],Y = PimaIndiansDiabetes$diabetes, cv=5,
#'  threshold_pruning = 0.25,nbr_obs_leaf = 100)

llm.cv <- function (X, Y, cv, threshold_pruning=0.25, nbr_obs_leaf=100 )
{
	data <- cbind(Y,X)
  setdiff.data.frame <- function(A,B) A[ !duplicated( rbind(B,A) )[ -seq_len(nrow(B))] , ]
	n <- dim(X)[1]
	if(cv <2 | cv>n) {stop("The number of cross-validations should be larger than 2 and smaller than the number of observations")}

	bootstrap <- sample(1:n,replace=FALSE)
	obs_per_fold <- floor(n/cv)
	pred_per_fold <- as.data.frame(array(NA, c(n,cv)))
	pred <- as.data.frame(array(NA, c(n,1)))
	class_per_fold <- as.data.frame(array(NA, c(n,cv)))
	class <- as.data.frame(array(NA, c(n,1)))

    	for (c in 1:cv) {
        	if (c < cv) {fold <- data[bootstrap[(((c-1)*obs_per_fold)+(1:obs_per_fold))],]
    } else {fold <- data[bootstrap[(((c-1)*obs_per_fold):nrow(data))],]}
        	if (c < cv) {fold_ids <- bootstrap[(((c-1)*obs_per_fold)+(1:obs_per_fold))]
		} else {fold_ids <- bootstrap[(((c-1)*obs_per_fold):nrow(data))]}
		traindata_folds <- setdiff.data.frame(data,fold)
		llm_object <- llm(X = traindata_folds[,-1],Y = traindata_folds[,1], threshold_pruning = threshold_pruning, nbr_obs_leaf = nbr_obs_leaf)
		results <- predict.llm(X = fold[,-1],object = llm_object)
		pred_per_fold[fold_ids,c] <- results$probability
		names(pred_per_fold)[[c]] <- paste("fold",c,sep="")
		pred[fold_ids,1] <- results$probability
		class_per_fold[fold_ids,c] <- ifelse(results$probability>0.5, 1,0)
		names(class_per_fold)[[c]] <- paste("fold",c,sep="")
		class[fold_ids,1] <- ifelse(results$probability>0.5, 1,0)
    	}

	conf <- table(as.matrix(class), Y, dnn=c("Predicted Class", "Observed Class"))
	output<- list(foldpred=pred_per_fold,pred=pred,foldclass=class_per_fold,class=class,conf=conf)
}
