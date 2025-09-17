# creat folds, from package caret
creatFolds <- function (y, k = 10, list = TRUE, returnTrain = FALSE) {
    if (is.numeric(y)) {
        cuts <- floor(length(y)/k)
        if (cuts < 2) 
            cuts <- 2
        if (cuts > 5) 
            cuts <- 5
        breaks <- unique(quantile(y, probs = seq(0, 1, length = cuts)))
        y <- cut(y, breaks, include.lowest = TRUE)
    }
    if (k < length(y)) {
        y <- factor(as.character(y))
        numInClass <- table(y)
        foldVector <- vector(mode = "integer", length(y))
        for (i in 1:length(numInClass)) {
            min_reps <- numInClass[i]%/%k
            if (min_reps > 0) {
                spares <- numInClass[i]%%k
                seqVector <- rep(1:k, min_reps)
                if (spares > 0) 
                  seqVector <- c(seqVector, sample(1:k, spares))
                foldVector[which(y == names(numInClass)[i])] <- sample(seqVector)
            }
            else {
                foldVector[which(y == names(numInClass)[i])] <- sample(1:k, 
                  size = numInClass[i])
            }
        }
    }
    else foldVector <- seq(along = y)
    if (list) {
        out <- split(seq(along = y), foldVector)
        names(out) <- paste("Fold", gsub(" ", "0", format(seq(along = out))), 
            sep = "")
        if (returnTrain) 
            out <- lapply(out, function(data, y) y[-data], y = seq(along = y))
    }
    else out <- foldVector
    out
}




# log likelihood for cross validation
#' @name negLLrobOmega
#' @title -log Likelihood on test set
#' @description The default evaluation function in corss validation, -log liekihood on test set
#' @param Sigma_hat the estimated *covariance* matrix of training set
#' @param Sigma the *covariance* matrix of test sets
#' @return -log likelihood 
#' @export
negLLrobOmega <- function(Sigma_hat, Sigma){
    determinant(Sigma_hat)$modulus + sum(diag(solve(Sigma_hat, Sigma)))
}


## several helper functions

# this fit the glasso on training and test with a given lambda
glasso_rho <- function(rho, trainset, testset,covest,evaluation,...){
    if( "function" %in% class(covest)){
        S <- covest(trainset)
        S_test <- covest(testset)
    }
    else {
        if("character" %in% class(covest)){
            S <- do.call(covest, args = list(trainset))
            S_test <- do.call(covest, args = list(testset))
        }
        else{
            stop("covest is not callable, reported in glasso_rho, this should not happen, if you see this please file an issue")
        }
    }
       
    
    
    res <- glasso::glasso(S, rho, ...)
    
    if("function" %in% class(evaluation)){
        evaluation(res$w, S_test)
    }
    else{
        if("character" %in% class(evaluation)){
            do.call(evaluation, args = list(res$w, S_test))
        }
        else{
            stop("please provide valid performance evaluation method (function or string)")
        }
    } 
    
}

# this loop over lambdas in a fold
glasso_fold <- function(fold, data, covest, rhos, evaluation, ...){
    trainset <- data[-fold, ]
    testset <- data[fold, ]
    sapply(rhos, glasso_rho, trainset, testset, covest, evaluation, ...)
}

#' @name cvglasso
#' @title Cross validation to chose tuning parameter of glasso
#' @description This routine use k fold cross validation to chose tuning parameter
#' @param data The full dataset, should be a matrix or a data.frame, row as sample
#' @param k number of folds
#' @param covest a *function* or name of a function (string) that takes a matrix to estimate covariance
#' @param rhos a vector of tuning parameter to be tested
#' @param evaluation a *function* or name of a function (string) that takes only two arguments, the estimated covariance and the test covariance, when NULL, we use negative log likelihood on test sets
#' @param ... extra arguments send to glasso
#' @return a matrix with k rows, each row is the evaluation loss of that fold
#' @examples cvglasso(matrix(rnorm(100),20,5))
#' @export
cvglasso <- function(data, k = 10, covest = cov, 
                    rhos = seq(0.1, 1, 0.1), 
                    evaluation = negLLrobOmega, ...){
    data <- as.matrix(data)
    folds <- creatFolds(1:nrow(data), k = k)
    #browser()
    res <- lapply(folds, glasso_fold, data, covest, rhos, evaluation, ...)
    res <- Reduce(rbind, res)
    rownames(res) <- paste0("folds", 1:k)
    return(res)
}


#' @name robglasso
#' @title glasso with robust covariance estimations
#' @description This routine fits glasso using a robust covariance matrix
#' @param data raw data, should be a matrix or a data.frame, row as sample
#' @param covest a *function* or name of a function (string) that takes a matrix to estimate covariance
#' @param rho a scalar or vector of tuning parameters to be chosen, if CV=FALSE, should be a scalar, if CV=TRUE scalar input will be override and tuning parameter will be chosen based on CV
#' @param CV bool, whether doing cross validation for tuning parameter, if set to TRUE and rho is a scalar, the candidate will be chosen automatically by log spacing between 0.01 max covariance and max covariance with number of grids
#' @param k fold for cross validation if applicable
#' @param grids number of candidate tuning parameters in cross validation
#' @param evaluation a *function* or name of a function (string) that takes only two arguments, the estimated *covariance* and the test *covariace*, when NULL, we use negative log likelihood on test sets
#' @param ... extra argument sent to glasso::glasso
#' @return a glasso return (see ?glasso::glasso), most important one is $X the estimated sparse precision,with an extra entry of tuning parameter lambda
#' @examples robglasso(matrix(rnorm(100),20,5))
#' @export
robglasso <- function(data, covest = cov, rho = 0.1, 
                    CV = FALSE, k = 10, grids = 15, evaluation = negLLrobOmega, ...){
    data <- as.matrix(data)
    if( "function" %in% class(covest)){
        S <- covest(data)
        
    }
    else {
        if("character" %in% class(covest)){
            S <- do.call(covest, args = list(data))
        }
        else{
            stop("please provide valid covariance estimation method")
        }
    }
    if(length(rho)!=1 & !CV){
        stop("Provide more than one tuning parameter while not doing cross validation\n")
    }
    if(CV){
        if(length(rho)<=1){
            rhos <- seq(log(0.01*max(S[upper.tri(S)])), log(max(S[upper.tri(S)])), length.out = grids)
            rhos <- exp(rhos)
        }
        else {
           rhos <- rho
        }
        cv_res <- cvglasso(data, k = k, covest = covest, 
                    rhos = rhos, 
                    evaluation = evaluation, ...)
        mean_cv <- colMeans(cv_res)
        rho <- rhos[which(mean_cv==min(mean_cv))[1]]
    }
    res <- glasso(S, rho, ...)
    res$rho <- rho
    return(res)
}

