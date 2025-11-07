#' Function to generate imputations using SuperLearner for data with a binary outcome.
#'
#' @param y Vector of observed values of the variable to be imputed.
#' @param x Numeric matrix of variables to be used as predictors in SuperLearner methods with rows corresponding to values in Y.
#' @param wy Logical vector of length \code{length(y)}. A \code{TRUE} value indicates
#' locations in \code{y} for which imputations are created.
#' @param SL.library Either a character vector of prediction algorithms or a list containing character vectors. A list of functions included in the SuperLearner package can be found with SuperLearner::listWrappers().
#' @param ... Further arguments passed to SuperLearner.
#' @return Binary Vector of randomly drawn imputed values.
#'
#' @importFrom stats binomial



#Binary SuperLearner regression
binarySuperLearner = function(y, x, wy, SL.library, ...){
  newdata <- data.frame(x)
  names(newdata) <- sapply(1:ncol(newdata), function(n){paste0("x", n)})

  if(any(!(unique(y) %in% c(0,1)))){
    if(is.character(y)){
      yTemp <- as.factor(y)
      yValues <- levels(yTemp)
      yTemp <- as.numeric(yTemp) - 1
    }
    else if(is.numeric(y)){
      yTemp <- (y - min(y)) / max(y)
      yValues <- unique(yTemp)[order(unique(yTemp))]
    }
    else if(is.factor(y)){
      yValues <- levels(y)
      yTemp <- as.numeric(y) - 1
    }
    else if(is.logical(y)){
      yTemp <- as.numeric(y)
      yValues <- c(FALSE, TRUE)
    }
  }
  else{
    yTemp <- y
    yValues <- c(0,1)
  }

  X <- data.frame(x[!wy,])
  names(X) <- sapply(1:ncol(newdata), function(n){paste0("x", n)})
  Y <- as.numeric(yTemp)[!wy]

  args = c(list(Y = Y, X = X, family = stats::binomial(),
                SL.library = SL.library),
           list(...))
  if(is.null(args$parallel)){
    args$parallel = "seq"
  }
  args$type = NULL
  sl <- do.call(SuperLearner, args[names(args) != "parallel"])

  phat <- predict.SuperLearner(object = sl, newdata = newdata,
                               X = X, Y = Y, TRUE)$pred
  binaryImputations = stats::rbinom(length(phat[wy]), 1, phat[wy])
  if(is.factor(y)){
    return(factor(levels(y)[binaryImputations + 1], levels = levels(y)))
  }
  else{
    return(yValues[binaryImputations + 1])
  }
}
