#' Function to generate imputations using SuperLearner for data with a continuous outcome
#'
#' @param y Vector of observed and missing/imputed values of the variable to be imputed.
#' @param x Numeric matrix of variables to be used as predictors in SuperLearner models
#' with rows corresponding to observed values of the variable to be imputed and
#' columns corresponding to individual predictor variables.
#' @param wy Logical vector. A TRUE value indicates locations in \code{y} that are
#' missing or imputed.
#' @param SL.library Either a character vector of prediction algorithms or a
#' list containing character vectors. A list of functions included in the
#' SuperLearner package can be found with \code{SuperLearner::listWrappers()}.
#' @param bw \code{NULL} or numeric value for bandwidth of kernel function (as standard deviations of the kernel).
#' @param bw.update logical indicating whether bandwidths should be computed
#' every iteration or only on the first iteration.  Default is \code{TRUE},
#' but \code{FALSE} may speed up the run time at the cost of accuracy.
#' @param kernel one of \code{gaussian}, \code{uniform}, or \code{triangular}.
#' Specifies the kernel to be used in estimating the distribution around a missing value.
#' @param ... further arguments passed to \code{SuperLearner()}.
#' @return numeric vector of randomly drawn imputed values.
#'


#Continuous SuperLearner
continuousSuperLearner <- function(y, x, wy, SL.library, kernel, bw, bw.update, ...){
  newdata <- data.frame(x)
  names(newdata) <- sapply(1:ncol(newdata), function(n){paste0("x", n)})

  X <- data.frame(x[!wy,])
  names(X) <- sapply(1:ncol(newdata), function(n){paste0("x", n)})
  Y <- y[!wy]

  args <- c(list(Y = Y, X = X, family = stats::gaussian(),
                 SL.library = SL.library),
            list(...))
  if(is.null(args$parallel)){
    args$parallel = "seq"
  }
  args$type <- NULL
  sl <- do.call(SuperLearner, args[names(args) != "parallel"])
  sl.preds <- predict.SuperLearner(object = sl, newdata = newdata, X = X, Y = Y,
                                   TRUE)$pred

  if(length(bw) == 1 & inherits(bw, c("numeric", "integer"))){
    bw <- as.list(rep(bw, times = sum(wy)))
  }
  else if(!bw.update){
    if(inherits(bw, c("numeric", "integer"))){
        bw <- sapply((1:length(y))[wy], jackknifeBandwidthSelection,
                     bwGrid = bw,
                     preds = sl.preds,
                     y = y,
                     delta = as.numeric(!wy),
                     kernel = kernel)
        bw <- as.list(bw)
    }
    p = parent.frame(2)
    p$args$bw <- bw
  }
  else{
    bw <- sapply((1:length(y))[wy], jackknifeBandwidthSelection,
                 bwGrid = bw,
                 preds = sl.preds,
                 y = y,
                 delta = as.numeric(!wy),
                 kernel = kernel)
    bw <- as.list(bw)
  }


  sapply(1:sum(wy), localImputation, preds = sl.preds, y = y,
         delta = as.numeric(!wy),
         bw = bw, kernel = kernel)
}
