#' This function allows the user to select the most relevant variables thanks to
#' the estimation of their selection frequencies obtained by the stability
#' selection approach.
#' @importFrom stats lm ARMAacf ARMAtoMA acf arima model.matrix pchisq
#' @import glmnet
#' @import parallel
#' @import Matrix
#' @param  X a matrix of covariables
#' @param  Y a response matrix
#' @param  square_root_inv_hat_Sigma   Estimation of the inverse of the square root of the covariance matrix
#' of each row of the residuals matrix obtained by the whitening function.
#' @param  nb_repli numerical, number of replications in the stability selection
#' @param  nb.cores  numerical, number of cores used
#' @param  parallel logical, if TRUE then a parallelized version of the code is used
#' @return A data frame containing the selection frequencies of the different variables obtained
#' by the stability selection, the corresponding level in the design matrix and the associated
#' column of the observations matrix.
#' @examples
#' data("copals_camera")
#' Y <- scale(Y[, 1:50])
#' X <- model.matrix(~ group + 0)
#' residuals <- lm(as.matrix(Y) ~ X - 1)$residuals
#' S12_inv <- whitening(residuals, "AR1", pAR = 1, qMA = 0)
#' Frequencies <- variable_selection(
#'   Y = Y, X = X,
#'   square_root_inv_hat_Sigma = S12_inv,
#'   nb_repli = 10, nb.cores = 1, parallel = FALSE
#' )
#' @export
variable_selection <-
  function(Y, X, square_root_inv_hat_Sigma, nb_repli = 1000,
             parallel = FALSE, nb.cores = 1) {
    p <- ncol(X)
    q <- ncol(Y)
    n <- nrow(Y)
    if(!is.matrix(Y)) Y <- as.matrix(Y)
    if(!is.matrix(X)) X <- as.matrix(X)
    if(!is.matrix(square_root_inv_hat_Sigma)){
      square_root_inv_hat_Sigma <- as.matrix(square_root_inv_hat_Sigma)
    }
    ## Vectorization to obtain a linear model
    Yvec <- as.numeric(Y %*% square_root_inv_hat_Sigma)
    Xvec <- kronecker(t(square_root_inv_hat_Sigma), X)

    ## 10-fold Cross-Validation method to choose lambda
    resultat_cv <- cv.glmnet(Xvec, Yvec,
      family = "gaussian",
      alpha = 1, parallel = parallel
    )
    lambda_min <- resultat_cv$lambda.min

    ## Stability Selection (it may take time if the number of replications is chosen very large and the number of
    ## core is not chosen high enough but it depends on your computer)
    stabsel.glmnet <- function(i) {
      b_sort <- sort(sample(1:(n * q), floor((n * q) / 2)))
      resultat_glmnet <- glmnet(Xvec[b_sort, ],
        Yvec[b_sort],
        family = "gaussian",
        alpha = 1,
        lambda = lambda_min
      )
      ind_glmnet <- which(resultat_glmnet$beta != 0)
      return(tabulate(ind_glmnet, (p * q)))
    }

    res.cum <- Reduce("+", mclapply(1:nb_repli, stabsel.glmnet, mc.cores = nb.cores))

    freq <- res.cum / nb_repli
    if (is.null(colnames(Y))) {
      colnames(Y) <- 1:ncol(Y)
    }
    if (is.null(colnames(X))) {
      colnames(X) <- 1:ncol(X)
    }
    Freqs <- cbind(rep(colnames(Y), each = p), rep(colnames(X), q), as.data.frame(freq))
    names(Freqs) <- c("Names_of_Y", "Names_of_X", "frequency")

    return(Freqs)
  }
