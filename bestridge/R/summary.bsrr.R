# Various imports
#' @importFrom Rcpp evalCpp
#' @importFrom stats runif
#' @importFrom survival coxph
#' @importFrom survival Surv
#' @importFrom stats glm
#' @importFrom stats lm
#' @importFrom stats rbinom
#' @importFrom stats rpois
#' @importFrom stats rnorm
#' @importFrom stats rexp
#' @importFrom stats deviance
#' @importFrom stats logLik
#' @importFrom stats quantile
#' @importFrom stats coef
#' @importFrom MASS mvrnorm
#' @importFrom Matrix Matrix
#' @importFrom pheatmap pheatmap
#' @importFrom graphics abline
#' @importFrom graphics axis
#' @importFrom graphics box
#' @importFrom graphics grid
#' @importFrom graphics layout
#' @importFrom graphics lines
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics plot.new
#' @importFrom graphics plot.window
#' @importFrom graphics text
#' @importFrom graphics title
NULL

#' summary method for a "bsrr" object
#'
#' Print a summary of the "bsrr" object.
#'
#'
#' @param object A "bsrr" object.
#' @param \dots additional print arguments
#' @seealso \code{\link{bsrr}}.
#' @inherit bsrr return author
#' @return
#' No return value
#' @examples
#'
#'
#' #-------------------linear model----------------------#
#' # Generate simulated data
#' n <- 200
#' p <- 20
#' k <- 5
#' rho <- 0.4
#' seed <- 10
#' Tbeta <- rep(0, p)
#' Tbeta[1:k*floor(p/k):floor(p/k)] <- rep(1, k)
#' Data <- gen.data(n, p, k, rho, family = "gaussian", beta = Tbeta, seed = seed)
#' lambda.list <- exp(seq(log(5), log(0.1), length.out = 10))
#' lm.bsrr <- bsrr(Data$x, Data$y, method = "pgsection")
#'
#' summary(lm.bsrr)
#'
#'#-------------------group selection----------------------#
#' beta <- rep(c(rep(1,2),rep(0,3)), 4)
#' Data <- gen.data(200, 20, 5, rho=0.4, beta = beta, snr = 100, seed =10)
#'
#' group.index <- c(rep(1, 2), rep(2, 3), rep(3, 2), rep(4, 3),
#'                 rep(5, 2), rep(6, 3), rep(7, 2), rep(8, 3))
#' lm.groupbsrr <- bsrr(Data$x, Data$y, s.min = 1, s.max = 8, group.index = group.index)
#'
#' summary(lm.groupbsrr)
#'
#' @method summary bsrr
#' @export
#' @export summary.bsrr
summary.bsrr <-function(object, ...){
  if(is.null(object$bsrr.one)){
    df <- sum(object$beta != 0)
    predictors <- names(which(object$beta!=0))
    a <- rbind(predictors, object$beta[predictors])
    cat("-------------------------------------------------------------------------------------------\n")
    if(object$algorithm_type != "L0L2" & object$algorithm_type != "GL0L2")
    {
      method <- ifelse(object$method == "gsection", "golden section", "sequential")
      cat("    Primal-dual active algorithm with tuning parameter determined by",method, "method", "\n\n")
    }else {
      if(object$method == "sequential"){
        method <- ifelse(object$method == "gsection", "golden section", "sequential")
        cat("    Penalized Primal-dual active algorithm", "\n")
        cat("    with tuning parameter determined by",method, "method", "\n\n")
      } else{
        line.search <- ifelse(object$line.search == "gsection", "golden section", "sequential")
        cat("    Penalized Primal-dual active algorithm with tuning parameter determined by","\n")
        cat("    powell method using",line.search,"method for line search","\n\n")
      }
    }
    if(object$algorithm_type == "PDAS")
      cat("    Best model with k =", df, "includes predictors:", "\n\n") else cat("    Best model with k =", df,"lambda =",object$lambda, "includes predictors:", "\n\n")
    print(object$beta[predictors])
    cat("\n")
    if(logLik(object)>=0)
      cat("    log-likelihood:   ", logLik(object),"\n") else cat("    log-likelihood:  ", logLik(object),"\n")

    if(deviance(object)>=0)
      cat("    deviance:         ", deviance(object),"\n") else cat("    deviance:        ", deviance(object),"\n")
    if(object$ic.type %in% c("AIC", "BIC", "GIC")){
      if(object$ic>=0)
        cat("    ", object$ic.type, ":               ", object$ic,"\n", sep = "") else cat("    ",object$ic.type,":             ", object$ic,"\n", sep = "")

    } else if(object$ic.type == "EBIC"){
      if(object$ic>=0)
        cat("    EBIC:             ", object$ic,"\n") else cat("    EBIC:            ", object$ic,"\n")
    } else{
      if(object$cvm >= 0)
        cat("    cv loss:          ", object$cvm,"\n") else cat("    cv loss:         ", object$cvm,"\n")
    }
    cat("-------------------------------------------------------------------------------------------\n")
  } else{
    df <- sum(object$beta != 0)
    predictors <- names(which(object$beta!=0))
    a <- rbind(predictors, object$beta[predictors])
    cat("----------------------------------------------------------------------------------\n")

    if(object$algorithm_type == "PDAS")
      cat("    Best model with k =", df, "includes predictors:", "\n\n") else cat("    Best model with k =", df,"lambda =",object$lambda, "includes predictors:", "\n\n")
    print(object$beta[predictors])
    cat("\n")
    if(logLik(object)>=0)
      cat("    log-likelihood:   ", logLik(object),"\n") else cat("    log-likelihood:  ", logLik(object),"\n")

    if(deviance(object)>=0)
      cat("    deviance:         ", deviance(object),"\n") else cat("    deviance:        ", deviance(object),"\n")
    if(is.null(object$bsrr.one)){
      if(object$ic.type %in% c("AIC", "BIC", "GIC")){
        if(object$ic>=0)
          cat("    ", object$ic.type, ":               ", object$ic,"\n", sep = "") else cat("    ",object$ic.type,":             ", object$ic,"\n", sep = "")

      } else if(object$ic.type == "EBIC"){
        if(object$ic>=0)
          cat("    EBIC:             ", object$ic,"\n") else cat("    EBIC:            ", object$ic,"\n")
      } else{
        if(object$cvm >= 0)
          cat("    cv loss:          ", object$cvm,"\n") else cat("    cv loss:         ", object$cvm,"\n")
      }
    }

    cat("----------------------------------------------------------------------------------\n")
  }

}

