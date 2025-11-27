#' Estimation of The Generalized Partial Credit Model
#'
#' This function computes the parameter estimates of a generalized partial credit model for polytomous responses
#' by using penalized JML estimation. Inputting a dichotomous responses to this model,
#' will automatically transforms the GPCM to the 2-PL model.
#'
#' @inheritParams pcm
#'
#' @return
#' \item{X}{   The dataset that is used for estimation.}
#' \item{mt_vek}{   A vector of the highest response given to items.}
#' \item{itemName}{   The vector of names of items (columns) in the dataset.}
#' \item{loglik}{   The log likelihood of the estimation.}
#' \item{hessian}{   The hessian matrix. Only when the \code{isHessian = TRUE}.}
#' \item{gamma}{   A vector of the natural logarithm of discrimination parameters of each items.}
#' \item{beta}{   A vector of the difficulty parameter of each items' categories (thresholds).}
#' \item{theta}{   A vector of the ability parameters of each individuals.}
#'
#' @details
#' In the discrimination parameters estimation, instead of estimating the discrimination parameters (\eqn{\alpha}),
#' we are estimating its natural logarithm to avoid negative values, \eqn{\alpha = exp(\gamma)}.
#'
#' @seealso \code{\link{pcm}}, \code{\link{gpcm}}
#'
#' @references
#' Muraki, E. (1992). A generalized partial credit model: Application of an EM algorithm. Applied Psychological Measurement, 16(2). https://doi.org/10.1177/014662169201600206
#'
#' @examples
#' gpcm_res <- gpcm(short_poly_data)
#' summary(gpcm_res, par = "alpha")
#'
#' @export
gpcm <- function(X, init_par = c(), setting = c(), method = c("fast","novel")){
  if(is.null(setting)){
    settingPar <- autoRaschOptions()
  } else {
    if("aR_opt" %in% class(setting)){
      settingPar <- setting
    } else {
      stop("The setting used should be a class of aR_opt!")
    }
  }

  settingPar$fixed_par <- c("delta")
  settingPar$isPenalized_delta <- FALSE
  settingPar$optz_method <- "optim"

  if(!is.null(setting$optim_control)){
    settingPar$optim_control <- setting$optim_control
  }

  result <- pjmle(X = X, init_par = init_par, setting = settingPar, method = method)

  class(result) <- c("gpcm","armodels","autoRasch",class(result))
  return(result)
}


#' @param object The object of class \code{'gpcm'}.
#' @param ... Further arguments to be passed.
#'
#' @rdname gpcm
#' @export
summary.gpcm <- function(object, ...){

  obj <- object

  dotdotdot <- list(...)

  if(!is.null(dotdotdot$par)){
    par <- dotdotdot$par
  } else {
    par <- NULL
  }

  if(is.null(par) | "theta" %in% par){
    cat("\n\n")
    cat("The estimated ability scores:")
    cat("\n")
    print(object$theta, ... = ...)
    cat("\n")
    cat("The highest ability score: ",round(max(object$theta,na.rm = TRUE),4))
    cat("\n")
    cat("The lowest ability score: ",round(min(object$theta,na.rm = TRUE),4))
  }

  if(is.null(par) | "beta" %in% par){
    cat("\n\n")
    cat("The estimated difficulty scores:")
    cat("\n")
    reported_beta <- unlist(tapply(obj$beta,rep(seq_along(obj$mt_vek),obj$mt_vek),function(x){
      if(length(x) < max(obj$mt_vek)){
        x <- c(x,rep(NA,(max(obj$mt_vek)-length(x))))
        x
      } else {
        x
      }
    }))
    beta_mat <- matrix(reported_beta, nrow = length(obj$mt_vek), byrow = TRUE)
    beta_mat <- as.data.frame(round(beta_mat,4), row.names = obj$itemName)
    colnames(beta_mat) <- paste("Th_",c(1:max(obj$mt_vek)),sep = "")
    beta_mat[["Item Loc."]] <- temp <- round(apply(beta_mat,1,mean,na.rm=TRUE),4)
    beta_mat$` ` <- apply(beta_mat[,1:max(obj$mt_vek)],1,function(x){if(is.unsorted(na.omit(x))){return("*")}else{return("")}})
    print(beta_mat, quote = FALSE)
    cat("\n")
    cat("The most difficult item: ",obj$itemName[which(temp == max(temp,na.rm = TRUE))])
    cat("\n")
    cat("The easiest item: ",obj$itemName[which(temp == min(temp,na.rm = TRUE))])
    cat("\n")
    ntd_items <- length(which(beta_mat[,ncol(beta_mat)] == "*"))
    cat("There are",ntd_items,"items which have disordered thresholds.")
    cat("\n")
    cat("'*' Item has disordered thresholds.")
  }

  if(is.null(par) | "alpha" %in% par){
    cat("\n\n")
    cat("The estimated discrimination parameters:")
    cat("\n")
    alpha_mat <- matrix(exp(object$gamma), ncol = 1, dimnames = list(c(object$itemName),c("alpha")))
    print(alpha_mat, quote = FALSE, ... = ...)
  }

  # if(is.null(par) | "gamma" %in% par){
  #   cat("\n\n")
  #   cat("The estimated discrimination parameters:")
  #   cat("\n")
  #   alpha_mat <- matrix((obj$gamma), ncol = 1, dimnames = list(c(obj$itemName),c("alpha")))
  #   print(alpha_mat, quote = FALSE)
  # }
}

#' @param x The object of class \code{'gpcm'}.
#'
#' @rdname gpcm
#' @export
print.gpcm <- function(x, ...){

  obj <- x

  dotdotdot <- list(...)

  if(!is.null(dotdotdot$par)){
    par <- dotdotdot$par
  } else {
    par <- NULL
  }

  if(is.null(par) | "theta" %in% par){
    cat("\n")
    cat("$theta")
    cat("\n")
    print(x$theta, ... = ...)
    cat("\n")
  }

  if(is.null(par) | "beta" %in% par){
    cat("\n")
    cat("$beta")
    cat("\n")
    print(x$beta, ... = ...)
    cat("\n")
  }

  if(is.null(par) | "gamma" %in% par){
    cat("\n")
    cat("$gamma")
    cat("\n")
    print(x$gamma, ... = ...)
    cat("\n")
  }

}
