#' Estimation of The Generalized Partial Credit Model with DIF
#'
#' This function computes the parameter estimates of a generalized partial credit model with DIF for polytomous responses
#' by using penalized JML estimation.
#'
#' @inheritParams pcm_dif
#'
#' @return
#' \item{X}{   The dataset that is used for estimation.}
#' \item{mt_vek}{   A vector of the highest responses given to items.}
#' \item{itemName}{   The vector of names of items (columns) in the dataset.}
#' \item{loglik}{   The log likelihood of the estimation.}
#' \item{hessian}{   The hessian matrix. Only when the \code{isHessian = TRUE}.}
#' \item{delta}{   A vector of the DIF parameters of each items on each groups.}
#' \item{gamma}{   A vector of the natural logarithm of discrimination parameters of each items.}
#' \item{beta}{   A vector of the difficulty parameter of each items' categories (thresholds).}
#' \item{theta}{   A vector of the ability parameters of each individuals.}
#'
#' @details
#' In the discrimination parameters estimation, instead of estimating the discrimination parameters,
#' we are estimating the natural logarithm of the parameters to avoid negative values, \eqn{\alpha = exp(\gamma)}.
#'
#' @seealso \code{\link{pcm}}, \code{\link{pcm_dif}}, \code{\link{gpcm}}, \code{\link{gpcm_dif}}
#'
#' @examples
#' \dontrun{
#' gpcmdif_res <- gpcm_dif(shortDIF, groups_map = c(rep(1,50),rep(0,50)))
#' summary(gpcmdif_res, par="delta")
#' }
#'
#' @export
gpcm_dif <- function(X, init_par = c(), groups_map = c(), setting = c(), method = c("fast","novel")){

  if(is.null(setting)){
    settingPar <- autoRaschOptions()
  } else {
    if("aR_opt" %in% class(setting)){
      settingPar <- setting
    } else {
      stop("The setting used should be a class of aR_opt!")
    }
  }

  settingPar$isHessian <- FALSE   #GPCM-DIF doesn't need to compute the Hessian for fit statistic
  settingPar$randomized <- TRUE
  if(is.null(setting$optz_method)){
    settingPar$optz_method <- "mixed"
  } else {
    settingPar$optz_method <- setting$optz_method
  }
  if(is.null(groups_map)){
    if(is.null(setting$groups_map)){
      stop("groups_map must be designed to use the PCM-DIF!")
    }
  } else {
    settingPar$groups_map <- as.matrix(groups_map)
  }

  if(!is.null(setting$optim_control)){
    settingPar$optim_control <- setting$optim_control
  }

  result <- pjmle(X = X, init_par = init_par, setting = settingPar, method = method)

  class(result) <- c("gpcmdif","armodels","autoRasch",class(result))
  return(result)

}


#' @param object The object of class \code{'gpcmdif'}.
#' @param ... Further arguments to be passed.
##'
#' @rdname gpcm_dif
#' @export
summary.gpcmdif <- function(object, ...){

  obj <- object

  dotdotdot <- list(...)

  if(!is.null(dotdotdot$par)){
    par <- dotdotdot$par
  } else {
    par <- NULL
  }

  if(!is.null(dotdotdot$th_dif)){
    th_dif <- dotdotdot$th_dif
  } else {
    th_dif <- 1e-5
  }

  if(is.null(par) | "theta" %in% par){
    cat("\n\n")
    cat("The estimated ability scores:")
    cat("\n")
    print(obj$theta)
    cat("\n")
    cat("The highest ability score: ",round(max(obj$theta,na.rm = TRUE),4))
    cat("\n")
    cat("The lowest ability score: ",round(min(obj$theta,na.rm = TRUE),4))
    cat("\n")
    cat("Respondent(s) no. ",paste(obj$exclResp,collapse = ",")," are excluded due to missing (incomplete) background information.")
  }

  if(is.null(par) | "beta" %in% par){
    cat("\n\n")
    cat("The estimated difficulty scores:")
    cat("\n")
    # reported_beta <- obj$beta * obj$real_vek
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
    alpha_mat <- matrix(exp(obj$gamma), ncol = 1, dimnames = list(c(obj$itemName),c("alpha")))
    print(alpha_mat, quote = FALSE)
  }

  if(is.null(par) | "delta" %in% par){
    cat("\n\n")
    cat("The estimated DIF effects (gap of difficulties):")
    cat("\n")
    # if(is.null(th_dif)){
    #   th_dif <- 1e-5
    # }
    # delta_mat <- matrix(round(obj$delta,5), ncol = ncol(obj$groups_map), dimnames = list(c(obj$itemName),c(paste("Group",c(1:ncol(obj$groups_map)),sep = ""))))
    delta_mat <- matrix(round(obj$delta,5), ncol = ncol(obj$groups_map), dimnames = list(c(obj$itemName),c(colnames(obj$groups_map))))
    delta_mat[which(abs(delta_mat) < th_dif)] <- ""
    delta_mat <- as.data.frame(delta_mat)
    remRowIdx <- which(unlist(
      lapply(
        apply(delta_mat,1,function(x){
          which(x != "")
        }),
        function(x){
          if(length(x)!=0){
            return(1)
          } else {
            return(0)
          }
        }
      )
    ) == 1)

    delta_mat <- as.data.frame(delta_mat)[remRowIdx, , drop = FALSE]
    if(nrow(delta_mat) == 0){
      cat("There is no differential functioning in items found.")
    } else {
      print(delta_mat, quote = FALSE)
    }
    cat("\n")
    # cat("DIF effect threshold =",th_dif)
  }
}



#' @param x The object of class \code{'gpcmdif'}.
#'
#' @rdname gpcm_dif
#' @export
print.gpcmdif <- function(x, ...){

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
    print(obj$theta)
    cat("\n")
  }

  if(is.null(par) | "beta" %in% par){
    cat("\n")
    cat("$beta")
    cat("\n")
    print(obj$beta)
    cat("\n")
  }

  if(is.null(par) | "delta" %in% par){
    cat("\n")
    cat("$delta")
    cat("\n")
    print(obj$delta)
    cat("\n")
  }

}
