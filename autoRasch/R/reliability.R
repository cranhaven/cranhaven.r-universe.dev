#' Compute Reliability and Standard Error
#'
#' This function computes the reliability index, separation and the standard error of the models estimation.
#'
#' @param obj Object that resulted from any models estimation, e.g., \code{pcm}, \code{gpcm}, \code{pcmdif}, and \code{gpcmdif}.
#'
#' @return
#' A list of two objects, the reliability and the standard error.
#'
#' \emph{reliability}
#' \itemize{
#'    \item{PRI}{   Person reliability index.}
#'    \item{PSR}{   Person separation reliability.}
#'    \item{IRI}{   Item reliability index.}
#'    \item{ISR}{   Item separation reliability.}
#' }
#' \emph{stdError}
#' \itemize{
#'    \item{var_err_pers}{   A matrix of variance error of the estimation.}
#'    \item{std_err_pers}{   A matrix of standard error of the estimation.}
#'    \item{rmsse_pers}{   Root mean square of the standard error per person.}
#'    \item{var_err_item}{   A matrix of variance error of the estimation.}
#'    \item{std_err_item}{   A matrix of standard error of the estimation.}
#'    \item{rmsse_item}{   Root mean square of the standard error per person.}
#'    \item{hessian_theta}{   Hessian matrix of \code{theta} parameter.}
#'    \item{hessian_beta}{   Hessian matrix of \code{beta} parameter.}
#' }
#'
#' @details
#' Person reliability index
#'
#' @examples
#' pcmObject <- pcm(shortDIF)
#' rel <- checkRel(pcmObject)
#' summary(rel)
#'
#' @rdname reliability
#' @export
checkRel <- function(obj){


  # beta <- obj$beta[which(!is.na(obj$beta))]
  if(is.null(obj$beta.raw)){
    beta <- obj$beta
    method <- "novel"
  } else {
    beta <- obj$beta.raw
    method <- "fast"
  }
  # beta[which(is.na(beta))] <- 0

  if(is.null(obj$hessian)){
    if("gpcmdif" %in% class(obj)){

      settingRel <- autoRaschOptions()

      settingRel$isHessian <- TRUE
      settingRel$optz_method <- "optim"
      settingRel$optim_control <- list(maxit = 0, reltol = 1e-12, fnscale = 1)

      obj_hessian <- autoRasch::gpcm_dif(obj$X, init_par = c(obj$theta,beta,obj$gamma,obj$delta),
                                         groups_map = obj$groups_map, setting = settingRel, method = method)

    } else if("pcmdif" %in% class(obj)){

      settingRel <- autoRaschOptions()

      settingRel$isHessian <- TRUE
      settingRel$optz_method <- "optim"
      settingRel$optim_control <- list(maxit = 0, reltol = 1e-12, fnscale = 1)

      init_par <- c(obj$theta,beta,obj$delta)
      obj_hessian <- autoRasch::pcm_dif(obj$X, init_par = init_par,
                                         groups_map = obj$groups_map, setting = settingRel, method = method)


    } else if("gpcm" %in% class(obj)){

      settingRel <- autoRaschOptions()

      settingRel$isHessian <- TRUE
      settingRel$optz_method <- "optim"
      settingRel$optim_control <- list(maxit = 0, reltol = 1e-12, fnscale = 1)

      obj_hessian <- autoRasch::gpcm(obj$X, init_par = c(obj$theta,beta,obj$gamma),
                                        setting = settingRel, method = method)

    } else if("pcm" %in% class(obj)){

      settingRel <- autoRaschOptions()

      settingRel$isHessian <- TRUE
      settingRel$optz_method <- "optim"
      settingRel$optim_control <- list(maxit = 0, reltol = 1e-12, fnscale = 1)

      obj_hessian <- autoRasch::pcm(obj$X, init_par = c(obj$theta,beta),
                                        setting = settingRel, method = method)

    } else {
      stop("autoRasch ERROR: the separation reliability and standard error can not be computed without Hessian matrix.")
    }

    obj[["hessian"]] <- obj_hessian$hessian

  }

  rmseroor <- stdError(obj)

  rmse <- rmseroor$rmsse_pers
  p_var <- var(obj$theta)
  true_pvar <- p_var - (rmse^2)
  true_psd <- sqrt(true_pvar)
  p_sep_coeff <- true_psd/rmse
  p_rel_idx <- (true_pvar)/(p_var)

  rmse_item <- rmseroor$rmsse_item
  i_var <- var(obj$beta.raw[c(which(!is.na(obj$real_vek)))])
  true_ivar <- i_var - (rmse_item^2)
  true_isd <- sqrt(true_ivar)
  i_sep_coeff <- true_isd/rmse_item
  i_rel_idx <- (true_ivar)/(i_var)

  result <- list("reliability" = list("PRI" = p_sep_coeff, "PSR" = p_rel_idx, "IRI" = i_sep_coeff, "ISR" = i_rel_idx), "stdError" = rmseroor)
  class(result) <- c("seprel","autoRasch",class(result))
  return(result)
}

stdError <- function(obj){

  hess_theta <- obj$hessian[seq_along(obj$theta), seq_along(obj$theta)]
  varerr_p <- (diag(solve(hess_theta)))
  stderr_p <- sqrt(varerr_p)
  rmse_p <- sqrt(mean(varerr_p))

  if(is.null(obj$beta.raw)){
    hess_beta <- obj$hessian[(length(obj$theta)+1):(length(obj$theta)+length(which(!is.na(obj$real_vek)))),(length(obj$theta)+1):(length(obj$theta)+length(which(!is.na(obj$real_vek))))]
  } else {
    lengthBeta <- length(which(!is.na(obj$real_vek)))
    hess_beta <- obj$hessian[(length(obj$theta)+1):(length(obj$theta)+length(obj$beta)),(length(obj$theta)+1):(length(obj$theta)+length(obj$beta))]
  }
  hess_beta <- hess_beta[c(which(!is.na(obj$real_vek))),c(which(!is.na(obj$real_vek)))]
  varerr_i <- (diag(solve(hess_beta)))
  stderr_i <- sqrt(varerr_i)
  rmse_i <- sqrt(mean(varerr_i))

  return(list("var_err_pers" = varerr_p, "std_err_pers" = stderr_p, "rmsse_pers" = rmse_p, "var_err_item" = varerr_i,
              "std_err_item" = stderr_i, "rmsse_item" = rmse_i, "hessian_theta" = hess_theta, "hessian_beta" = hess_beta))
}


#' @param object The object of class \code{'seprel'}.
#' @param ... Further arguments to be passed.
#'
#' @rdname reliability
#' @export
summary.seprel <- function(object,...){
  obj <- object
  res_table <- round(cbind(c(obj$reliability$PRI,obj$reliability$PSR,obj$stdError$rmsse_pers),c(obj$reliability$IRI,obj$reliability$ISR,obj$stdError$rmsse_item)),2)
  dimnames(res_table) <- list(c("Reliability Index","Separation Reliability","RMSSE"),c("Person","Item"))
  cat("\n")
  print(res_table)
  cat("\n")

}

