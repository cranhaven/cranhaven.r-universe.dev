
#' @title Stratified Partition
#' 
#' @description 
#' A variation of \link[caret]{createDataPartition},
#' to split \link[survival]{Surv} \eqn{y} by survival status 
#' instead of the percentiles survival time.
#' 
#' @param y response \eqn{y}, 
# a \link[base]{double}, 
# \link[base]{logical} or \link[base]{factor} \link[base]{vector},
# or 
#' a \link[survival]{Surv} object
#' 
#' @param times positive \link[base]{integer} scalar \eqn{n}, 
#' number of \link[base]{replicate}s of partitions. Default `1L`.
#' 
#' @param p \link[base]{double} scalar between 0 and 1, 
#' percentage \eqn{p} of training subjects, default `.8`
# following nomenclature of 
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' See `vignette('intro', package = 'maxEff')`.
#' 
#' @returns 
#' The function [statusPartition()] returns a length-\eqn{n} \link[stats]{listof}
#' \link[base]{integer} \link[base]{vector}s.
#' In each \link[base]{integer} \link[base]{vector} indicates the training subjects.
#' 
#' @note
#' The function `caTools::sample.split` is not what we need.
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/nonS3/statusPartition.html}
#' 
#' @keywords internal
#' @export 
statusPartition <- function(y, times, p = .8, ...) {
  
  if (!inherits(y, what = 'Surv')) stop('input must be Surv object')
    
  # stratify by censoring status
  if (dim(y)[2L] == 3L) stop('3-col Surv response not supported yet')
  e <- as.logical(y[,2L])
  if (anyNA(e)) stop('do not allow missingness in the status, for now')
  
  idx <- list(
    which(!e), # 'integer' indices of censored events
    which(e) # 'integer' indices of observed events
  )

  ret <- replicate(n = times, expr = {
    idx |> lapply(FUN = \(id) {
      sample(id, size = floor(length(id) * p), replace = FALSE)
    }) |>
      unlist(use.names = FALSE) |>
      sort()
  }, simplify = FALSE)
  class(ret) <- 'listof'
  return(ret)
  
}



