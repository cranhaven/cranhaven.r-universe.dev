


#' @title \eqn{xy}-Correlation
#' 
#' @param object a [hyper_gam] object
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @name cor_xy
#' @export
cor_xy <- function(object, ...) UseMethod(generic = 'cor_xy')



#' @rdname cor_xy
#' 
#' @param probs \link[base]{numeric} scalar or \link[base]{vector} \eqn{\tilde{p}}, 
#' taking values between 0 and 1, see function \link[stats]{quantile}.
#' 
#' @details ..
# calculates the sign-adjusted quantile indices in the following steps.
# \enumerate{
# \item Obtain the \link[base]{sign}-adjustment, see section **Details** of function [integrandSurface];
# }
# 
# *Sign-adjusted quantile indices*
# are the product of 
# `sign` (from Step 2) and `gam(.)$linear.predictors` (from Step 1).
# Multiplication by `sign` ensures
# that the sign-adjusted quantile indices
# are positively correlated with the user-selected \eqn{X_{\cdot,j}}.
#' 
#' @returns
#' The `S3` method [cor_xy.hyper_gam()] returns a \link[base]{numeric} scalar or \link[base]{vector} of 
#' \link[stats]{cor}relation(s).
#' 
#' @keywords internal
#' @export cor_xy.hyper_gam
#' @export
cor_xy.hyper_gam <- function(object, probs = .5, ...) { # parameter `xfom` removed
  
  xname <- attr(object, which = 'xname', exact = TRUE)
  if (!is.symbol(xname)) stop('input must carry attr xname')
  
  model <- object$model
  if (!length(model)) stop('should not happen!')
  
  x <- eval(xname, envir = model)
  if (!is.matrix(x)) stop('`x` information should be matrix')
  id <- quantile(seq_len(dim(x)[2L]), probs = probs, type = 3L)
  
  c(cor(
    x = x[, id, drop = FALSE], # ?stats::cor is very beautifully vectorized!
    y = object$linear.predictors,
    use = 'complete.obs'
  ))
  
}
