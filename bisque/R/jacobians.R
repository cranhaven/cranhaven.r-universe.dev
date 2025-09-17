#' Jacobian for logit transform
#' 
#' Let \eqn{X=logit(Y)} be a transformation of a random variable \eqn{Y} that 
#' lies in the closed interval (L,U).  
#' This function computes the jacobian \eqn{J(x)} when using the density of 
#' \eqn{Y} to evaluate the density of \eqn{X} via
#' \deqn{f(x) = f_y(logit^{-1}(x) * (U-L) + L) J(x)}
#' where 
#' \deqn{J(x) = (U-L) d/dx logit^{-1}(x).}
#' 
#' @export
#' 
#' @param x value at which to evaluate \eqn{J(x)}
#' @param log TRUE to return \eqn{log(J(x))}
#' @param range vector specifying min and max range of the closed interval for 
#'  the logit.  While the logit is defined for real numbers in the unit 
#'  interval, we extend it to real numbers in arbitrary closed intervals (L,U).
#' 
#' @export
#' 
#' @examples 
#' jac.logit(1)
#' 
jac.logit = function(x, log = TRUE, range = c(0,1)) {
  res = x - 2 * log(exp(x) + 1)  + log(diff(range))
  if(log) { res } else { exp(res) }
}


#' Jacobian for logit transform
#' 
#' Let \eqn{X=logit^{-1}(Y)} be a transformation of a random variable \eqn{Y}.  
#' This function computes the jacobian \eqn{J(x)} when using the density of 
#' \eqn{Y} to evaluate the density of \eqn{X} via
#' \deqn{f(x) = f_y(logit(x)) J(x)}
#' where 
#' \deqn{J(x) = d/dx logit(x).}
#' 
#' @export
#' 
#' @param x value at which to evaluate \eqn{J(x)}
#' @param log TRUE to return \eqn{log(J(x))}
#' 
#' @examples 
#' jac.invlogit(1)
#' 
jac.invlogit = function(x, log = TRUE) {
  res = - ( log(x) + log(1-x) )
  if(log) { res } else { exp(res) }
}


#' Jacobian for log transform
#' 
#' Let \eqn{X=log(Y)} be a transformation of a random variable \eqn{Y}.  
#' This function computes the jacobian \eqn{J(x)} when using the density of 
#' \eqn{Y} to evaluate the density of \eqn{X} via
#' \deqn{f(x) = f_y(exp(x)) J(x)}
#' where 
#' \deqn{J(x) = d/dx exp(x).}
#' 
#' @export
#' 
#' @param x value at which to evaluate \eqn{J(x)}
#' @param log TRUE to return \eqn{log(J(x))}
#' 
#' @examples 
#' jac.log(1)
#' 
jac.log = function(x, log = TRUE) {
  if(log) { x } else { exp(x) }
}


#' Jacobian for exponential transform
#' 
#' Let \eqn{X=exp(Y)} be a transformation of a random variable \eqn{Y}.  
#' This function computes the jacobian \eqn{J(x)} when using the density of 
#' \eqn{Y} to evaluate the density of \eqn{X} via
#' \deqn{f(x) = f_y(ln(x)) J(x)}
#' where 
#' \deqn{J(x) = d/dx ln(x).}
#' 
#' @export
#' 
#' @param x value at which to evaluate \eqn{J(x)}
#' @param log TRUE to return \eqn{log(J(x))}
#' 
#' @examples 
#' jac.exp(1)
#' 
jac.exp = function(x, log = TRUE) {
  if(log) { -log(x) } else { 1/x }
}