#' Wrapper to abstractly evaluate log-Jacobian functions for transforms
#' 
#' @param x values at which to evaluate \eqn{J(x)}
#' @param link Character vector specifying link function for which the 
#'   inverse link function should be evaluated.  Supports \code{'identity'},
#'   \code{'log'}, and \code{'logit'}.
#' @param linkparams Optional list of additional parameters  for link functions.
#'   For example, the logit function can be extended to allow mappings to any 
#'   closed interval.  There should be one list entry for each link function.  
#'   Specify NA if defaults should be used.
#'   
#' @export
#' 
#' @examples 
#' bisque:::logjac(1, 'logit', list(NA))
#' 
#' @seealso \code{\link{jac.log}}, \code{\link{jac.logit}}
#' 
logjac = function(x, link, linkparams) {
  for(i in 1:length(link)) {
    x[i] = switch (link[i],
                   'identity' = 0,
                   'log' = jac.log(x[i], log = TRUE),
                   'logit' = {
                     range = linkparams[[i]]
                     if(any(is.na(range))) {
                       range = c(0,1)
                     }
                     jac.logit(x[i], log = TRUE, range = range)
                   }
    )
  }
  x
}