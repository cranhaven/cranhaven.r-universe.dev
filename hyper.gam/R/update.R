


#' @title \link[stats]{update} a [hyper_gam] model
#' 
#' @param object a [hyper_gam] model
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' The `S3` method [update.hyper_gam()] returns a [hyper_gam] model
#' 
#' @keywords internal
#' @export update.hyper_gam
#' @export
update.hyper_gam <- function(object, ...) {
  # we do not have any ?mgcv:::update.*
  # 'gam' object is typically c("gam", "glm", "lm")
  # we do not have many ?stats:::update.*
  
  # `update(gam)` invokes ?stats::update.default, as of 2023-12-20 packageDate('mgcv')
  ret <- update.default(object, ...)
  # ret <- NextMethod(generic = 'update') # error
  
  attr(ret, which = 'xname') <- attr(object, which = 'xname', exact = TRUE) # needed by [cor_xy.hyper_gam]
  class(ret) <- c('hyper_gam', class(ret))
  return(ret)
}
