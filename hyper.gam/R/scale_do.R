
#' @title Alternative \link[base]{scale} Methods
#' 
#' @description
#' Alternative \link[base]{scale} using \link[stats]{median}, \link[stats]{IQR} and \link[stats]{mad}.
#' 
#' @param x \link[base]{numeric} \link[base]{vector}
#' 
#' @param center \link[base]{function}
#' 
#' @param scale \link[base]{function}
#' 
#' @details
#' The function [scale_do()] performs scaling according to user-specified
#' definition of `center` and `scale`. 
#' 
#' @return 
#' The function [scale_do()] returns a \link[base]{numeric} \link[base]{vector} of the same length as `x`.
#' 
#' @examples
#' set.seed(1315); x = rnorm(20)
#' x |> scale_do(center = median, scale = mad) 
#' x |> scale_do(center = median, scale = IQR) 
#' @keywords internal
#' @export
scale_do <- function(x, center, scale) {
  
  if (!is.vector(x, mode = 'numeric')) stop('input must be numeric vector, for now')
  
  if (missing(center) || !is.function(center)) stop('`center` must be a function')
  center_ <- center(x, na.rm = TRUE) # `center` must be a function with argument `na.rm`
  if (!is.numeric(center_) || length(center_) != 1L || is.na(center_)) stop('wrong `center_` calculated')
  
  if (missing(scale) || !is.function(scale)) stop('`scale` must be a function')
  scale_ <- scale(x, na.rm = TRUE) # `scale` must be a function with argument `na.rm`
  if (!is.numeric(scale_) || length(scale_) != 1L || is.na(scale_)) stop('wrong `scale_` calculated')
  
  (x - center_) / scale_
  
}

