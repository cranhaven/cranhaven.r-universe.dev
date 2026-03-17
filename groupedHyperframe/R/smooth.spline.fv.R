

#' @title \link[stats]{smooth.spline} of \link[spatstat.explore]{fv.object}
#' 
#' @param x an \link[spatstat.explore]{fv.object}
#' 
#' @param key,.x \link[base]{character} scalars
#' 
#' @param ... additional parameters of the functions 
#' \link[stats]{smooth.spline}, except for `x`, `y` and `keep.data`
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @importFrom spatstat.explore fvnames as.data.frame.fv
#' @export
smooth.spline.fv <- function(
    x, 
    key = fvnames(fv, a = '.y'), 
    .x = fvnames(fv, a = '.x'),
    ...
) {
  #fv.nm <- substitute(x)
  fv <- x; x <- NULL # make code more readable
  force(key)
  force(.x)
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  #fom <- eval(call(name = '~', as.symbol(key), as.symbol(.x)))
  #lo <- loess(formula = fom, data = as.data.frame.fv(fv), ...)
  ss <- smooth.spline(x = fv[[.x]], y = fv[[key]], keep.data = TRUE, ...)
  ss$call$x <- as.symbol(.x)
  ss$call$y <- as.symbol(key)
  ss$yname <- fv |> 
    attr(which = 'ylab', exact = TRUE) |> 
    deparse1() |>
    sprintf(fmt = '%s smooth.spline')
  return(ss)
}

