

#' @title \link[stats]{ksmooth} of \link[spatstat.explore]{fv.object}
#' 
#' @param x an \link[spatstat.explore]{fv.object}
#' 
#' @param key,.x \link[base]{character} scalars
#' 
#' @param ... additional parameters of the functions 
#' \link[stats]{ksmooth}, except for `x` and `y`
#' 
#' @keywords internal
#' @importFrom spatstat.explore fvnames as.data.frame.fv
#' @export
ksmooth.fv <- function(
    x, 
    key = fvnames(fv, a = '.y'), 
    .x = fvnames(fv, a = '.x'),
    ...
) {
  fv.nm <- substitute(x)
  fv <- x; x <- NULL # make code more readable
  force(key)
  force(.x)
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  #fom <- eval(call(name = '~', as.symbol(key), as.symbol(.x)))
  ks <- ksmooth(x = fv[[.x]], y = fv[[key]], ...)
  attr(ks, which = 'x') <- fv[[.x]]
  attr(ks, which = 'xlab') <- .x
  attr(ks, which = 'y') <- fv[[key]]
  attr(ks, which = 'ylab') <- key
  attr(ks, which = 'yname') <- fv |> 
    attr(which = 'ylab', exact = TRUE) |> 
    deparse1() |>
    sprintf(fmt = '%s ksmooth')
  class(ks) <- 'ksmooth'
  return(ks)
}

