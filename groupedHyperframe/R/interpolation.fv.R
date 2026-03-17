

#' @title Interpolation of \link[spatstat.explore]{fv.object}
#' 
#' @param x an \link[spatstat.explore]{fv.object}
#' 
#' @param key,.x \link[base]{character} scalars
#' 
#' @param ... additional parameters of the functions 
#' \link[stats]{approxfun}, \link[stats]{splinefun} and \link[splines]{interpSpline}
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @name interpolation_fv
#' @importFrom spatstat.explore fvnames
#' @export
approxfun.fv <- function(
    x, 
    key = fvnames(fv, a = '.y'), 
    .x = fvnames(fv, a = '.x'),
    ...
) {
  fv <- x; x <- NULL # make code more readable
  force(key)
  force(.x)
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  fn <- approxfun(x = fv[[.x]], y = fv[[key]], ...) # no need to drop additional attributes
  fv |> 
    attr(which = 'ylab', exact = TRUE) |> 
    deparse1() |>
    sprintf(fmt = '%s linear interpolation') |>
    assign(x = 'yname', value = _, envir = environment(fn))
  assign(x = 'xlab', value = .x, envir = environment(fn))
  assign(x = 'ylab', value = key, envir = environment(fn))
  return(fn)
}



#' @rdname interpolation_fv
#' @importFrom spatstat.explore fvnames
#' @export
splinefun.fv <- function(
    x, 
    key = fvnames(fv, a = '.y'), 
    .x = fvnames(fv, a = '.x'),
    ...
) {
  fv <- x; x <- NULL # make code more readable
  force(key)
  force(.x)
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  fn <- splinefun(x = fv[[.x]], y = fv[[key]], ...) # no need to drop additional attributes
  if (FALSE) {
    # fn |> environment() |> ls(envir = _) # only 'z' !!!
    z <- fn |> environment() |> get('z', envir = _)
  }
  fv |> 
    attr(which = 'ylab', exact = TRUE) |> 
    deparse1() |>
    sprintf(fmt = '%s spline interpolation') |>
    assign(x = 'yname', value = _, envir = environment(fn))
  assign(x = 'xlab', value = .x, envir = environment(fn))
  assign(x = 'ylab', value = key, envir = environment(fn))
  return(fn)
}



#' @rdname interpolation_fv
#' @importFrom spatstat.explore fvnames
#' @importFrom splines interpSpline
#' @export
interpSpline_.fv <- function(
    x, 
    key = fvnames(fv, a = '.y'), 
    .x = fvnames(fv, a = '.x'),
    ...
) {
  
  fv <- x; x <- NULL # make code more readable
  force(key)
  force(.x)
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  sp <- interpSpline(obj1 = fv[[.x]], obj2 = fv[[key]], ...)
  
  # overwrite existing
  attr(sp, which = 'formula') <- call(name = '~', as.symbol(key), as.symbol(.x)) |> 
    eval()
    
  # additional
  attr(sp, which = 'yname') <- fv |> 
    attr(which = 'ylab', exact = TRUE) |> 
    deparse1() |>
    sprintf(
      fmt = '%s %s interpolation',
      .x = _, 
      if (inherits(sp, what = 'bSpline')) 'B-spline' else 'piecewise polynomial'
    )
  attr(sp, which = 'x') <- fv[[.x]]
  attr(sp, which = 'y') <- fv[[key]]
  attr(sp, which = 'xlab') <- .x
  attr(sp, which = 'ylab') <- key
  
  return(sp)
  
}


# @importFrom splines periodicSpline # wont work :)
