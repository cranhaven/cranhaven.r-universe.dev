#' @title extract.extrema
#' @description Internal function, explicitely returns a list with the tri objects of the extrema.
#' @aliases  extract.etxrema
#' @param tri.obj .
#' @param n.extrema.min .
#' @author Pierre Roudier
#'
extract.extrema <- function(
  tri.obj,
  n.extrema.min=1 # Number of extrema beyond which the data is monotone
  ){

  if (!any(class(tri.obj) != 'extrema')) stop('plot.tri.extrema needs a tri and extrema object')

  extrema <- NULL
  i.min <- which(tri.obj$is.minima)
  i.max <- which(tri.obj$is.maxima)
  nb.min <- length(i.min)
  nb.max <- length(i.max)

  if ((nb.min >= n.extrema.min) && (nb.max >= n.extrema.min)) {
    monotone <- FALSE
    pop.min <- data.frame(cbind(tri.obj$x[i.min],tri.obj$y[i.min],tri.obj$value[i.min]))
    pop.max <- data.frame(cbind(tri.obj$x[i.max],tri.obj$y[i.max],tri.obj$value[i.max]))
    names(pop.min)[1] <- names(pop.max)[1] <- 'x'
    names(pop.min)[2] <- names(pop.max)[2] <- 'y'
    names(pop.min)[3] <- names(pop.max)[3] <- 'value'
    extrema$min <- pop.min
    extrema$max <- pop.max
  } else {
    monotone <- TRUE
  }

  return(list(extrema=extrema,nb.min=nb.min,nb.max=nb.max,monotone=monotone))
}
