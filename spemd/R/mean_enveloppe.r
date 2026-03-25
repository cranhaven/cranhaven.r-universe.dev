#' @title return.mean.enveloppe
#' @description Internal function, returns the mean enveloppe of a spatial dataset.
#' @aliases  return.mean.enveloppe
#' @param extrema .
#' @param data .
#' @param zcol Name of the column containing the data.
#' @param method Interpolation method. Currently only `splines` is supported.
#' @param n.pts.spline .
#' @param verbose Prints progress information messages. Defaults to TRUE.
#' @author Pierre Roudier
#' @importFrom MBA mba.points
#' @importFrom sp "coordinates<-"
return.mean.enveloppe <- function(
  extrema,
  data,
  zcol = "z",
  method = "splines",
  n.pts.spline = 3,	# number of points to locally interpolate
  verbose = TRUE
){

  if (method == "splines") {

    min.data <- as.data.frame(list(x=extrema$min$x,y=extrema$min$y,z=extrema$min$value))
    max.data <- as.data.frame(list(x=extrema$max$x,y=extrema$max$y,z=extrema$max$value))
    names(min.data) <- names(max.data) <- c('x','y','z')

    interp.min <- mba.points(min.data, coordinates(data), verbose = FALSE)
    interp.max <- mba.points(max.data, coordinates(data), verbose = FALSE)

    extrema.min.surf <- as.data.frame(interp.min$xyz.est)
    extrema.max.surf <- as.data.frame(interp.max$xyz.est)
    names(extrema.min.surf) <- names(extrema.max.surf) <- c('x','y','z')

  }
  else {
    stop("No other interpolation method that multi-level B splines had been implemented for the moment. Please use the splines option.\n")
  }

  mean.enveloppe <- as.data.frame(coordinates(data))
  mean.enveloppe[[zcol]] <- rowMeans(cbind(extrema.max.surf$z,extrema.min.surf$z))
  coordinates(mean.enveloppe) <- ~x+y

  return(mean.enveloppe)
}
