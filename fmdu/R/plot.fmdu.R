#' Visualisation of a fmdu objects
#'
#' Plot method for a \code{fmdu} object. The plot shows the result of \code{fmdu}.
#'
#' @method plot fmdu
#' @param x An fmdu object .
#' @param ... additional arguments to pass
#'
#' @return No return value, called for side effects (plot)
#'
#' @importFrom graphics text abline
#' @export
plot.fmdu <- function( x, ...)
{
  plot( c( x$row.coordinates[,1], x$col.coordinates[,1] ),
        c( x$row.coordinates[,2], x$col.coordinates[,2] ),
        main = "configuration plot",
        sub = "blue = row coordinates, red = column coordinates",
        xlab = "dimension 1",
        ylab = "dimension 2",
        type = "p",
        pch = 20,
        cex = 1,
        col = "darkgray",
        asp = 1.0 )
  xoffset <- 0.04 * sign( x$row.coordinates[,1] ) * mean( abs( range( x$row.coordinates[,1] ) ) )
  yoffset <- 0.04 * sign( x$row.coordinates[,2] ) * mean( abs( range( x$row.coordinates[,2] ) ) )
  text( x$row.coordinates[,1] + xoffset, x$row.coordinates[,2] + yoffset,
        labels = 1:nrow( x$row.coordinates ),
        cex = 0.5,
        col = "blue" )
  xoffset <- 0.04 * sign( x$col.coordinates[,1] ) * mean( abs( range( x$col.coordinates[,1] ) ) )
  yoffset <- 0.04 * sign( x$col.coordinates[,2] ) * mean( abs( range( x$col.coordinates[,2] ) ) )
  text( x$col.coordinates[,1] + xoffset, x$col.coordinates[,2] + yoffset,
        labels = 1:nrow( x$col.coordinates ),
        cex = 0.5,
        col = "red" )

  delta <- as.vector( x$data )
  d <- as.vector( x$distances )
  if ( !is.null( x$weights ) ) {
    nonmissings <- as.vector( x$weights ) != 0.0
    delta <- delta[nonmissings]
    d <- d[nonmissings]
  }

  plot( delta, d,
        main = "fit plot",
        xlab = "data",
        ylab = "distances",
        type = "p",
        pch = 20,
        cex = 1,
        col = "darkgreen",
        cex.main = 0.75,
        asp = 1.0 )
  abline( a = 0, b = 1, col = "black" )

  plot( delta, delta - d,
        main = "residuals plot",
        xlab = "data",
        ylab = "residuals",
        type = "p",
        pch = 20,
        cex = 1,
        cex.main = 0.75,
        col = "gray" )
  abline( h = 0, col = "black" )

}
