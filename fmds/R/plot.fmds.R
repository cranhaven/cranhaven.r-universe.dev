#' Visualisation of an fmds object
#'
#' Plot function for a \code{fmds} object. The plot shows the result of \code{fmds}.
#'
#' @method plot fmds
#' @param x An fmds object .
#' @param type type of plot (configuration by default)
#' @param markers vector or matrix for pie markers
#' @param labels vector with labels
#' @param \dots additional arguments to pass
#'
#' @return none
#'
#' @examples
#' delta <- as.matrix( eurodist )
#' r <- fastmds( delta = delta )
#' plot( r )
#'
#' @importFrom graphics par abline points lines legend text axis mtext polygon
#' @export
plot.fmds <- function( x,
                       type = c( "configuration", "transformation", "fit", "residuals", "shepard", "stress", "biplot", "dendrogram", "threshold", "neighbors" ),
                       markers = NULL,
                       labels = NULL, ...)
{
  oldpar <- par( no.readonly = TRUE )
  on.exit( par( oldpar ) )

  type <- tolower( type )
  type <- match.arg( type, c( "configuration", "transformation", "fit", "residuals", "shepard", "stress", "biplot", "dendrogram", "threshold", "neighbors" ), several.ok = FALSE )

  n <- nrow( x$coordinates )
  p <- ncol( x$coordinates )

  if ( type == "configuration" ) {
    stopifnot( p == 2 )
    xlim=range( x$coordinates[,1] )
    ylim=range( x$coordinates[,2] )
    par( mar = c(5, 4, 4, 4) + 0.0 )
    plot( x$coordinates[,1], x$coordinates[,2],
          main = "configuration plot",
          xlab = "dimension 1",
          ylab = "dimension 2",
          xlim = xlim,
          ylim = ylim,
          type = "p",
          pch = 20,
          cex = ifelse( is.null( markers ), 1, 0 ),
          cex.lab=0.75,
          cex.axis=0.67,
          cex.main=0.75,
          col = "steelblue",
          asp = 1.0 )
    if ( !is.null( labels ) ) {
      xoffset <- 0.04 * sign( x$coordinates[,1] ) * mean( abs( range( x$coordinates[,1] ) ) )
      yoffset <- 0.04 * sign( x$coordinates[,2] ) * mean( abs( range( x$coordinates[,2] ) ) )
      text( x$coordinates[,1] + xoffset, x$coordinates[,2] + yoffset,
            labels = labels,
            xlim = xlim,
            ylim = ylim,
            cex = 0.67,
            col = "slategray" )
    }
    if ( !is.null( markers ) ) {
      pie.slice <- function( x, y, radius, arc1, arc2, ..., fine = 1 ) {
        xb <- seq( arc1, arc2, length.out = ( arc2 * 180 - arc1 * 180) * fine )
        x <- x + c( 0, cospi( 2 * xb ), 0 ) * radius
        y <- y + c( 0, sinpi( 2 * xb ), 0 ) * radius
        polygon( x, y, ... )
      }
      pie.marker <- function( x, y, radii, size = 1.0, color = NULL, ...) {
        nslices <- length( radii )
        steps <- seq( 0.0, 1.0, 1.0 / nslices )
        if ( is.null( color ) ) color = 1:nslices
        for ( k in 1:nslices ) pie.slice( x, y, size * radii[k], steps[k], steps[k+1], col = color[k], fine = 1.0 )
      }
      norm <- function( x ) ( ( x - min( x ) ) / ( max( x ) - min( x ) ) )
      markers <- apply( markers, 2, norm )
      piesize <- ( 2.0 / log10( n ) ) * ( diff( xlim ) + diff( ylim ) ) / 50.0
      for ( i in 1:n ) pie.marker( x$coordinates[i,1], x$coordinates[i,2], markers[i,], size = piesize, color = NULL )
      legend( x = "topleft", legend = colnames( markers ), fill = 1:ncol( markers ), cex = 0.67 )
    }
  }

  lt <- lower.tri( x$data )
  ut <- upper.tri( x$data )
  if ( !is.null( x$weights ) ) {
    nonzeroweights <- x$weights != 0.0
    lt <- lt & nonzeroweights
    ut <- ut & nonzeroweights
  }
  delta <- c( x$data[lt], x$data[ut] )
  gamma <- c( x$transformed.data[lt], x$transformed.data[ut] )
  d <- c( x$distances[lt], x$distances[ut] )
  ix <- sort( delta, index.return = TRUE )$ix
  ix <- ix[sort( gamma[ix], index.return = TRUE )$ix]

  if ( type == "transformation" ) {
    par( mar = c(5, 4, 4, 4) + 0.0 )
    plot( delta, gamma,
          main = "transformation plot",
          xlab = "data",
          ylab = "transformed data",
          type = "p",
          las = 1,
          pch = 20,
          cex = 1,
          cex.lab=0.75,
          cex.axis=0.67,
          cex.main=0.75,
          col = "steelblue" )
    abline( a = 0, b = 1, col = "black" )
    if ( x$knotstype > 0 ) {
      abline( v = range( delta ) )
      if ( x$ninner > 0 ) abline( v = as.vector( x$iknots ) )
    }
  }

  if ( type == "fit" ) {
    par( mar = c(5, 4, 4, 4) + 0.25 )
    plot( gamma, d,
          main = "fit plot",
          xlab = "transformed data",
          ylab = "distances",
          las = 1,
          type = "p",
          pch = 20,
          cex = 1,
          col = "steelblue",
          cex.lab=0.75,
          cex.axis=0.67,
          cex.main=0.75,
          asp = 1.0 )
    abline( a = 0, b = 1, col = "black" )
  }

  if ( type == "residuals" ) {
    par( mar = c(5, 4, 4, 4) + 0.0 )
    plot( delta, gamma - d,
          main = "residuals plot",
          xlab = "data",
          ylab = "residuals",
          type = "p",
          pch = 20,
          cex = 1,
          las = 1,
          cex.lab=0.75,
          cex.axis=0.67,
          cex.main=0.75,
          col = "steelblue" )
    abline( h = 0, col = "black" )
  }

  if ( type == "shepard" ) {
    par( mar = c(5, 4, 4, 4) + 0.25 )
    plot( delta, d,
          xlim=range(delta),
          ylim=range(d),
          main = "Shepard plot",
          xlab = "data",
          ylab = "",
          type = "p",
          pch = 20,
          cex = 1,
          las = 1,
          cex.lab=0.75,
          cex.axis=0.67,
          cex.main=0.75,
          col = "steelblue" )
    mtext( "distances",
           side = 2,
           line = 3,
           col = "steelblue",
           cex=0.67 )
    lines( delta[ix], gamma[ix],
           xlim=range(delta),
           ylim=range(d),
           lwd = 1,
           col = "black" )
    par( new = TRUE )
    plot( delta[ix], gamma[ix],
            xlim=range(delta),
            ylim=range(d),
            axes=FALSE,
            bty="n",
            xlab="",
            ylab = "",
            type = "p",
            pch = 20,
            las = 1,
            cex.lab=0.75,
            cex.axis=0.67,
            cex.main=0.75,
            cex = 1,
            col = "darkolivegreen" )
    axis( 4,
          cex.axis = 0.67 )
    mtext( "transformed data",
           side = 4,
           line = 3,
           col = "darkolivegreen",
           cex = 0.67 )
    par( mfrow = c( 1, 1 ) )
    par( mfcol = c( 1, 1 ) )
  }
}
#  #------------------------------ bubble plot -------------------------
#  if (plot.type == "bubbleplot")
#  {
#
#    if (missing(main)) main <- paste("Bubble Plot") else main <- main
#    if (missing(xlab)) xlab <- paste("Dimension", x1,sep = " ") else xlab <- xlab
#    if (missing(ylab)) ylab <- paste("Dimension", y1,sep = " ") else ylab <- ylab
#
#    if (missing(xlim)) xlim <- range(x$conf[,x1])*1.1
#    if (missing(ylim)) ylim <- range(x$conf[,y1])*1.1
#
#    spp.perc <- x$spp
#    bubsize <- spp.perc/length(spp.perc)*(bubscale + 3)
#
#
#    plot(x$conf, cex = bubsize, main = main, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, asp = asp, ...)
#    xylabels <- x$conf
#    ysigns <- sign(x$conf[,y1])
#    xylabels[,2] <- (abs(x$conf[,y1])-(x$conf[,y1]*(bubsize/50)))*ysigns
#    text(xylabels, rownames(x$conf), pos = 3,cex = 0.7)
#  }
