#' Spiral plot of phenological parameters 
#' 
#' This utility function yields a spiral plot based on
#' phenological dates estimated from a polygon.
#' 
#' @param   LIST list, containing 6 estimated phenological parameters.
#' @param    MAT matrix, containing 6 estimated phenological parameters. Default, \code{NULL}.
#' @param height numeric, height parameter of \code{\link[spiralize]{spiral_track}} (used internally)
#' @param LABELS character, labels parameter of \code{\link[spiralize]{spiral_axis}} (used internally)
#' @param    ... additional parameters to \code{\link[spiralize]{spiral_initialize}}
#'               
#' @export
#' 
#' @importFrom spiralize spiral_initialize
#' @importFrom spiralize spiral_track
#' @importFrom spiralize spiral_axis
#' @importFrom spiralize spiral_points
#' 
#' @return No value is returned
#' 
#' @seealso \code{\link[sephora]{getSpiralPlot}}, \code{\link[sephora]{phenopar_polygon}}, \code{\link[spiralize]{spiral_track}}, 
#' \code{\link[spiralize]{spiral_axis}}, \code{\link[spiralize]{spiral_initialize}}
#' 
getSpiralPlot <- function(LIST, MAT=NULL, height=0.2, LABELS, ...){
  
  if(!missing(LIST)){
    gu <- getDist_phenoParam(LIST=LIST,phenoParam="GU")
    sos <- getDist_phenoParam(LIST=LIST,phenoParam="SoS")
    mat <- getDist_phenoParam(LIST=LIST,phenoParam="Mat")
    sen <- getDist_phenoParam(LIST=LIST,phenoParam="Sen")
    eos <- getDist_phenoParam(LIST=LIST,phenoParam="EoS")
    dorm <- getDist_phenoParam(LIST=LIST,phenoParam="Dor")
  }
  
  if(!is.null(MAT)){
    gu <- MAT[,1]
    sos <- MAT[,2]
    mat <- MAT[,3]
    sen <- MAT[,4]
    eos <- MAT[,5]
    dorm <- MAT[,6]
  }
  
  season <- cbind(gu, sos, mat, sen, eos, dorm)
  
  spiral_initialize(xlim=c(0, 360*nrow(season)), 
                    start=360+90, end=360*(nrow(season)+1)+90, 
                    reverse=TRUE, ...)
  
  spiral_track(height=height)
  spiral_axis(major_at=seq(0, 360*8, by=30)[1:12], #curved_labels=TRUE,
              labels=LABELS, labels_gp=grid::gpar(cex=1.25, fontface=2)) #
  
  X <- c()
  for(i in 1:nrow(season)){
    X <- c(X, 360 * (season[i,]-1)/364 + (i-1) * 360)
  }
  
  spiral_points(x=X, y=0.5, pch=18, 
                gp=grid::gpar(col=rep(colores,8), cex=1.5))
  
}
