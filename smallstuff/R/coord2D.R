#########1#########2#########3#########4#########5#########6#########7#########8
#' Plot a 2D Coordinate System
#'
#' Plot a coordinate system in 2D with the origin in the center.
#'
#' @param x Distance from the origin to the maximum x-value.
#' @param y Distance from the origin to the maximum y-value.
#' @return No return value, called for side effects
#' @examples
#' coord2D()
#' @export
################################################################################
coord2D<-function(x=5,y=5) {
  x=x[1];y=y[1]
  if (!isInt(x)|x<1) stop("x must be an integer greater than 0")
  if (!isInt(y)|y<1) stop("y must be an integer greater than 0")
  plot(1,type='n',xlim=c(x*-1,x),ylim=c(y*-1,y),axes=F,xlab="",ylab="",asp=1)
  graphics::axis(1,(x*-1):x,(x*-1):x,cex.axis=.7,pos=0,padj=-2,tck=-.01)
  lab=c(as.character((y*-1):-1),"",paste(" ",as.character(1:y)))
  graphics::axis(2,(y*-1):y,lab,cex.axis=.7,pos=0,hadj=0, tck=-.01,las=1)
  return(NULL)
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Add a Vector to a 2D Coordinate System
#'
#' @param v A vector with 2 entries.
#' @param fr Vector containing the point at which the vector should start
#'           (defaults to the origin).
#' @param col Color of the vector (defaults to red).
#' @return No return value, called for side effects
#' @examples
#' a=c(2,4)
#' b=c(0,3)
#' coord2D()
#' vector2D(a)
#' vector2D(b)
#' vector2D(a-b,b,"blue")
#' @export
################################################################################
vector2D<-function(v,fr=c(0,0),col=2) {
  if (!inherits(v,c("integer","numeric"))) stop("v must be numeric")
  if (!inherits(fr,c("integer","numeric"))) stop("fr must be numeric")
  if (length(v)!=2) stop("vector must have dimension 2")
  if (length(fr)!=2) stop("starting point must have dimension 2")
  v2=c(v[1]+fr[1],v[2]+fr[2])
  graphics::arrows(fr[1],fr[2],v2[1],v2[2],length=.1,col=col)
}
