#########1#########2#########3#########4#########5#########6#########7#########8
#' Plot a 3D Coordinate System
#'
#' Plot a coordinate system in 3D with the origin bottom left.
#'
#' @param th The angle at which the 3D plot should be displayed.
#' @param x Distance from the origin to the maximum x-value.
#' @param y Distance from the origin to the maximum y-value.
#' @param z Distance from the origin to the maximum z-value.
#' @return A matrix containing the plot coordinates (used when adding features).
#' @examples
#' coord3D()
#' @export
################################################################################
coord3D<-function(th=0,x=10,y=10,z=10) {
  x3=matrix(rep(0,(x+1)*(y+1)),x+1)
  graphics::persp(0:x,0:y,matrix(rep(0,(x+1)*(y+1)),x+1),zlim=c(0,z),theta=th,xlab="x",
        ylab="y", zlab="z", ticktype="detailed")
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Add a Vector to a 3D Coordinate System
#'
#' @param pl Matrix containing the current plot coordinates.
#' @param v A vector with 3 entries.
#' @param fr The point at which the vector should start
#'           (defaults to the origin).
#' @param col Color of the vector (defaults to red).
#' @return No return value, called for side effects
#' @examples
#' a=c(2,4,8)
#' b=c(6,0,4)
#' pl=coord3D()
#' vector3D(pl,a)
#' vector3D(pl,b)
#' vector3D(pl,a-b,b,3)
#' @export
################################################################################
vector3D<-function(pl,v,fr=rep(0,3),col="red") {
  if (length(v)!=3) stop("vector must have dimension 3")
  if (length(fr)!=3) stop("starting point must have dimension 3")
  v1=grDevices::trans3d(fr[1],fr[2],fr[3],pmat = pl)
  v2=grDevices::trans3d(v[1]+fr[1],v[2]+fr[2],v[3]+fr[3],pmat = pl)
  graphics::arrows(v1$x,v1$y,v2$x,v2$y,length=.1,col=col)
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Plot Vectors in 3D
#'
#' Plot one or more vectors in a 3D plot at one or more angles. A plot is
#' produced for each entry of \code{th}.
#'
#' @param V Either a vector of length 3 or a matrix with each column a vector
#'          of length 3.
#' @param th A vector indicating the angles at which the plot should be shown.
#' @param V2 A matrix or vector of the same dimensions as V indicating the
#'          starting points of the vectors in V (default is the origin for all).
#' @param col Vector colors; if entered, must have a value for each vector.
#' @return No return value, called for side effects
#' @examples
#' a=c(2,4,8)
#' b=c(6,0,4)
#' oldpar <- par(mfrow=c(3,2))
#' allvectors3D(cbind(a,b,a-b),V2=matrix(c(rep(0,6),b),3))
#' par(oldpar)
#' @export
################################################################################
allvectors3D<-function(V,th=c(0,30,60,90,120,150),V2=NULL,col=NULL) {
  if (inherits(V,"numeric")) V=as.matrix(V)
  if (!inherits(V,"matrix")) stop("V must be a matrix")
  if (nrow(V)!=3) stop("Matrix must have 3 rows")
  if (is.null(V2)) {
    V2=matrix(rep(0,3*ncol(V)),3)
  }
  if (inherits(V2,"numeric")) V2=as.matrix(V2)
  if (sum(dim(V)==dim(V2))!=2) stop ("V2 must have the same dimensions as V")
  if (!is.null(col)&&length(col)!=ncol(V)) stop("Wrong number of colors")
  if (is.null(col)) col=rep(2,ncol(V))
  for (i in 1:length(th)) {
    pl=coord3D(th[i])
    for (j in 1:ncol(V)) {
      vector3D(pl,V[,j],V2[,j],col=col[j])
    }
  }
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Span of a Matrix
#'
#' Displays a perspective plot showing the plane that is the span of a matrix
#'
#' @param M Matrix for which the span should be shown.
#' @param th A vector indicating the horizontal angle at which the plot should
#'  be shown.
#' @param ph A vector indicating the vertical angle at which the plot should be
#'  shown.
#' @return A matrix containing the plot coordinates (used when adding features).
#' @examples
#' span3D(matrix(c(1,0,0,1,1,1),3))
#' @export
################################################################################
span3D<-function(M,th=0,ph=15) {
  if (!inherits(M,"matrix")|!identical(as.numeric(dim(M)),c(3,2))) 
    stop("M must be a 3 by 2 matrix")
  if (Matrix::rankMatrix(M)==1) stop("Columns are dependent")
  ec=matlib::echelon(cbind(t(M),c(0,0)),fractions=T)[,]
  x1=0:5;x2=0:5;eg=expand.grid(x1,x2)
  x3=matrix(ec[1,3]*eg$Var1+ec[2,3]*eg$Var2,length(x1));rownames(x3)=x1
  colnames(x3)=x2
  graphics::persp(x1,x2,x3,theta=th,phi=ph,ticktype="detailed",shade=.1)
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Lines in 3D
#'
#' Plot a line in a 3D plot through a set of points
#'
#' @param pl Matrix containing the current plot coordinates.
#' @param x Vector with x-coordinates.
#' @param y Vector with y-coordinates.
#' @param z Vector with z-coordinates.
#' @param ... additional graphical parameters (see lines()).
#' @return No return value, called for side effects
#' @examples
#' pl=coord3D(30)
#' lines3D(pl,0:10,0:10,rep(0,11))
#' lines3D(pl,0:10,0:10,c(0,2,1,3:8,7,5),col=2)
#' @export
################################################################################
lines3D<-function(pl,x,y,z,...) {
  v=grDevices::trans3d(x,y,z,pmat=pl)
  graphics::lines(v$x,v$y,...)
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Plot Span and Vectors in 3D
#'
#' Plot the span of a matrix plus any vectors in a 3D plot at one or more
#' angles. A plot is produced for each entry of \code{th}.
#'
#' @param M Matrix for which the span should be shown.
#' @param V Either NULL, a vector of length 3, or a matrix with each column a
#'  vector of length 3.
#' @param th A vector indicating the horizontal angle at which the plot should
#'  be shown.
#' @param V2 A matrix or vector of the same dimensions as M indicating the
#'          starting points of the vectors in M (default is the origin for all).
#' @param col Vector colors; if entered, must have a value for each vector.
#' @return No return value, called for side effects
#' @examples
#' M=matrix(c(1,2,4,3,0,2),3)
#' oldpar <- par(mfrow=c(3,2))
#' allspan3D(M,cbind(M,M[,1]-M[,2]),V2=matrix(c(rep(0,6),M[,2]),3),col=c(2,2,1))
#' par(oldpar)
#' @export
################################################################################
allspan3D<-function(M,V=NULL,th=c(-90,-45,0,45,90,135),V2=NULL,col=NULL) {
  if (inherits(M,"numeric")) M=as.matrix(M)
  if (!inherits(M,"matrix")) stop("M must be a matrix")
  if (nrow(M)!=3) stop("M Matrix must have 3 rows")
  if (!is.null(V)) {
    if (inherits(V,"numeric")) V=as.matrix(V)
    if (nrow(V)!=3) stop("V must have 3 rows")
    if (is.null(V2)) {
      V2=matrix(rep(0,3*ncol(V)),3)
    }
    if (sum(dim(V)==dim(V2))!=2) stop ("V2 must have the same dimensions as V")
    if (!is.null(col)&&length(col)!=ncol(V)) stop("Wrong number of colors")
    if (is.null(col)) col=rep(2,ncol(V))
  }
  if (inherits(V2,"numeric")) V2=as.matrix(V2)
  for (i in 1:length(th)) {
    pl=span3D(M,th[i])
    if (!is.null(V)) {
      for (j in 1:ncol(V)) {
        vector3D(pl,V[,j],V2[,j],col=col[j])
      }
    }
  }
}

