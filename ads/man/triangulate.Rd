\encoding{latin1}
\name{triangulate}
\alias{triangulate}
\title{Triangulate polygon}
\description{
 Function \code{triangulate} decomposes a simple polygon (optionally having holes) into contiguous triangles.
}
\usage{
triangulate(outer.poly, holes)
}
\arguments{
  \item{outer.poly}{a list with two component vectors \code{x} and \code{y} giving vertex coordinates of the polygon 
  or a vector \code{(xmin,ymin,xmax,ymax)} giving coordinates \eqn{(ximn,ymin)} and \eqn{(xmax,ymax)} of the origin and the
  opposite corner of a rectangle sampling window (see \code{\link{swin}}).  }
  \item{holes}{(optional) a list (or a list of list) with two component vectors \code{x} and \code{y} giving vertices 
  coordinates of inner polygon(s) delineating hole(s) within the \code{outer.poly}.}
}
\details{
 In argument \code{outer.poly}, the vertices must be listed following boundary of the polygon without any repetition (i.e. do not repeat the first vertex).
 Argument \code{holes} may be a list of vertices coordinates of a single hole (i.e. with \eqn{x} and \eqn{y} component vectors) or a list of list for multiple holes,
 where each \code{holes[[i]]} is a list with \eqn{x} and \eqn{y} component vectors. Holes' vertices must all be inside the \code{outer.poly} boundary (vertices on the boundary 
 are considered outside). Multiple holes do not overlap each others.
 }
\value{
 A list of 6 variables, suitable for using in \code{\link{swin}} and \code{\link{spp}}, and giving the vertices coordinates \eqn{(ax,ay,bx,by,cx,cy)} of the triangles that
 pave the polygon. For a polygon with \emph{t} holes totaling \eqn{n} vertices (outer contour + holes), the number of triangles produced 
is \eqn{(n-2)+2t}, with \eqn{n<200} in this version of the program.
}
\references{
 Goreaud, F. and P?Pelissier, R. 1999. On explicit formula of edge effect correction for Ripley's \emph{K}-function. \emph{Journal of Vegetation Science}, 10:433-438.\cr\cr
 Narkhede, A. & Manocha, D. 1995. Fast polygon triangulation based on Seidel's algorithm. Pp 394-397 In A.W. Paeth (Ed.)
  \emph{Graphics Gems V}. Academic Press. \url{http://www.cs.unc.edu/~dm/CODE/GEM/chapter.html}.
}
\author{
 \email{Raphael.Pelissier@ird.fr}
}
\seealso{
  \code{\link{spp}},
  \code{\link{swin}}
}
\examples{
  data(BPoirier)
  BP <- BPoirier
  plot(BP$poly1$x, BP$poly1$y)
  
  \dontrun{a single polygon triangulation}
  tri1 <- triangulate(BP$poly1)
  plot(swin(BP$rect, tri1))
  
  \dontrun{a single polygon with a hole}
  \donttest{tri2 <- triangulate(c(-10,-10,120,100), BP$poly1)
  plot(swin(c(-10,-10,120,100), tri2))}
}
\keyword{spatial}
