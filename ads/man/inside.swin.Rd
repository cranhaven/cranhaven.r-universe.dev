\encoding{latin1}
\name{inside.swin}
\alias{inside.swin}
\title{Test whether points are inside a sampling window}
\description{
 Function \code{inside.swin} tests whether points lie inside or outside a given sampling window.
}
\usage{
inside.swin(x, y, w, bdry=TRUE)
}
\arguments{
  \item{x}{a vector of \code{x} coordinates of points.}
  \item{y}{a vector of \code{y} coordinates of points.}
  \item{w}{an object of class \code{"swin"} (see \code{\link{swin}}) defining the sampling window.}
  \item{bdry}{by default \code{bdry = TRUE}. If \code{FALSE}, points located 
  on the boundary of the sampling window are considered to be outside.}
}
\value{
  A logical vector whose \code{ith} entry is \code{TRUE} if the corresponding point \eqn{(x[i],y[i])} is inside w, \code{FALSE} otherwise.
}
\note{
 For \code{"complex"} sampling windows, points inside the triangles to remove or on their boundary, are considered outside.  
}
\author{
  \email{Raphael.Pelissier@ird.fr}
}
\seealso{
  \code{\link{swin}}.
}
\examples{
  data(BPoirier)
  BP <- BPoirier
  wr <- swin(BP$rect)
  sum(inside.swin(BP$trees$x, BP$trees$y, wr))
  
  wc <- swin(c(55,45,45))
  sum(inside.swin(BP$trees$x, BP$trees$y, wc))
  
  wrt <- swin(BP$rect, triangles=BP$tri1)
  sum(inside.swin(BP$trees$x, BP$trees$y,wrt))
}
\keyword{spatial}