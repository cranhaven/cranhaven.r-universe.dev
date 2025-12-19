\encoding{latin1}
\name{area.swin}
\alias{area.swin}
\title{Area of a sampling window}
\description{
  Function \code{area.swin} computes the area of a sampling window.
}
\usage{
area.swin(w)
}
\arguments{
  \item{w}{an object of class \code{"swin"} defining the sampling window.}
}
\details{
For \code{"simple"} sampling windows, returns simply the area of the rectangle or circle delineating the study region.\cr
For \code{"complex"} sampling windows, returns the area of the initial rectangle or circle, minus the total area of the 
triangles to remove (see \code{\link{swin}}).
}
\value{
The area of the sampling window.
}
\author{
  \email{Raphael.Pelissier@ird.fr}
}
\seealso{
  \code{\link{swin}}.
}
\examples{
  \dontrun{rectangle of size [0,110] x [0,90]}
  wr<-swin(c(0,0,110,90))
  area.swin(wr)
  
  \dontrun{circle with radius 50 centred on (55,45)}
  wc<-swin(c(55,45,50))
  area.swin(wc)
  
 \dontrun{polygon (diamond shape)}
 t1 <- c(0,0,55,0,0,45)
 t2 <- c(55,0,110,0,110,45)
 t3 <- c(0,45,0,90,55,90)
 t4 <- c(55,90,110,90,110,45)
 wp <- swin(wr, rbind(t1,t2,t3,t4))
 area.swin(wp)
}
\keyword{spatial}
