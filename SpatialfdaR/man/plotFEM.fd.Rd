\name{plotFEM.fd}
\alias{plotFEM.fd}
\title{
   Plots an FEM functional data object.
}
\description{
  Plots a FEM object fdobj over a rectangular grid defined by
  vectors Xgrid and Ygrid.
}
\usage{
  plotFEM.fd(fdobj, Xgrid=NULL, Ygrid=NULL, nderivs=rep(0,2), 
                      xlab="X", ylab="Y", zlab="Z", main="",
                      ticktype="detailed", r=10, phi=15, theta=30)
}
\arguments{
  \item{fdobj}{
    A surface defined by a finite element FEM basis.
  }
  \item{Xgrid}{
    A vector of X coordinate values.
  }
  \item{Ygrid}{
    A vector of Y coordinate values.
  }
  \item{nderivs}{A vector of length 2 indicating order of partial derivatives.
  }
  \item{xlab}{A character vector for labelling the X-axis
  when using function \code{persp}.
  }
  \item{ylab}{A character vector for labelling the Y-axis
  when using function \code{persp}.
  }
  \item{zlab}{A character vector for labelling the Z-axis
  when using function \code{persp}.
  }
  \item{main}{A character vector for the title
  when using function \code{persp}.
  }
  \item{ticktype}{A character vector for defining the tick marks
  when using function \code{persp}.
  }
  \item{r}{A positive number for the distance of the eyepoint from the center
  of the plot when using function \code{persp}.
  }
  \item{phi}{A number specifying the collatitude
  when using function \code{persp}.
  }
  \item{theta}{A number specifying the azimuthal direction
  when using function \code{persp}.
  }
}
\value{No return value, called for side effects.}
\seealso{
  \code{\link{plotFEM.mesh}},
  \code{\link{eval.FEM.fd}}
}
\examples{
#  Set up and plot a square with side length one with 32 interior triangles,
#  and then create an order one functional basis object of type FEM.
m <- 4
petList <- squareMesh(m)
pts <- petList$p
edg <- petList$e
tri <- petList$t
pts <- pts/m
plotFEM.mesh(pts, tri)
norder <- 1
FEMbasis <- create.FEM.basis(pts, edg, tri, norder)
#  set up surface values on the nodes of this mesh
data     <- sin(2*pi*pts[,1])*cos(2*pi*pts[,2])
#  smooth the data
FEMList  <- smooth.FEM.basis(pts, data, FEMbasis)
sincosfd <- FEMList$fd
#  set up a 21 by 21 grid of plotting points
Npts <- 21
Xpts <- seq(0,1,len=Npts)
Ypts <- Xpts
#  plot the surface
oldpar <- par(no.readonly = TRUE)
plotFEM.fd(sincosfd, Xpts, Ypts)
#  plot the X partial derivative of the surface
plotFEM.fd(sincosfd, Xpts, Ypts, c(1,0))
#  plot the Y partial derivative of the surface
plotFEM.fd(sincosfd, Xpts, Ypts, c(0,1))
par(oldpar)
}
\keyword{plotFEM.fd}
