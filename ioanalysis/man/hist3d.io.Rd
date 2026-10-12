\name{hist3d.io}
\alias{hist3d.io}

\title{3D Histogram of Input-Output object }
\description{Produces a three dimensional histogram from plot3d}
\usage{
hist3d.io(obj, alpha = 1, phi = 65, theta = 45, limits, 
                   colors = ramp.col(c('yellow', 'violet', 'blue')))
}

\arguments{
  \item{obj}{The nxm matrix to be plotted}
  \item{alpha}{The transparency of bars where 1 is opaque and 0 is complete transparency. Default is 1}
  \item{phi}{Colatitude rotation (shaking head left and right)}
  \item{theta}{Colatitude rotation (nodding up and down)}
  \item{limits}{The lower and upper bound for color limits}
  \item{colors}{A \code{ramp.col()} for the 3D histogram}
}
\details{Uses \code{hist3D} from the package \code{plot3d} to generate a 3D plot
}

\examples{
data(toy.IO)
obj = toy.IO$Z[1:5, 1:5]

hist3d.io(obj, alpha = 0.7)
  
}
