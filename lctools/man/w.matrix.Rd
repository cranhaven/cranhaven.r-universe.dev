\name{w.matrix}
\alias{w.matrix}
\title{Weights Matrix based on a number of nearest neighbours or a fixed distance}
\description{This function constructs an n by n weights matrix for a geography with n geographical elements (e.g. points or polygons) using a number of nearest neighbours or a fixed distance.}

\usage{
w.matrix(Coords, Bandwidth, WType = "Binary", family = "adaptive")
}

\arguments{
  \item{Coords}{a numeric matrix or vector or data frame of two columns giving the X,Y coordinates of the geographical elements (data points or geometric / population weighted centroids for polygons)}
  \item{Bandwidth}{either a positive integer that defines the number of nearest neighbours for the calculation of the weights of an adaptive kernel (family = 'adaptive') or a fixed distance in meters for a fixed kernel (family = 'fixed').}
  \item{WType}{a string giving the weighting function used to compute the weights matrix. 
  Options are: "Binary", "Bi-square", and "RSBi-square". The default value is "Binary".

  Binary: weight = 1 for distances less than or equal to the distance of the furthest neighbour (H), 0 otherwise;

  Bi-square: weight = (1-(ndist/H)^2)^2 for distances less than or equal to H, 0 otherwise;

  RSBi-square: weight = Bi-square weights / sum (Bi-square weights) for each row in the weights matrix
  }
  \item{family}{a string giving the weighting scheme used to compute the weights matrix. 
  Options are: "adaptive" and "fixed". The default value is "adaptive".
  
  adaptive: the number of nearest neighbours (integer). 
  
  fixed: a fixed distance around each observation's location (in meters).
  }
}
\value{
  A matrix of weights
}
\references{
Kalogirou, S. (2003) The Statistical Analysis and Modelling of Internal Migration Flows within England and Wales, PhD Thesis, School of Geography, Politics and Sociology, University of Newcastle upon Tyne, UK. \url{https://theses.ncl.ac.uk/jspui/handle/10443/204}
}
\author{Stamatis Kalogirou <stamatis.science@gmail.com>}

\seealso{
 \code{\link{moransI.w}}, \code{\link{spGini.w}}
}
\examples{
data(GR.Municipalities)
attr <- GR.Municipalities@data

#adaptive kernel
w.adapt <- w.matrix(cbind(attr$X, attr$Y),6, WType='Binary', family='adaptive')

#fixed kernel
w.fixed <- w.matrix(cbind(attr$X, attr$Y), 50000, WType='Binary', family='fixed')

}
\keyword{Spatial Analysis}
\keyword{Weights Matrix}
