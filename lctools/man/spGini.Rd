\name{spGini}
\alias{spGini}
\title{Spatial Gini coefficient}
\description{This is the implementation of the spatial decomposition of the Gini coefficient introduced by Rey and Smith (2013). The function calculates the global Gini and the two components of the spatial Gini: the inequality among nearest (geographically) neighbours and the inequality of non-neighbours. Three weighted schemes are currently supported: binary, bi-square and row standardised bi-square.}
\usage{
spGini(Coords, Bandwidth, x, WType = 'Binary')
}
\arguments{
  \item{Coords}{a numeric matrix or vector or data frame of two columns giving the X,Y coordinates of the observations (data points or geometric / population weighted centroids)}
  \item{Bandwidth}{a positive integer that defines the number of nearest neighbours for the calculation of the weights}
  \item{x}{a numeric vector of a variable}
  \item{WType}{a string giving the weighting scheme used to compute the weights matrix. 
  Options are: "Binary", "Bi-square", "RSBi-square". Default is "Binary".

  Binary: weight = 1 for distances less than or equal to the distance of the furthest neighbour (H), 0 otherwise;

  Bi-square: weight = (1-(ndist/H)^2)^2 for distances less than or equal to H, 0 otherwise;

  RSBi-square: weight = Bi-square weights / sum (Bi-square weights) for each row in the weights matrix}
}

\value{Returns a list of five values Gini, gwGini, nsGini, gwGini.frac, nsGini.frac

  \item{Gini }{Global Gini}
  \item{gwGini}{First component of the spatial Gini: the inequality among nearest (geographically) neighbours}
  \item{nsGini}{Second component of the spatial Gini: the inequality among non-neighbours}
  \item{gwGini.frac}{The fraction of the first component of the spatial Gini}
  \item{nsGini.frac}{The fraction of the second component of the spatial Gini}
}

\references{
Rey, S.J., Smith, R. J. (2013) A spatial decomposition of the Gini coefficient, Letters in Spatial and Resource Sciences, 6 (2), pp. 55-70.

Kalogirou, S. (2015) Spatial Analysis: Methodology and Applications with R. [ebook] Athens: Hellenic Academic Libraries Link. ISBN: 978-960-603-285-1 (in Greek). \url{https://repository.kallipos.gr/handle/11419/5029?locale=en}
}

\author{Stamatis Kalogirou <stamatis.science@gmail.com>}

\examples{
data(GR.Municipalities)
Coords1<-cbind(GR.Municipalities@data$X, GR.Municipalities@data$Y)
Bandwidth1<-12
x1<-GR.Municipalities@data$Income01
WType1<-'Binary'
spGini(Coords1,Bandwidth1,x1,WType1)
}

\keyword{Gini}
\keyword{Spatial Gini}
\keyword{Spatial Inequality}
\keyword{Spatial autocorrelation}
