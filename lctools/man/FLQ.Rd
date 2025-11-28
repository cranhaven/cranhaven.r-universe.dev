\name{FLQ}
\alias{FLQ}
\title{Focal Location Quotient}
\description{This is the implementation of the Focal Location Quotients proposed by Cromley and Hanink (2012). The function calculates the standard LQ and the Focal LQ based on a kernel of nearest neighbours. Two weighted schemes are currently supported: binary and bi-square weights for a fixed number of nearest neighbours set by the user.}
\usage{
FLQ(Coords, Bandwidth, e, E, Denominator, WType = "Bi-square")
}
\arguments{
  \item{Coords}{a numeric matrix or vector or dataframe of two columns giving the X,Y coordinates of the observations (data points or geometric / population weighted centroids)}
  \item{Bandwidth}{a positive value that defines the number of nearest neighbours for the calculation of the weights}
  \item{e}{a numeric vector of a variable e_i as in the nominator of the Equation 1 (Cromley and Hanink, 2012) referring to the employment in a given sector for each location}
  \item{E}{a numeric vector of a variable E_i as in the nominator of the Equation 1 (Cromley and Hanink, 2012) referring to the total employment in a given sector for each location}
  \item{Denominator}{a ratio as in the denominator (e/E) of the Equation 1 (Cromley and Hanink, 2012), where e and E are total employment in the given sector and overall employment in
the reference economy, respectively.}
  \item{WType}{string giving the weighting scheme used to compute the weights matrix. 
  Options are: "Binary", "Bi-square". Default is "Bi-square".

  Binary: weight = 1 for distances less than or equal to the distance of the furthest neighbour (H), 0 otherwise;

  Bi-square: weight = (1-(ndist/H)^2)^2 for distances less than or equal to H, 0 otherwise
}
}
\value{
FLQ returns a list of 2 vectors:
  \item{LQ}{A numeric vector with the Location Quotient values}
  \item{FLQ}{A numeric vector with the Focal Location Quotient values}
}
\references{
Cromley, R. G. and Hanink, D. M. (2012), Focal Location Quotients: Specification and Application, Geographical Analysis, 44 (4), pp. 398-410. doi: 10.1111/j.1538-4632.2012.00852.x

Kalogirou, S. (2015) Spatial Analysis: Methodology and Applications with R. [ebook] Athens: Hellenic Academic Libraries Link. ISBN: 978-960-603-285-1 (in Greek). \url{https://repository.kallipos.gr/handle/11419/5029?locale=en}
}
\author{Stamatis Kalogirou <stamatis.science@gmail.com>}

\examples{
data(VotesGR)
res<-FLQ(cbind(VotesGR$X, VotesGR$Y),4,VotesGR$NDJune12,VotesGR$AllJune12,0.2966)
boxplot(res)
}
\keyword{LQ}
\keyword{FLQ}
\keyword{Location Quotient}
\keyword{Focal Location Quotient}
