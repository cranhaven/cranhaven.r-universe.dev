\name{spGini.w}
\alias{spGini.w}
\title{Spatial Gini coefficient with a given weights matrix}
\description{This is the implementation of the spatial decomposition of the Gini coefficient introduced by Rey and Smith (2013) as in the function spGini. In this function, the calculation of the global Gini and the two components of the spatial Gini is performed using matrix algebra and a ready made weights matrix. Thus, it is possible to use weighting schemes other than those currently supported in spGini.
}
\usage{
spGini.w(x, w)
}

\arguments{
  \item{x}{a numeric vector of a variable}
  \item{w}{Weights Matrix usin w.matrix or other R function}
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
w<-w.matrix(cbind(GR.Municipalities@data$X, GR.Municipalities@data$Y),12,WType='Binary')
spGini.w(GR.Municipalities@data$Income01,w)
}

\keyword{Gini}
\keyword{Spatial Gini}
\keyword{Spatial Inequality}
\keyword{Spatial autocorrelation}
