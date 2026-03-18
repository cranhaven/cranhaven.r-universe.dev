\name{weightgain}
\alias{weightgain}
\docType{data}
\title{weight gain in rats}
\description{
  Rats were fed diets with different quantities of protein from either animal or
  plant sources. The weight gained at the end of the experiment was the
  response variable.
}
\usage{data("weightgain")}
\format{
  A data frame with 40 observations on the following 3 variables
  \describe{
    \item{\code{source}}{source of protein given, a factor with levels
      \code{Beef} and \code{Cereal}}
    \item{\code{type}}{amount of protein given, a factor with levels \code{High}
      and \code{Low}}
    \item{\code{weightgain}}{weight gain in grams}
  }
}
\source{

  Hand, D.J., Daly, F., Lunn, A.D., McConway, K.J. and Ostrowski, E. 1994.
  A Handbook of Small Datasets, Chapman and Hall, London.
}
\examples{
  data("weightgain")
  with(weightgain, table(source, type))
}
\keyword{datasets}