\name{darwin}
\alias{darwin}
\docType{data}
\title{ Charles Darwin's (1876) data on the fecundity of selfed and
  crossed corn plants. }
\description{
  Charles Darwin(1876) provided data on the difference in the heights attained by selfed and crossed mother plants.
}
\usage{
data(darwin)
}
\format{
  A data frame with 15 observations on the following variable:
  \describe{
    \item{\code{difference}}{the difference in height in inches between
      each paired pair of offspring of a selfed and a crossed mother plant}
  }
}
\source{
  Darwin, C.R. 1876. The effects of cross and self fertilisation in the
  vegetable kingdom. John Murray, London.
  
  van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\examples{
data(darwin)
with(darwin, qqnorm(difference) )
with(darwin, qqline(difference) )
}
\keyword{datasets}

