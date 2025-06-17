\name{hiv}
\alias{hiv}
\docType{data}
\title{
HIV data
}
\description{
Light absorbance for positive control samples in an ELISA test for HIV (Hoaglin et al., 1991).
}
\usage{data(hiv)}
\format{
  A data frame with 75 observations on the following 3 variables.
  \describe{
    \item{\code{Absorbance}}{Measurement of absorbance of light (dimensionless)}
    \item{\code{Lot}}{Five levels of lot}
    \item{\code{Run}}{Five levels of run}
  }
}

\source{
Hoaglin, D.C., Mosteller, F. and Tukey, J.W. (1991) Fundamentals of Exploratory Analysis of Variance, Wiley, New York, p.107.
}

\examples{
data(hiv)
head(hiv)
}
\keyword{datasets}
