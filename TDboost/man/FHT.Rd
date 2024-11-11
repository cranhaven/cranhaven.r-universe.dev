\name{FHT}
\alias{FHT}
\alias{data1}
\alias{data2}
\docType{data}
\title{Simulation data generated based from the FHT model used in Yang, Y., Qian, W. and Zou, H. (2013).}
\description{
There are two data sets, one for training and the other for testing. The training data set has n = 200 observations and p = 6 predictors. The testing data set has n = 20 observations and p = 6 predictors. See details in Friedman et al. (2010).
}
\usage{data(FHT)}
\format{
  Two data frames both contain the following columns:
  \describe{
    \item{X1-X6}{predictor columns}
    \item{Y}{response variable}
  }
}
\references{
Yang, Y., Qian, W. and Zou, H. (2013), \dQuote{A Boosted Tweedie Compound Poisson Model for Insurance Premium} Preprint.
}

\examples{
data(FHT)
}
\keyword{datasets}
