\name{AOV1}
\alias{AOV1}
\docType{data}
\title{ Made-up data for a balanced one-way anova. }
\description{
  Made-up data with easy numbers for practicing one-way anova by
  hand to understand how an anova works.
}
\usage{data(AOV1)}
\format{
  A data frame with 15 observations on the following 2 variables.
  \describe{
    \item{\code{y}}{response}
    \item{\code{i}}{predictor, a factor with 3 levels}
  }
}
\details{
  Note that the design is balanced.
  }
\source{ van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\examples{
  data(AOV1)
  summary(aov(y ~ i, data=AOV1))
}
\keyword{datasets}

