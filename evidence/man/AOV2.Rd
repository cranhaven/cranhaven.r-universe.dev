\name{AOV2}
\alias{AOV2}
\docType{data}
\title{ Made-up data for an unbalanced one-way anova. }
\description{
  Made-up data with easy numbers for practicing one-way anova by
  hand to understand how an anova works.
}
\usage{data(AOV2)}
\format{
  A data frame with 22 observations on the following 2 variables:
  \describe{
    \item{\code{y}}{response}
    \item{\code{i}}{predictor: a factor with 4 levels}
  }
}
\details{
  Note that the design is unbalanced.
  }
\source{ van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\examples{
  data(AOV2, package)
  summary(aov(y ~ i, data=AOV2))
}
\keyword{datasets}

