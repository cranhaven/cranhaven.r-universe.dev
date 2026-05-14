\name{jdata}
\alias{jdata}
\docType{data}
\title{Jonckheere's data}
\description{
This synthetic data is reported by Jonckheere (1954). The data consists of 4 groups with 4 observations in each.
}

\usage{jdata}

\format{
  A data frame with 16 observations on the following 2 variables.
  \describe{
    \item{\code{X}}{Number of samples}
    \item{\code{Y}}{Value}
  }
}
\source{
Jonckheere, A. R. (1954). A Distribution-Free k-Sample Test Against Ordered Alternatives. \emph{Biometrika}, \bold{41}, 133-145.
}

\examples{

library(npordtests)

data(jdata)
levels(jdata$X) <- c("I", "II", "III", "IV")
boxplot(Y~X,data=jdata,xlab="Groups")
}
\keyword{datasets}
