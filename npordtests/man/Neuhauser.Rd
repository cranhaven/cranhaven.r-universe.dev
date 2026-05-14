\name{neuhauser}
\alias{neuhauser}
\docType{data}
\title{Neuhauser's data}
\description{
This synthetic data is reported by Neuhauser et al. (1998). The data consists of 4 groups with 10 observations per group.
}

\usage{neuhauser}

\format{
  A data frame with 72 observations on the following 2 variables.
  \describe{
    \item{\code{group}}{Group number}
    \item{\code{value}}{Value}
  }
}

\source{
Neuhauser M, Liu PY, Hothorn LA (1998), Nonparametric Tests for Trend: Jonckheere's Test, a Modification and a Maximum Test. \emph{Biom J}, \bold{40}, 899-909.
}
\examples{

library(npordtests)

data(neuhauser)
boxplot(value~group,data=neuhauser,xlab="Groups")
}
\keyword{datasets}
