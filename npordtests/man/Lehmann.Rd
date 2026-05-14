\name{lehmann}
\alias{lehmann}
\docType{data}
\title{Lehmann's data}
\description{
This synthetic data is reported by Lehmann (1975). The data consists of 3 groups with 28, 23 and 21 observations respectively.
}

\usage{lehmann}

\format{
  A data frame with 72 observations on the following 2 variables.
  \describe{
    \item{\code{Group}}{Number of samples}
    \item{\code{Values}}{Value}
  }
}

\source{
Lehmann, E. (1975). \emph{Nonparametrics: Statistical Methods based on Ranks}, Holden-Day, San Francisco, 1st edition, p.12.
}
\examples{

library(npordtests)

data(lehmann)
levels(lehmann$Group) <- c("Undergraduates", "Trainees", "Staff")
boxplot(Values~Group,data=lehmann,xlab="Groups")
}
\keyword{datasets}
