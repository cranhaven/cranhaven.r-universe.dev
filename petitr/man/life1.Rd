\name{life1}
\alias{life1}
\docType{data}
\title{
life table for ten individuals
}
\description{
Daily fecundity of ten individuals (fictitious data) 
}
\usage{data(life1)}
\format{
  A data frame with 10 observations on the following 11 variables.
  \describe{
    \item{\code{X1}}{numeric vector, age}
    \item{\code{X2}}{numeric vector, first individual}
    \item{\code{X3}}{numeric vector, second individual}
    \item{\code{X4}}{numeric vector, ...}
    \item{\code{X5}}{numeric vector, ...}
    \item{\code{X6}}{numeric vector, ...}
    \item{\code{X7}}{numeric vector, ...}
    \item{\code{X8}}{numeric vector, ...}
    \item{\code{X9}}{numeric vector, ...}
    \item{\code{X10}}{numeric vector, ...}
    \item{\code{X11}}{numeric vector, tenth individual}
  }
}
\details{
the first columns stands for the age of the individual. must be sorted in ascending order and represent equal age classes.
}
\examples{
data(life1,life2,life3)
ranova(list(life1,life2,life3))
}
\keyword{datasets}
