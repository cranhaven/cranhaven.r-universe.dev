\name{grosdata}
\alias{grosdata}
\docType{data}
\title{
"Big" life table with 100 individuals
}
\description{
a life table with 100 individuals to test the time required by jackknife estimation 
}
\usage{data(grosdata)}
\format{
  A data frame with 100 observations on the following 22 variables.
  \describe{
    \item{\code{V1}}{fecundity of day 1}
    \item{\code{V2}}{fecundity of day 2}
    \item{\code{V3}}{fecundity of day 3}
    \item{\code{V4}}{fecundity of day 4}
    \item{\code{V5}}{fecundity of day 5}
    \item{\code{V6}}{fecundity of day 6}
    \item{\code{V7}}{fecundity of day 7}
    \item{\code{V8}}{fecundity of day 8}
    \item{\code{V9}}{fecundity of day 9}
    \item{\code{V10}}{fecundity of day 10}
    \item{\code{V11}}{fecundity of day 11}
    \item{\code{V12}}{fecundity of day 12}
    \item{\code{V13}}{fecundity of day 13}
    \item{\code{V14}}{fecundity of day 14}
    \item{\code{V15}}{fecundity of day 15}
    \item{\code{V16}}{fecundity of day 16}
    \item{\code{V17}}{fecundity of day 17}
    \item{\code{V18}}{fecundity of day 18}
    \item{\code{V19}}{fecundity of day 19}
    \item{\code{V20}}{fecundity of day 20}
    \item{\code{V21}}{fecundity of day 21}
    \item{\code{V22}}{fecundity of day 22}
  }
}
\details{
fictitious data designed to check the time needed and the effect of the m parameter of the jackknife estimation
}
\source{
J.S. Pierre, fictitious
}

\examples{
data(grosdata)
petitr(grosdata)
}
\keyword{datasets}
