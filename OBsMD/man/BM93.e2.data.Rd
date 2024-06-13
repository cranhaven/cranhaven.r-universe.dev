\name{BM93.e2.data}
\alias{BM93.e2.data}
\docType{data}
\title{Example 2 data in Box and Meyer (1993)}
\description{
12-run Plackett-Burman design for the study of fatigue
    life of weld repaired castings.
}
\usage{data(BM93.e2.data)}
\format{
  A data frame with 12 observations on the following 8 variables.
  \describe{
    \item{A}{a numeric vector. Initial structure factor.}
    \item{B}{a numeric vector. Bead size factor.}
    \item{C}{a numeric vector. Pressure treat factor.}
    \item{D}{a numeric vector. Heat treat factor.}
    \item{E}{a numeric vector. Cooling rate factor.}
    \item{F}{a numeric vector. Polish factor.}
    \item{G}{a numeric vector. Final treat factor.}
    \item{y}{a numeric vector. Natural log of fatigue life response.}
  }
}
\references{
  Hunter, G. B., Hodi, F. S., and Eager, T. W. (1982)
  High-Cycle Fatigue of Weld Repaired Cast Ti-6A1-4V.,
  \emph{Metallurgical Transactions} \bold{13}(9), 1589--1594.


  Box, G. E. P. and Meyer, R. D. (1993)
  Finding the Active Factors in Fractionated Screening Experiments.,
  \emph{Journal of Quality Technology} \bold{25}(2), 94--105.
  \doi{10.1080/00224065.1993.11979432}.

}
\examples{
library(OBsMD)
data(BM93.e2.data,package="OBsMD")
print(BM93.e2.data)
}
\keyword{datasets}
