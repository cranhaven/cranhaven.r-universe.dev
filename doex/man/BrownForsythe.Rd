\name{BF}
\alias{BF}
\title{Brown-Forsythe test}
\usage{
BF(data,group)
}
\description{
This function performs Brown-Forsythe test.
}
\examples{
library(doex)
BF(hybrid$data,hybrid$species)
}
\arguments{
  \item{data}{A vector containing the observations to which the treatments are randomly assigned.}
  \item{group}{A numerical or character vector indicating the treatment/control groups.}
}
\value{
  \item{pvalue}{the p-value of the Brown-Forsythe test}
}
\author{Mustafa CAVUS}
\references{
Brown, M.B. and Forsythe, A.B. (1974) The small sample behavior of some statistics which test the equality of several
means, Technometrics, 16, 129–132.
}
