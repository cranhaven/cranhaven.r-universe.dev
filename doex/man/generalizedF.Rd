\name{GF}
\alias{GF}
\title{Generalized F-test}
\usage{
GF(data,group,rept)
}
\description{
This function performs Generalized F-test.
}
\examples{
library(doex)
GF(hybrid$data,hybrid$species)
}
\arguments{
  \item{data}{A vector containing the observations to which the treatments are randomly assigned.}
  \item{group}{A numerical or character vector indicating the treatment/control groups.}
  \item{rept}{The loop size to perform the test.}
}
\value{
  \item{pvalue}{the p-value of the Generalized F-test}
}
\author{Mustafa CAVUS}
\references{
Weerahandi, S.(1994) ANOVA under unequal error variances, Biometrics, 51, 589-599.
}
