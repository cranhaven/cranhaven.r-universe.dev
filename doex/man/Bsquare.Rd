\name{B2}
\alias{B2}
\title{B-square test}
\usage{
B2(alpha,data,group)
}
\description{
This function performs B-square test.
}
\examples{
library(doex)
B2(0.05,hybrid$data,hybrid$species)
}
\arguments{
  \item{alpha}{significance level of the test.}
  \item{data}{A vector containing the observations to which the treatments are randomly assigned.}
  \item{group}{A numerical or character vector indicating the treatment/control groups.}
}
\value{
  \item{p.value}{the p-value of the B-square test}
}
\author{Mustafa CAVUS}
\references{
Özdemir, A.F. and Kurt, S. (2006) One way fixed effect analysis of variance under variance heterogeneity and a solution proposal, Selçuk Journal of Applied Mathematics, 7(2), 81-90.
}
