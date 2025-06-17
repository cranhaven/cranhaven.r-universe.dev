\name{AGF}
\alias{AGF}
\title{Alvandi et al. Generalized F-test}
\usage{
AGF(data,group,rept)
}
\description{
This function performs Alvandi et al. Generalized F-test.
}
\examples{
library(doex)
AGF(hybrid$data,hybrid$species,10000)
}
\arguments{
  \item{data}{A vector containing the observations to which the treatments are randomly assigned.}
  \item{group}{A numerical or character vector indicating the treatment/control groups.}
  \item{rept}{The loop size to perform the test.}
}
\value{
  \item{pvalue}{the p-value of the Alvandi et al. Generalized F-test}
}
\author{Mustafa CAVUS}
\references{
Sadooghi-Alvandi, S.M., Jafari, A.A., Mardani-Fard, H.A. (2012) One-way ANOVA with unequal variances, Communications in Statistics: Theory and Methods, 41, 4200-4221.
}
