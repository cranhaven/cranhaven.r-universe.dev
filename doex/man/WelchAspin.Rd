\name{WA}
\alias{WA}
\title{Welch-Aspin test}
\usage{
WA(data,group)
}
\description{
This function performs the Welch-Aspin test.
}
\examples{
library(doex)
WA(hybrid$data,hybrid$species)
}
\arguments{
  \item{data}{A vector containing the observations to which the treatments are randomly assigned.}
  \item{group}{A numerical or character vector indicating the treatment/control groups.}
}
\value{
  \item{pvalue}{the p-value of the Welch-Aspin test}
}
\author{Mustafa CAVUS}
\references{
Aspin, A.A. (1948) An examination and further development of a formula arising in the problem of comparing two means, Biometrika, 35, 88-96.
}
