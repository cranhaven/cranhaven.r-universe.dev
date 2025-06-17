\name{JF}
\alias{JF}
\title{Johansen F-test}
\usage{
JF(data,group)
}
\description{
This function performs Johansen F-test.
}
\examples{
library(doex)
JF(hybrid$data,hybrid$species)
}
\arguments{
  \item{data}{A vector containing the observations to which the treatments are randomly assigned.}
  \item{group}{A numerical or character vector indicating the treatment/control groups.}
}
\value{
  \item{pvalue}{the p-value of the Johansen F-test}
}
\author{Mustafa CAVUS}
\references{
Johansen, S. (1980) Whe Welch-James approximation to the distribution of the residual sum of squares in a weighted linear regression, Biometrika, 67(1), 58-92.
}
