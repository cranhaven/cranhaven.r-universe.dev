\name{WE}
\alias{WE}
\title{Welch F-test}
\usage{
WE(data,group)
}
\description{
This function performs Welch F-test.
}
\examples{
library(doex)
WE(hybrid$data,hybrid$species)
}
\arguments{
  \item{data}{A vector containing the observations to which the treatments are randomly assigned.}
  \item{group}{A numerical or character vector indicating the treatment/control groups.}
}
\value{
  \item{pvalue}{the p-value of the Welch F-test}
}
\author{Mustafa CAVUS}
\references{
Welch, B.L. (1951) On the comparison of several mean values, Biometrika, 38, 330-336.
}
