\name{FA}
\alias{FA}
\title{Fiducial Approach test}
\usage{
FA(data,group,rept)
}
\description{
This function performs Fiducial Approach test.
}
\examples{
library(doex)
FA(hybrid$data,hybrid$species)
}
\arguments{
  \item{data}{A vector containing the observations to which the treatments are randomly assigned.}
  \item{group}{A numerical or character vector indicating the treatment/control groups.}
  \item{rept}{The loop size to perform the test.}
}
\value{
  \item{pvalue}{the p-value of the Fiducial Approach test}
}
\author{Mustafa CAVUS}
\references{
Li, X., Wang, J. and Liang, H. (2011) Comparison of several means: a fiducial based approach, Computational Statistics and Data Analysis, 55, 1993-2002.
}
