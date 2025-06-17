\name{PB}
\alias{PB}
\title{Parametric Bootstrap test}
\usage{
PB(data,group,rept)
}
\description{
This function performs Parametric Bootstrap test.
}
\examples{
library(doex)
PB(hybrid$data,hybrid$species)
}
\arguments{
  \item{data}{A vector containing the observations to which the treatments are randomly assigned.}
  \item{group}{A numerical or character vector indicating the treatment/control groups.}
  \item{rept}{The loop size to perform the test.}
}
\value{
  \item{pvalue}{the p-value of the Parametric Bootstrap test}
}
\author{Mustafa CAVUS}
\references{
Krishnamoorthy, K., Lu, F., Mathew, T. (2007) A parametric bootstrap approach for anova with unequal variances: Fixed and random models, Computational Statistics and Data Analysis, 51, 5731-5742.
}
