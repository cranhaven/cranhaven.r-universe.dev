\name{simdata}
\alias{simdata}
\docType{data}
\title{An example dataset to demonstrate the usage of GLIDE
}
\description{
  An example simulation dataset used to demonstrate the usage of GLIDE. 
It is a list composed of 
  a dataframe "simdat" and a dataframe "coeff"
}
\usage{data("simdata")}
\format{
coeff is a one-column dataframe. It consists of exposure coefficients of genotypes of 75 SNPs.
simdat is a data frame consisting of 20000 observations. It has 81 columns, with 75 columns for 
genotypes of 75 SNPs and 6 columns of following covariates:
  \describe{
    \item{\code{age}}{an integer vector to indicate age}
    \item{\code{sex}}{a factor vector to indicate gender}
    \item{\code{pc1}}{a numeric vector of the first principal component of genotypes}
    \item{\code{pc2}}{a numeric vector of the second principal component of genotypes}
    \item{\code{pc3}}{a numeric vector of the third principal component of genotypes}
    \item{\code{outcome}}{a factor vector to indicate the outcome, 1/0}
  }
}
\examples{
  data("simdata")
  ls()
}
\keyword{datasets}
