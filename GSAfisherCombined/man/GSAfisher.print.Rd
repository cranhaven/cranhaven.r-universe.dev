\name{GSAfisher.print}
\alias{GSAfisher.print}
\title{GSAfisher.print}
\description{
Print method of GSAfisher function that computes the p-value for a joint test of association between a phenotype and a set of genetic variants (SNPs) by combining marginal p-values using the Fisher method and prints the result.
}
\usage{
\method{GSAfisher}{print}(x,\dots)
}
\arguments{
  \item{x}{Numeric set of marginal p-values (one vector or a single p-value).}
  \item{\dots}{Further arguments passed to or from other methods.}
}

\value{
  \item{print.p}{Displays Fisher method combined pvalue.}
}
\author{
Carlos Garcia Prieto
}
\examples{
#Generate 1000 random values from an Uniform distribution U(0, 0.25). 
x<-runif(1000, 0, 0.25)
#Set data class to "print".
class(x)<-"print"
#Apply the GSAfisher.print function to this data vector.
GSAfisher(x)
}