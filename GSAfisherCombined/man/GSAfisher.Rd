\name{GSAfisher}
\alias{GSAfisher}
\title{GSAfisher}
\description{It is a generic function that dispatches different methods. It provides the p-value for a joint test of association between a phenotype and a set of genetic variants (SNPs) by combining marginal p-values using the Fisher method.}
\usage{GSAfisher(x,\dots)}
\arguments{
  \item{x}{Set of marginal p-values.}
  \item{\dots}{Further arguments passed to other methods.}
}
\details{Use methods("GSAfisher") to get all the methods for the GSAfisher generic.}
\value{UseMethod("GSAfisher")}
\references{
Fisher, R.A. (1925). Statistical Methods for Research Workers. ISBN 0-05-002170-2.
}
\author{Carlos Garcia Prieto}