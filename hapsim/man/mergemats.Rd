\name{mergemats}

\alias{mergemats}

\title{Merges two LD matrices}

\description{Merges two LD matrices. It can be used to compare the LD coefficients estimated in the real and simulated data sets}

\usage{mergemats(mat1, mat2)}

\arguments{
  \item{mat1}{First square matrix}
  \item{mat2}{Second square matrix of same dimensions}
}

%\details{}

\value{
 The resulting matrix has upper triangular matrix from \code{mat1} and lower triangular matrix from \code{mat2}
}

\references{
Montana, G. HapSim: a simulation tool for generating haplotype data with
pre-specified allele frequencies and LD coefficients. 2005.
}

\author{Giovanni Montana}

%\note{ ~~further notes~~ }

%\examples{}

\keyword{ utilities }
