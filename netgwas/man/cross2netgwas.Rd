\name{cross2netgwas}
\alias{cross2netgwas}

\title{\code{cross} object to \code{netgwas} data frame}

\description{Converts \code{cross} object from R/\pkg{qtl} package to \code{netgwas} dataframe

}

\usage{
  cross2netgwas (cross.obj)
}

\arguments{
  \item{cross.obj}{An object of class \code{cross}.}
}

\value{
An (\eqn{n \times p}) matrix corresponds to a genotype data matrix (\eqn{n} is the sample size and \eqn{p} is the number of variables). This matrix can be as an input data for \code{\link{netmap}}, and \code{\link{netsnp}} functions.
}

\author{ 
  Pariya Behrouzi \cr
  Maintainer: Pariya Behrouzi \email{pariya.behrouzi@gmail.com}\cr
}