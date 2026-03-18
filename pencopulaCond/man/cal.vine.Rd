\name{cal.vine}
\alias{cal.vine}
\title{Estimating Non-Simplified Vine Copulas Using Penalized Splines}
\description{Calculating the density of the estimated regular vine at the
  point(s) val.}
\usage{cal.vine(obj,val,cores)}
\arguments{
  \item{obj}{Vine object of class 'pencopula'.}
  \item{val}{Values in which the current R-vine should be evaluated.}
  \item{cores}{Default=NULL, the number of cpu cores used for parallel computing can be specified.}

}
\details{The current R-vine is evaluated in val and the corresponding
  density values are returned.}
\value{The returing values are the density of the current R-vine at the
  point(s) 'val'.}
\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}
\references{Estimating Non-Simplified Vine Copulas Using Penalized Splines, Schellhase, C. and Spanhel, F. (2017), Statistics and Computing.
 }
