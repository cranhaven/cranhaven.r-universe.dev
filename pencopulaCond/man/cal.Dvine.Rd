\name{cal.Dvine}
\alias{cal.Dvine}
\title{Estimating Non-Simplified Vine Copulas Using Penalized Splines}
\description{Calculating the density of the estimated Dvine at the
  point(s) val.}
\usage{cal.Dvine(obj,val)}
\arguments{
  \item{obj}{object of class 'penDvine', result of 'Dvine'.}
  \item{val}{Values in which the current Dvine should be evaluated.}
}
\details{The current Dvine is evaluated in val and the corresponding
  density values are returned.}
\value{The returing values are the density of the current Dvine at the
  point(s) 'val'.}
\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}
\references{Estimating Non-Simplified Vine Copulas Using Penalized Splines, Schellhase, C. and Spanhel, F. (2017), Statistics and Computing.}
