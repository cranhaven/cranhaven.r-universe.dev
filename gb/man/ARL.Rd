% This is a template R document file
% Distributed under GPL 3 or later

\name{ARL1}
\alias{ARL1}
\alias{ARL0}
\alias{SampleControlChart}

\title{Compute Average Run Length}
\description{
 Compute average run length for control chart.
}
\usage{
 ARL1(x,K,pm1,pI1)
 ARL0(x,ARL0=370,gridsize=20)
}
\arguments{
  \item{x}{An R object generate using \code{kde} function
  from package \code{ks}.}
  \item{K}{a vector of the levels}
  \item{ARL0}{in-control average run length}
  \item{pm1,pI1}{out-of-control parameters for the control
  chart.}
  \item{gridsize}{Gridsize of countour levels to search for ARL.}
}
\author{
  B. Wang  \email{bwang@southalabama.edu}
}
\references{
  Yang, S.F. and Wang, B. ``Using A Kernel Control Region to Monitor
  Both the Process Location and Dispersion''. 
}
\keyword{stats}

