% This is a template R document file
% Distributed under GPL 3 or later

\name{fkde}
\alias{fkde}
\alias{ControlChart}
\alias{contour.ControlChart}
\alias{persp.ControlChart}
\alias{plot.ControlChart}
\alias{print.ControlChart}

\title{Estimate Asymptotic Joint Distribution of EWMA variables}
\description{
 Estimate Asymptotic Joint Distribution of EWMA variables
 for control chart.
}
\usage{
  fkde(n=5, pm0=0.5, pI0=0.2, lambda=0.05,
       gridsize=100,B=10000,T=10000)
}
\arguments{
  \item{n}{sample size.}
  \item{lambda}{a parameter to compute EWMA}
  \item{pm0,pI0}{in-control parameters for the control
  chart.}
  \item{gridsize}{gridsize to evalue the joint PDF values}
  \item{B, T}{iteration times and maximum time of \code{t} to
  generate random samples for density estimation}
}
\author{
  B. Wang  \email{bwang@southalabama.edu}
}
\references{
  Yang, S.F. and Wang, B. ``Using A Kernel Control Region to Monitor
  Both the Process Location and Dispersion''. 
}
\keyword{stats}

