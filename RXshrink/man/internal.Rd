\name{RXshrink-internal}
\title{Internal RXshrink functions}
\alias{RXshrink-internal}
\alias{print.aug.lars}
\alias{print.correct.signs}
\alias{print.eff.ridge}
\alias{print.MLboot}
\alias{print.MLcalc}
\alias{print.MLhist}
\alias{print.MLtrue}
\alias{print.qm.ridge}
\alias{print.RXpredict}
\alias{print.uc.lars}
\alias{print.unr.ridge}
\alias{print.eff.biv}
\alias{print.YonX}
\alias{mstep}
\alias{kofm1}
\alias{mofk1}
\alias{mapp}
\alias{B19}
\description{These are internal functions for printing and calculating quantities such
 as additive K-factors and multiplicative Delta-Factors that determine a specified
 m-Extent of Shrinkage. mstep() is called by qm.ridge(), true.risk(), true.simu(), aug.lars()
 and uc.lars(). kofm1(), mofk1() and mapp() are called by YonX(). B19() is called by
 plot.eff.ridge() and plot.qm.ridge().
}
\usage{
  mstep(mobj, kinc, p, qp, eqm1)
  mofk1(k, dMSE)
  kofm1(muobj, dMSE, delmax)
  mapp(mVal, YXobj)
  B19(x)  
}
\arguments{
  \item{mobj}{Objective value for the M-extent-of-shrinkage along a path of shape qp.}
  \item{kinc}{Previous additive eigenvalue inflation constant yielding a smaller mobj.}
  \item{p}{Number of estimates in the regression coefficient vector.}
  \item{qp}{Shrinkage path \bold{Q}-shape parameter.}
  \item{eqm1}{Vector of X'X eigenvalues raised to the power (qp-1).}
  \item{k}{Multiplicative factor that must decrease to increase shrinkage.}
  \item{dMSE}{MSE Optimal Shrinkage delta-factor for YonX().}
  \item{muobj}{Objective value for M-extent-of-shrinkage in kofm1().}
  \item{delmax}{Scalar value strictly < 1. Default value is delmax = 0.999999.}
  \item{mVal}{Objective M-extent-of-shrinkage in mapp().}
  \item{YXobj}{YonX object used by mapp().}
  \item{x}{Numerical value argument for B19().}
}
\value{An output list consisting of two objects (k-factor & d-matrix):
  \item{kinc}{New k-factor producing an m-Extent-of-shrinkage of mobj.}
  \item{d}{A pxp diagonal matrix of multiplicative shrinkage delta-factors for the
   uncorrelated components of the regression coefficient vector. p = 1 in mofk1().}
  \item{m}{m-Extent of Shrinkage output by mofk1() or mapp().}  
  \item{kStar}{New k-factor producing an m-Extent-of-shrinkage of muobj.}
}
\author{Bob Obenchain <wizbob@att.net>}
\keyword{internal}
