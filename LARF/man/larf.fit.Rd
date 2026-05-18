\name{larf.fit}
\alias{larf.fit}

\title{Fitting the Local Average Response Function}
\description{
It is the work-horse function for its high-level interface \code{larf}.
}
\usage{
larf.fit(Y, X, D, Z, method, AME, optimizer, zProb)
}

\arguments{
  \item{Y}{a vector containing the outcome.}
  \item{X}{a matrix containing the covariates excluding the treatment.}
  \item{D}{a vector containing the binary treatment.}
  \item{Z}{a vector containing the binary instrument for the endogenous treatment.}
  \item{method}{the estimation method to be used. The default is ``LS", standing for least squares. ``ML", standing for maximum likelihood, is an alternative.}
  \item{AME}{whether average marginal effects (AME) should be reported. The default is FALSE, in which case marginal effects at the means (MEM) are reported.}
    \item{optimizer}{the optimization algorithm for the ML method. It should be one of ``Nelder-Mead", ``BFGS", ``CG", ``L-BFGS-B", ``SANN", or ``Brent". See \code{optim} in R for more detail.}
  \item{zProb}{a vector containing the probability of receiving the treatment inducement (i.e., instrument = 1) that have been estimated by semiparametrical methods.}
}

\author{
Weihua An and Xuefu Wang, Departments of Sociology and Statistics, Indiana University Bloomington
}

\seealso{
\code{\link{larf}}, \code{\link{c401k}}
}

