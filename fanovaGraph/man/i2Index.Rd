\name{i2Index}
\alias{i2Index}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Estimation of pure second order indices
}
\description{Estimation of the unscaled pure second order Sobol indices.
}
\usage{
i2Index(f.mat, d, q = NULL, q.arg = NULL, n.i2, ...)
}

\arguments{
  \item{f.mat}{vectorized function of which indices shall be estimated
}
  \item{d}{integer, number of input factors (vertices)
}
  \item{q}{a vector of character strings of quantile functions corresponding to the factors distributions, it can be a single character string meaning same distribution for all, if not specified \code{"qunif"} is taken
}
  \item{q.arg}{a list of lists of quantile functions parameters of the distributions in \code{q}, it can be a single list meaning same parameters for all, if not specified the default values of the respective distributions are taken
}
  \item{n.i2}{number of Monte Carlo evaluations
}
  \item{\dots}{additional arguments to be passed to the function \code{f.mat}
}
}

\value{
A vector containing the unscaled pure second order indices
}
\references{
Sobol', I. M. (1993) Sensitivity estimates for nonlinear mathematical models, \emph{Mathematical Modeling and Computational Experiment}, \bold{1}, 407-414.
}
\author{
J. Fruth
}

\seealso{
\code{\link{estimateGraph}}
\code{\link{totalIndex}}
}
\examples{
i2Index(f.mat=ishigami.fun, d=3, q.arg=list(min=-pi,max=pi), n.i2=10000)
}


