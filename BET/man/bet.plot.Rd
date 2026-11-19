\name{bet.plot}
\alias{bet.plot}
\title{Plotting Binary Expansion Testing (2-dimensions)}
\usage{
\method{bet}{plot}(X, dep, unif.margin = FALSE, cex=0.5, index = list(c(1:ncol(X))), ...)
}
\arguments{
\item{X}{a matrix with two columns.}

\item{dep}{depth of BET.}

\item{unif.margin}{
  logicals. If \code{TRUE} the marginal distribution is known to be Uniform[0,1]. Default is \code{FALSE}, and empirical cdf transformation will be applied to each marginal distribution.
  }

\item{cex}{number indicating the amount by which plotting text and symbols should be scaled relative to the default.}

\item{index}{
  a list of indices. If provided, test the independence among two or more groups of variables. For example, \code{index = list(c(1,2))} refers to test data uniformity, \code{index = list(1, 2)} refers to test independence between \eqn{X_1} and \eqn{X_2}. Default to be \code{list(c(1:p))}, where \code{p = ncol(X)}, then test data uniformity.
}

\item{...}{graphical parameters to plot}
}
\description{
\code{bet.plot} shows the cross interaction of the strongest asymmetry, which the BET returns with the rejection of independence null.
This function only works for the test on two variables, that is, \code{X} can only have two columns.
There are \eqn{2^{2dep} - 1} nontrivial binary variables in the \eqn{\sigma}-field and \eqn{(2^dep - 1)^2} of them are cross interactions, whose positive regions are in plotted in white and whose negative regions are plotted in blue.
\code{bet.plot} shows the cross interaction where the difference of number of observations in the positive and negative region is largest.
}
\examples{
v <- runif(128, -pi, pi)
X1 <- cos(v) + 2.5 * rnorm(128, 0, 1/20)
X2 <- sin(v) + 2.5 * rnorm(128, 0, 1/20)
bet.plot(cbind(X1, X2), 3, index = list(1,2))
}
