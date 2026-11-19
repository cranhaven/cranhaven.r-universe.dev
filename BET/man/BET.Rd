\name{MaxBET}
\alias{MaxBET}
\title{Binary Expansion Testing at a Certain Depth}
\usage{
MaxBET(
  X,
  dep,
  unif.margin = FALSE,
  asymptotic = TRUE,
  plot = FALSE,
  index = list(c(1:ncol(X)))
)
}
\arguments{
\item{X}{a matrix to be tested. When \code{X} has only one column, \code{BET} will test whether \code{X} is [0, 1]-uniformly distributed (an error will be given if data is out of range [0, 1]). When \code{X} has two or more columns, \code{BET} tests the independence among those column vectors.}

\item{dep}{depth of the binary expansion for the \code{BET}. }
\item{unif.margin}{
  logicals. If \code{TRUE} the marginal distribution is known to be Uniform[0,1]. Default is \code{FALSE}, and empirical cdf transformation will be applied to each marginal distribution.
  }
\item{asymptotic}{
  logicals. If \code{TRUE} the p-value is computed by asymptotic distribution. Default to be \code{TRUE}. Ignored if \code{X} has three or more columns.
  }
\item{plot}{
  logicals. If \code{TRUE}, make the plot of cross interaction of the strongest asymmetry. Default to be \code{FALSE}. This option only works for X with two columns.
  }
\item{index}{
  a list of indices. If provided, test the independence among two or more groups of variables. For example, \code{index = list(c(1,2), c(3)))} refers to test the independence between \eqn{(X_1, X_2)} and \eqn{X_3}. Default to be \code{list(c(1:p))}, where \code{p = ncol(X)}, then test data uniformity.
}
}
\details{
\code{MaxBET} tests the independence or uniformity by considering the maximal magnitude of the symmetry statistics in the \eqn{sigma}-field generated from marginal binary expansions at the depth \code{d}.
}
\value{
\item{Interaction}{a dataframe with \eqn{p} columns, where \eqn{p} is the number of columns of \code{X}. It displays the interactions where the extreme symmetry statistics happens. For each column in \code{X}, we use a binary index to indicate binary variables involved in the extreme symmetry statistic.}
\item{Extreme.Asymmetry}{the extreme asymmetry statistics.}
\item{p.value.bonf}{p-value of the test with Bonferroni adjustment.}
\item{z.statistic}{normal approximation of the test statistic.}
}
\description{
\code{MaxBET} stands for Binary Expansion Testing. It is used for nonparametric detection of nonuniformity or dependence. It can be used to test whether a column vector is [0, 1]-uniformly distributed. It can also be used to detect dependence between columns of a matrix \code{X}, if \code{X} has more than one column.
}
\examples{
##test mutual independence
v <- runif(128, -pi, pi)
X1 <- cos(v) + 2.5 * rnorm(128, 0, 1/20)
X2 <- sin(v) + 2.5 * rnorm(128, 0, 1/20)
MaxBET(cbind(X1, X2), 3, asymptotic = FALSE, index = list(1,2))

##test independence between (x1, x2) and y
x1 = runif(128)
x2 = runif(128)
y = sin(4*pi*(x1 + x2)) + 0.4*rnorm(128)
MaxBET(cbind(x1, x2, y), 3, index = list(c(1,2), c(3)))

##test uniformity
x1 = rbeta(128, 2, 4)
x2 = rbeta(128, 2, 4)
x3 = rbeta(128, 2, 4)
MaxBET(cbind(x1, x2, x3), 3)
}
