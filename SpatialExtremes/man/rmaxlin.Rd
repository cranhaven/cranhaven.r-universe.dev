\name{rmaxlin}
\alias{rmaxlin}
\title{Simulation from max-linear models}
\description{This function generates realisations from a max-linear
  model.}
\usage{
rmaxlin(n, coord, cov.mod = "gauss", dsgn.mat, grid = FALSE, p = 500,
...)
}
\arguments{
  \item{n}{Integer. The number of observations.}
  \item{coord}{A vector or matrix that gives the coordinates of each
    location. Each row corresponds to one location - if any. May be
    missing if \code{dsgn.mat} is specified.}
  \item{cov.mod}{A character string that specifies the max-linear
    model. Currently only the discretized Smith model is implemented,
    i.e., \code{cov.mod = "gauss"}.}
  \item{dsgn.mat}{The design matrix of the max-linear model --- see
    Section Details. May be missing if \code{coord} and \code{cov.mod}
    are given.}
  \item{grid}{Logical. Does \code{coord} defines a grid?}
  \item{p}{An integer corresponding to the number of unit Frechet random
    variable used in the max-linear model --- see Section Details.}
  \item{\dots}{The parameters of the max-stable model --- see Section
    Details.}
}
\details{
  A max-linear process \eqn{\{Y(x)\}}{{Y(x)}} is defined by
  \deqn{Y(x) = \max_{j=1, \ldots, p} f_j(x) Z_j, \qquad x \in
    R^d,}{Y(x) = max_{j=1, \dots, p} f_j(x) Z_j, x in R^d,}
  where \eqn{p}{p} is a positive integer, \eqn{f_j}{f_j} are non
  negative functions and \eqn{Z_j}{Z_j} are independent unit Frechet
  random variables. Most often, the max-linear process will be generated
  at locations \eqn{x_1, \ldots, x_k \in R^d}{x_1, \dots, x_k
    in R^d} and an alternative but equivalent formulation is
  \deqn{\bf{Y} = A \odot \bf{Z},}{Y = A * Z,} where \eqn{\mathbf{Y} =
    \{Y(x_1), \ldots, Y(x_k)\}}{Y = {Y(x_1), \ldots, Y(x_k)}},
  \eqn{\mathbf{Z} = (Z_1, \ldots, Z_p)}{Z = (Z_1, \ldots, Z_p)},
  \eqn{\odot}{*} is the max-linear operator, see the first equation, and
  \eqn{A}{A} is the design matrix of the max-linear model. The design
  matrix \eqn{A}{A} is a \eqn{k \times p}{k x p} matrix with non
  negative entries and whose \eqn{i}{i}-th row is \eqn{\{f_1(x_i),
    \ldots, f_p(x_i)\}}{{f_1(x_i), \ldots, f_i(x_p)}}.

  Currently only the discretized Smith model is implemented for which
  \eqn{f_j(x) = c(p) \varphi(x - u_j ; \Sigma)}{f_j(x) = c(p) phi(x -
  u_j ; Sigma)} where \eqn{\varphi(\cdot; \Sigma)}{phi( . ; Sigma)} is the
  zero mean (multivariate) normal density with covariance matrix
  \eqn{\Sigma}{Sigma}, \eqn{u_j}{u_j} is a sequence of deterministic
  points appropriately chosen and \eqn{c(p)}{c(p)} is a constant
  ensuring unit Frechet margins. Hence if this max-linear model is used,
  users must specify \code{var} for one dimensional processes, and
  \code{cov11}, \code{cov12}, \code{cov22} for two dimensional
  processes.
}
\value{
  A matrix containing observations from the max-linear model. Each
  column represents one stations. If \code{grid = TRUE}, the function
  returns an array of dimension nrow(coord) x nrow(coord) x n.
}
\references{
  Wang, Y. and Stoev, S. A. (2011) Conditional Sampling for Max-Stable
  Random Fields. \emph{Advances in Applied Probability}.
}
\author{Mathieu Ribatet}
\seealso{\code{\link{condrmaxlin}}, \code{\link{rmaxstab}}}
\examples{
## A one dimensional simulation from a design matrix. This design matrix
## corresponds to a max-moving average process MMA(alpha)
n.site <- 250
x <- seq(-10, 10, length = n.site)

## Build the design matrix
alpha <- 0.8
dsgn.mat <- matrix(0, n.site, n.site)
dsgn.mat[1,1] <- 1

for (i in 2:n.site){
dsgn.mat[i,1:(i-1)] <- alpha * dsgn.mat[i-1,1:(i-1)]
dsgn.mat[i,i] <- 1 - alpha
}

data <- rmaxlin(3, dsgn.mat = dsgn.mat)
matplot(x, t(log(data)), pch = 1, type = "l", lty = 1, ylab =
expression(log(Y(x))))

## One realisation from the discretized Smith model (2d sim)
x <- y <- seq(-10, 10, length = 100)
data <- rmaxlin(1, cbind(x, y), cov11 = 3, cov12 = 1, cov22 = 4, grid =
TRUE, p = 2000)
image(x, y, log(data), col = heat.colors(64))
}
\keyword{distribution}
