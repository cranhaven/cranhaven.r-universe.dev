\name{groupBound}
\title{Lower bound on the l1-norm of groups of regression variables}
%%--- NB:  quite similar to ./clusterGroupBound.rd --- keep in sync !
\alias{groupBound}
\description{
  Computes a lower bound that forms a one-sided confidence interval for
  the group l1-norm of a specified group of regression parameters. It is
  assumed that errors have a Gaussian distribution with unknown noise
  level.  The underlying vector that inference is made about is the
  l1-sparsest approximation to the noiseless data.
  %% Under a weak compatibility condition, this is identical
  %% to inference about the l1-sparsest approximation to the noiseless
  %% data.
}
\usage{
groupBound(x, y, group, alpha = 0.05, eps = 0.1, nsplit = 11,
           s = min(10, ncol(x) - 1), setseed = TRUE,
           silent = FALSE, lpSolve = TRUE, parallel = FALSE,
           ncores = getOption("mc.cores", 2L))
}
\arguments{
  \item{x}{numeric design matrix of the regression \eqn{n \times p}{n * p}
    with \eqn{p} columns for \eqn{p} predictor variables and \eqn{n}
    rows corresponding to \eqn{n} observations.}
  \item{y}{numeric response variable of length \eqn{n}.}
  \item{group}{either a numeric vector with entries in \eqn{\{1,...,p\}}
    or a \code{\link{list}} with such numeric vectors. If \code{group}
    is a numeric vector, this is the group of variables for which a
    lower bound is computed. If \code{group} is a list, the lower bound
    is computed for each group in the list.}
  \item{alpha}{numeric level in \eqn{(0,1)} at which the test /
    confidence interval is computed.}
  \item{eps}{a level of eps * alpha is used and the values of different
    splits are aggregated using the (1 - eps) quantile. See reference
    below for more details.}
  \item{nsplit}{the number of data splits used.}
  \item{s}{the dimensionality of the projection that is used.  Lower
    values lead to faster computation and if \eqn{n > 50}, then \code{s}
    is set to 50 if left unspecified, to avoid lengthy computations.}
  \item{setseed}{a logical; if this is true (recommended), then the same
    random seeds are used for all groups, which makes the confidence
    intervals simultaneously valid over all groups of variables tested.}
  \item{silent}{logical enabling progress output.}
  \item{lpSolve}{logical; only set it to false if \code{lpSolve()} is not
    working on the current machine: setting it to false will result in
    much slower computations; only use on small problems.}
  \item{parallel}{should parallelization be used? (logical)}
  \item{ncores}{number of cores used for parallelization.}
}
\details{The data are split since the noise level is unknown.  On the
  first part of the random split, a cross-validated lasso solution is
  computed, using the \CRANpkg{glmnet} implementation.  This estimator
  is used as an initial estimator on the second half of the data.
  Results at level \code{alpha} are aggregated over \code{nsplit} splits
  via the median of results at levels \code{alpha/2}.
}

\value{If \code{group} is a single numeric vector, a scalar containg the lower
 bound for this group of variables is returned. If \code{group} is a
 list, a numeric vector is retuned where each entry corresponds to the
 group of variables defined in the same order in \code{group}.
}

\references{
  Meinshausen, N. (2015)
  Group bound: confidence intervals for groups of variables in sparse
  high dimensional regression without assumptions on the design.
  \emph{Journal of the Royal Statistical Society: Series B}, \bold{77},
  923--945; \doi{10.1111/rssb.12094}.
}

\author{Nicolai Meinshausen}

\seealso{Use \code{\link{clusterGroupBound}} to test all groups in a
  hierarchical clustering tree.
}

\examples{
## Create a regression problem with correlated design: p = 6, n = 50,
## block size B = 3 and within-block correlation of rho = 0.99
p   <- 6
n   <- 50
B   <- 3
rho <- 0.99

ind   <- rep(1:ceiling(p / B), each = B)[1:p]
Sigma <- diag(p)

for (ii in unique(ind)){
  id <- which(ind == ii)
  Sigma[id, id] <- rho
}
diag(Sigma) <- 1

x <- matrix(rnorm(n * p), nrow = n) \%*\% chol(Sigma)

## Create response with active variable 1
beta    <- rep(0, p)
beta[1] <- 5

y  <- as.numeric(x \%*\% beta + rnorm(n))

## Compute lower bounds:

## Lower bound for the L1-norm of *all* variables 1-6 of the sparsest
## optimal vector
nsplit <- 4  ## to make example run fast (use larger value)
lowerBoundAll <- groupBound(x, y, 1:p, nsplit = nsplit)
cat("\nlower bound for all variables 1-6: ", lowerBoundAll, "\n")

## Compute additional lower bounds:
q()## Lower bounds for variable 1 itself, then group {1,3}, 1-2, 1-3, 2-6,
lowerBound <- groupBound(x, y, list(1, c(1,3), 1:2, 1:3, 2:6),
                         nsplit = nsplit)
cat("lower bound for the groups\n\t {1}, {1,3}, {1,2}, {1..3}, {2..6}:\n\t",
    format(formatC(c(lowerBound))), "\n")
}
\keyword{confidence intervals}
\keyword{regression}
