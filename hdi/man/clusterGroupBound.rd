\name{clusterGroupBound}
\alias{clusterGroupBound}
%%--- NB:  quite similar to ./groupBound.rd --- keep in sync !
\title{Hierarchical structure group tests in linear model}
\description{
  Computes confidence intervals for the l1-norm of groups of linear regression
  coefficients in a hierarchical clustering tree.
}
\usage{
clusterGroupBound(x, y, method = "average",
                  dist = as.dist(1 - abs(cor(x))), alpha = 0.05,
                  eps = 0.1, hcloutput, nsplit = 11,
                  s = min(10, ncol(x) - 1),
                  silent = FALSE, setseed = TRUE, lpSolve = TRUE)}
\arguments{
  \item{x}{numeric design matrix of the regression \eqn{n \times p}{n * p}
    with \eqn{p} columns for \eqn{p} predictor variables and \eqn{n}
    rows corresponding to \eqn{n} observations.}
  \item{y}{numeric response variable of length \eqn{n}.}
  \item{method}{a \code{\link{character}} string; the method used for
    constructing the hierarchical clustering tree (default:
    \code{"average"} for \dQuote{average linkage}) via
    \code{\link{hclust}}.
    Alternatively, you can provide your own hierarchical clustering
    through the optional argument \code{hcloutput}.}
  \item{dist}{a distance matrix can be specified on which the
    hierarchical clustering will be based (see \code{\link{dist}}).  The
    default option is that the distance between variables will be
    calculated as 1 less the absolute correlation matrix.
    Alternatively, you can provide your own hierarchical clustering
    through the optional argument \code{hcloutput}.}
  \item{alpha}{numeric level in \eqn{(0, 1)} at which the test / confidence
    intervals are to be constructed.}
  \item{eps}{a level of eps*alpha is used and the values of different
    splits are aggregated using the (1-eps) quantile. See reference
    below for more details.}
  \item{hcloutput}{optionally, the value of a \code{\link{hclust}()}
    call. If it is provided, the arguments \code{dist} and \code{method}
    are ignored.}
  \item{nsplit}{the number of data splits used.}
  \item{s}{the dimensionality of the projection that is used. Lower
    values lead to faster computation and if \eqn{n > 50}, then \code{s}
    is set to 50 if left unspecified, to avoid lengthy computations.}
  \item{silent}{logical enabling progress output.}
  \item{setseed}{a logical; if this is true (recommended), then the same
    random seeds are used for all groups, which makes the confidence
    intervals simultaneously valid over all groups of variables tested.}
  \item{lpSolve}{logical; only set it to false if \code{lpSolve()} is not
    working on the current machine: setting it to false will result in
    much slower computations; only use on small problems.}
}
%\details{
%}
\value{
  Returns a list with components
  \item{groupNumber}{The index of the group tested in the original
    hierarchical clustering tree}
  \item{members}{A list containing the variables that belong into each
    testes group}
  \item{noMembers}{A vector containing the number of members in each group}
  \item{lowerBound}{The lower bound on the l1-norm in each group}
  \item{position}{The position on the x-axis of each group (used for plotting)}
  \item{leftChild}{Gives the index of the group that corresponds to the
    left child node in the tested tree (negative values correspond to leaf
    nodes)}
  \item{rightChild}{Same as \code{leftCHild} for the right child of each node}
  \item{isLeaf}{Logical vector. Is \code{TRUE} for a group if it is a leaf
    node in the tested tree or if both child nodes have a zero lower bound
    on their group l1-norm}
}

\references{
  Meinshausen, N. (2015); JRSS B, see \code{\link{groupBound}}.
}
\author{Nicolai Meinshausen}

\seealso{
  Use \code{\link{groupBound}} to compute the lower bound for selected
  groups of variables whereas you use this \code{clusterGroupBound} to
  test all groups in a hierarchical clustering tree.
}
\examples{
%% the following code is in donttest environment to
%% speed-up computing
%% >>> copy any changes to "../tests/ex-clusterGroupBound.R" <<< to ensure
%% code is running
## Create a regression problem with correlated design (n = 10, p = 3):
## a block of size 2 and a block of size 1, within-block correlation is 0.99

set.seed(29)
p   <- 3
n   <- 10

Sigma <- diag(p)
Sigma[1,2] <- Sigma[2,1] <- 0.99

x <- matrix(rnorm(n * p), nrow = n) \%*\% chol(Sigma)

## Create response with active variable 1
beta    <- rep(0, p)
beta[1] <- 5

y  <- as.numeric(x \%*\% beta + rnorm(n))
\donttest{
out <- clusterGroupBound(x, y, nsplit = 4) ## use larger value for nsplit!

## Plot and print the hierarchical group-test
plot(out)
print(out)
out$members
out$lowerBound
}
}
\keyword{confidence intervals}
\keyword{regression}
\keyword{hierarchical clustering}

