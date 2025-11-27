\name{Stability.cumu}
\alias{Stability.cumu}
\title{Stability selection for ordinal-on-ordinal regression.}
\description{This function performs stability selection for the cumulative logit model.}

\usage{
Stability.cumu(x, y, lambda, n_iter=100, type=c("selection", "fusion"), ...)
}
\arguments{
  \item{x}{a vector or matrix of integers 1,2,... giving the observed levels
  of the ordinal factor(s). If \code{x} is a matrix, it is assumed that
  each column corresponds to one ordinal factor.}
  \item{y}{the vector of response values.}
  \item{lambda}{vector of penalty parameters (in decreasing order).}
  \item{n_iter}{number of subsamples. Details below.}
  \item{type}{penalty to be applied. If "selection", group lasso penalty for smoothing and selection is used. If "fusion", a fused lasso penalty for fusiona dn selection is used.}
  \item{...}{additional arguments to \code{\link{ordFusion}} and
  \code{\link{ordSelect}}, respectively.}
}
\details{
The method assumes that ordinal factor levels (contained in vector/columns of
matrix \code{x}) take values 1,2,...,max, where max denotes the highest level
of the respective factor observed in the data. Every level between 1 and max has
to be observed at least once.

Instead of selecting/fitting one model, the data are pertubed/subsampled \code{iter} times and we choose those variables that occur in a large fraction (\eqn{pi}) of runs.
The stability path then shows the order of relevance of the predictors according to stability selection.
}

\value{
    \item{Pi}{the matrix of estimated selection probabilities. Columns correspond to 
    different lambda values, rows correspond to covariates.}
    \item{mSize}{matrix of size \code{n_iter} \eqn{x} length(\code{lambda}) containing the corresponding model size.}
}


\references{
Hoshiyar, A., Gertheiss, L.H., and Gertheiss, J. (2023). \emph{Regularization and Model Selection for Item-on-Items Regression with Applications to Food Products' Survey Data.} Preprint, available from https://arxiv.org/abs/2309.16373.

Meinshausen, N. and Buehlmann, P. (2010). \emph{Stability selection}, Journal of the Royal Statistical Society B (Statistical Methodology), 72, 417-473.
}

\author{Aisouda Hoshiyar}


\seealso{\code{\link{ordSelect}}, \code{\link{ordFusion}}}
 

\keyword{stability selection}
