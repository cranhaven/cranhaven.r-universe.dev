\name{hdi}
\alias{hdi}
\title{Function to perform inference in high-dimensional (generalized) linear models}
\description{Perform inference in high-dimensional (generalized) linear
  models using various approaches.}
\usage{
hdi(x, y, method = "multi.split", B = NULL, fraction = 0.5,
    model.selector = NULL, EV = NULL, threshold = 0.75,
    gamma = seq(0.05, 0.99, by = 0.01),
    classical.fit = NULL,
    args.model.selector = NULL, args.classical.fit = NULL,
    verbose = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Design matrix (without intercept).}
  \item{y}{Response vector.}
  \item{method}{Multi-splitting ("multi.split") or stability-selection
    ("stability").}
  \item{B}{Number of sample-splits (for "multi.split") or sub-sample
    iterations (for "stability"). Default is 50 ("multi.split")or 100
    ("stability"). Ignored otherwise.}
  \item{fraction}{Fraction of data used at each of the B iterations.}
  \item{model.selector}{Function to perform model selection. Default is
    \code{\link{lasso.cv}} ("multi.split") and
    \code{\link{lasso.firstq}} ("stability"). Function must have at
    least two arguments: x (the design matrix) and y (the response
    vector). Return value is the index vector of selected columns. See
    \code{\link{lasso.cv}} and \code{\link{lasso.firstq}} for
    examples. Additional arguments can be passed through
    \code{args.model.selector}.}
  \item{EV}{(only for
    "stability"). Bound(s) for expected number of false positives . Can
    be a vector.}
  \item{threshold}{(only for "stability"). Bound on selection frequency.}
  \item{gamma}{(only for "multi.split"). Vector of gamma-values.}
  \item{classical.fit}{(only
    for "multi.split"). Function to calculate (classical)
    p-values. Default is \code{\link{lm.pval}}. Function must
    have at least two arguments: x (the design matrix) and y (the
    response vector). Return value is the vector of p-values. See
    \code{\link{lm.pval}} for an example. Additional arguments can be
    passed through \code{args.classical.fit}.}
  \item{args.model.selector}{Named list of further arguments for
    function \code{model.selector}.}
  \item{args.classical.fit}{Named list of further arguments for function
    \code{classical.fit}.}
  \item{verbose}{Should information be printed out while computing
    (logical).}
  \item{...}{Other arguments to be passed to the underlying functions.}
}
%\details{}
\value{
  \item{pval}{(only for "multi.split"). Vector of p-values.}
  \item{gamma.min}{(only for "multi.split"). Value of gamma where
    minimal p-values was attained.}
  \item{select}{(only for "stability"). List with selected predictors
    for the supplied values of EV.}
  \item{EV}{(only for "stability"). Vector of corresponding values of EV.}
  \item{thresholds}{(only for "stability"). Used thresholds.}
  \item{freq}{(only for "stability"). Vector of selection frequencies.}
}

\references{
  Meinshausen, N., Meier, L. and \enc{Bühlmann}{Buhlmann}, P. (2009)
  P-values for high-dimensional regression.
  \emph{Journal of the American Statistical Association} \bold{104}, 1671--1681.

  Meinshausen, N. and \enc{Bühlmann}{Buhlmann}, P. (2010)
  Stability selection (with discussion).
  \emph{Journal of the Royal Statistical Society: Series B} \bold{72}, 417--473.
}
\author{Lukas Meier}

\seealso{
  \code{\link{stability}}, \code{\link{multi.split}}
}

\examples{
x <- matrix(rnorm(100 * 200), nrow = 100, ncol = 200)
y <- x[,1] * 2 + x[,2] * 2.5 + rnorm(100)

## Multi-splitting with lasso.firstq as model selector function
fit.multi <- hdi(x, y, method = "multi.split",
                 model.selector =lasso.firstq,
                 args.model.selector = list(q = 10))
fit.multi
fit.multi$pval.corr[1:10] ## the first 10 p-values

## Stability selection
fit.stab <- hdi(x, y, method = "stability", EV = 2)
fit.stab
fit.stab$freq[1:10] ## frequency of the first 10 predictors
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{regression}% __ONLY ONE__ keyword per line
