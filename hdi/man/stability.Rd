\name{stability}
\alias{stability}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to perform stability selection}
\description{Function to perform stability selection}
\usage{
stability(x, y, EV, threshold = 0.75, B = 100, fraction = 0.5,
          model.selector = lasso.firstq, args.model.selector = NULL,
          parallel = FALSE, ncores = getOption("mc.cores", 2L),
          verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Design matrix (without intercept).}
  \item{y}{Response vector.}
  \item{EV}{Bound for expected number of false positives.}
  \item{threshold}{Threshold for selection frequency. Must be in (0.5, 1).}
  \item{B}{Number of sub-sample iterations.}
  \item{fraction}{Fraction of data used at each of the B sub-samples.}
  \item{model.selector}{Function to perform model selection. Default is
    \code{\link{lasso.firstq}}. User supplied function must have at
    least three arguments: x (the design matrix), y (the response
    vector) and q (the maximal model size). Return value is the index
    vector of selected columns. See \code{\link{lasso.firstq}} for an
    example. Additional arguments can be passed through
    \code{args.model.selector}.}
  \item{args.model.selector}{Named list of further arguments for
    function \code{model.selector}.}
  \item{parallel}{Should parallelization be used? (logical)}
  \item{ncores}{Number of cores used for parallelization.}
  \item{verbose}{Should information be printed out while computing (logical).}
}
%\details{...}

\value{
  \item{selected}{Vector of selected predictors.}
  \item{freq}{Vector of selection frequencies.}
  \item{q}{Size of fitted models in order to control error rate at
    desired level.}
}

\references{
  Meinshausen, N. and \enc{Bühlmann}{Buhlmann}, P. (2010)
  Stability selection (with discussion).
  \emph{Journal of the Royal Statistical Society: Series B} \bold{72}, 417--473.

  \enc{Bühlmann}{Buhlmann}, P., Kalisch, M. and Meier,
  L. (2014) \emph{High-dimensional statistics with a view towards
  applications in biology}. Annual Review of Statistics and
  its Applications \bold{1}, 255--278}

\author{Lukas Meier}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{...}

\examples{
x <- matrix(rnorm(100 * 1000), nrow = 100, ncol = 1000)
y <- x[,1] * 2 + x[,2] * 2.5 + rnorm(100)
fit.stab <- stability(x, y, EV = 1)
fit.stab
fit.stab$freq[1:10] ## selection frequency of the first 10 predictors
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{regression} % __ONLY ONE__ keyword per line
