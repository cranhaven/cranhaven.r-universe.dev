\name{multi.split}
\alias{multi.split}
\title{Calculate P-values Based on Multi-Splitting Approach}
\description{
  Calculate p-values and confidence intervals based on the
  multi-splitting approach
}
\usage{
multi.split(x, y, B = 100, fraction = 0.5, ci = TRUE, ci.level = 0.95,
            model.selector = lasso.cv,
            classical.fit = lm.pval, classical.ci = lm.ci,
            parallel = FALSE, ncores = getOption("mc.cores", 2L),
            gamma = seq(ceiling(0.05 * B) / B, 1 - 1 / B, by = 1 / B),
            args.model.selector = NULL, args.classical.fit = NULL,
            args.classical.ci = NULL,
            return.nonaggr = FALSE, return.selmodels = FALSE,
            repeat.max = 20,
            verbose = FALSE)
}
\arguments{
  \item{x}{numeric design matrix (without intercept).}
  \item{y}{numeric response vector.}
  \item{B}{the number of sample-splits, a positive integer.}
  \item{fraction}{a number in \eqn{(0,1)}, the fraction of data used at
    each sample split for the model selection process.  The remaining
    data is used for calculating the p-values.}
  \item{ci}{logical indicating if a confidence interval should be
    calculated for each parameter.}
  \item{ci.level}{(if \code{ci} is true:) a number in \eqn{(0,1)},
    typically close to 1, the desired coverage level of the confidence
    intervals.}
  \item{model.selector}{a \code{\link{function}} to perform model
    selection, with default \code{\link{lasso.cv}}.  The function must have at
    least two arguments, \code{x} (the design matrix) and \code{y} (the
    response vector).  Return value is the index vector of selected columns.  See
    \code{\link{lasso.cv}} and \code{\link{lasso.firstq}} for an
    example.  Additional arguments can be passed via \code{args.model.selector}.}
  \item{classical.fit}{a \code{\link{function}} to calculate (classical)
    p-values.  Default is \code{\link{lm.pval}}.  The function must have
    at least two arguments, \code{x} (the design matrix) and \code{y}
    (the response vector), and return the vector of p-values.  See
    \code{\link{lm.pval}} for an example.  Additional arguments can be
    passed through \code{args.classical.fit}.}
  \item{classical.ci}{a \code{\link{function}} to calculate (classical)
    confidence intervals.  Default is \code{\link{lm.ci}}.  The function
    must have at least 3 arguments, \code{x} (the design matrix),
    \code{y} (the response vector) and \code{level} (the coverage
    level), and return the matrix of confidence intervals.  See
    \code{\link{lm.ci}} for an example.  Additional arguments can be
    passed through \code{args.classical.ci}.}
  \item{parallel}{logical indicating if parallelization via
    \code{\link[parallel]{mclapply}} should be used.}
  \item{ncores}{number of cores used for parallelization as
    \code{mc.cores} in \code{\link[parallel]{mclapply}()}.}
  \item{gamma}{vector of gamma-values.  In case gamma is a scalar, the
    value \eqn{Q_j} instead of \eqn{P_j} is being calculated (see
    reference below).}
  \item{args.model.selector}{named \code{\link{list}} of further arguments for
    function \code{model.selector}.}
  \item{args.classical.fit}{named \code{\link{list}} of further
    arguments for function \code{classical.fit}.}
  \item{args.classical.ci}{named \code{\link{list}} of further arguments
    for function \code{classical.ci}.}
  \item{return.nonaggr}{\code{\link{logical}} indicating if the
    unadjusted p-values be returned.}
  \item{return.selmodels}{\code{\link{logical}} indicating if the
    selected models (at each split) should be returned.  Necessary for
    the \code{clusterGroupTest()} part of the result.}
  \item{repeat.max}{positive integer indicating the maximal number of
    split trials.   Should not matter in regular cases, but necessary to
    prevent infinite loops in borderline cases.}
  \item{verbose}{should information be printed out while computing? (logical).}
}
%%\details{...}%% FIXME!
\value{
  \item{pval.corr}{Vector of multiple testing corrected p-values.}
  \item{gamma.min}{Value of gamma where minimal p-values was attained.}
  \item{clusterGroupTest}{Function to perform groupwise tests based on
    hierarchical clustering.  You can either provide a distance matrix
    and clustering method or the output of hierarchical clustering from
    the function \code{\link{hclust}} as for
    \code{\link{clusterGroupBound}}. P-values are adjusted for multiple
    testing.} %% FIXME: Need  Example / Demo / Vignette
}
\references{
  Meinshausen, N., Meier, L. and \enc{Bühlmann}{Buhlmann}, P. (2009)
  P-values for high-dimensional regression.
  \emph{Journal of the American Statistical Association} \bold{104},
  1671--1681.

  Mandozzi, J. and \enc{Bühlmann}{Buhlmann}, P. (2015)
  A sequential rejection testing method for high-dimensional regression
  with correlated variables. To appear in the International Journal of
  Biostatistics. Preprint arXiv:1502.03300
}
\author{Lukas Meier, Ruben Dezeure, Jacopo Mandozzi}

\seealso{
  \code{\link{lasso.cv}}, \code{\link{lasso.firstq}};
  \code{\link{lm.pval}}, \code{\link{lm.ci}}.
}

\examples{
n <-  40 # a bit small, to keep example "fast"
p <- 256
x <- matrix(rnorm(n * p), nrow = n, ncol = p)
y <- x[,1] * 2 + x[,2] * 2.5 + rnorm(n)

## Multi-splitting with lasso.firstq as model selector function
## 'q' must be specified
fit.multi <- multi.split(x, y, model.selector = lasso.firstq,
                         args.model.selector = list(q = 10))
fit.multi
head(fit.multi$pval.corr, 10) ## the first 10 p-values
ci. <- confint(fit.multi)
head(ci.) # the first 6
stopifnot(all.equal(ci.,
     with(fit.multi, cbind(lci, uci)), check.attributes=FALSE))

\donttest{%% "too" expensive;  still run via --run-donttest
## Use default 'lasso.cv' (slower!!) -- incl cluster group testing:
system.time(fit.m2 <- multi.split(x, y, return.selmodels = TRUE))# 9 sec (on "i7")
head(fit.m2$pval.corr) ## the first  6  p-values
head(confint(fit.m2))  ## the first  6  95\% conf.intervals

## Now do clustergroup testing
clGTst <- fit.m2$clusterGroupTest
names(envGT <- environment(clGTst))# about 14
if(!interactive()) # if you are curious (and advanced):
  print(ls.str(envGT), max = 0)
stopifnot(identical(clGTst, envGT$clusterGroupTest))
ccc <- clGTst()
str(ccc)%% MM: hmm..
ccc$hh   # the clustering
has.1.or.2 <- sapply(ccc$clusters,
                function(j.set) any(c(1,2) \%in\% j.set))
ccc$pval[ has.1.or.2] ## all very small
ccc$pval[!has.1.or.2] ## all 1
}% dont...
}
\keyword{models}
\keyword{regression}
