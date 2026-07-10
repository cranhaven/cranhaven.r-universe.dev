\name{lasso.proj}
\title{P-values based on lasso projection method}
\alias{lasso.proj}
\description{
  Compute p-values based on the lasso projection method, also known as
  the de-sparsified Lasso, using an asymptotic gaussian approximation
  to the distribution of the estimator.
}
\usage{
lasso.proj(x, y, family = "gaussian", standardize = TRUE,
           multiplecorr.method = "holm", N = 10000,
           parallel = FALSE, ncores = getOption("mc.cores", 2L),
           betainit = "cv lasso", sigma = NULL, Z = NULL, verbose = FALSE,
           return.Z = FALSE, suppress.grouptesting = FALSE, robust = FALSE,
	   do.ZnZ = FALSE)
}
\arguments{
  \item{x}{Design matrix (without intercept).}
  \item{y}{Response vector.}
  \item{family}{family}
  \item{standardize}{Should design matrix be standardized to unit column
    standard deviation.}
  \item{multiplecorr.method}{Either "WY" or any of
    \code{\link{p.adjust.methods}}.}
  \item{N}{Number of empirical samples (only used if multiplecorr.method
    == "WY")}
  \item{parallel}{Should parallelization be used? (logical)}
  \item{ncores}{Number of cores used for parallelization.}
  \item{betainit}{Either a numeric vector, corresponding to a sparse
    estimate of the coefficient vector, or the method to be used for the
    initial estimation, "scaled lasso" or "cv lasso".}
  \item{sigma}{Estimate of the standard deviation of the error term. This
    estimate needs to be compatible with the initial
    estimate (see betainit) provided or calculated. Otherwise, results
    will not be correct.}
  \item{Z}{user input, also see \code{return.Z} below}
  \item{verbose}{A boolean to enable reporting on the progress of the
    computations. (Only prints out information when Z is not provided by
    the user)}
  \item{return.Z}{An option to return the intermediate result which only
    depends on the design matrix x. This intermediate results
    can be used when calling the function again and the design matrix
    is the same as before.}
  \item{suppress.grouptesting}{A boolean to optionally suppress the
    preparations made for testing groups. This will avoid quite a bit of
    computation and memory usage. The output will also be smaller.}
  \item{robust}{Uses a robust variance estimation procedure to be able
    to deal with model misspecification.}
  \item{do.ZnZ}{Use a slightly different way of choosing tuning
    parameters to compute Z, called Z&Z based on Zhang and Zhang (2014).
    This choice of tuning parameter
    results in a slightly higher variance of the estimator.
    More concretely, it achieves a 25% inflation of the average theoretical
    variance of the estimator (over j=1..ncol(x)) in comparison to tuning with
    cross-validation.}
}
\value{
  \item{pval}{Individual p-values for each parameter.}
  \item{pval.corr}{Multiple testing corrected p-values for each
    parameter.}
  \item{groupTest}{Function to perform groupwise tests. Groups are
    indicated using an index vector with entries in \{1,...,p\} or a list thereof.}
  \item{clusterGroupTest}{Function to perform groupwise tests based on
    hierarchical clustering. You can either provide a distance matrix
    and clustering method or the output of hierarchical clustering from
    the function \code{\link{hclust}} as for
    \code{\link{clusterGroupBound}}. P-values are adjusted for multiple testing.}
  %\item{betahat}{initial estimate by the scaled lasso of \eqn{\beta^0}}
  %\item{bhat}{de-sparsified \eqn{\beta^0} estimate used for p-value calculation}
  \item{sigmahat}{\eqn{\widehat{\sigma}} coming from the scaled lasso.}
  \item{Z}{Only different from NULL if the option return.Z is on. This
    is an intermediate result from the computation which only depends on
    the design matrix x. These are the residuals of the nodewise regressions.}
}
\references{
  van de Geer, S., \enc{Bühlmann}{Buhlmann}, P., Ritov, Y. and Dezeure, R. (2014)
  On asymptotically optimal confidence regions and tests for
  high-dimensional models. \emph{Annals of Statistics} \bold{42}, 1166--1202._

  Zhang, C., Zhang, S. (2014)
  Confidence intervals for low dimensional parameters in high
  dimensional linear models. \emph{Journal of the Royal Statistical
  Society: Series B} \bold{76}, 217--242.

  \enc{Bühlmann}{Buhlmann}, P. and van de Geer, S. (2015)
  High-dimensional inference in misspecified linear models.
  \emph{Electronic Journal of Statistics} \bold{9}, 1449--1473.
}
\author{Ruben Dezeure}

\examples{
%% parts of the following code are in donttest environment to
%% speed-up computing
%% >>> copy any changes to "../tests/ex-lasso.proj.R" <<< to ensure
%% code is running
x <- matrix(rnorm(100 * 10), nrow = 100, ncol = 10)
y <- x[,1] + x[,2] + rnorm(100)
fit.lasso <- lasso.proj(x, y)
which(fit.lasso$pval.corr < 0.05) # typically: '1' and '2' and no other

## Group-wise testing of the first two coefficients
fit.lasso$groupTest(1:2)

##Compute confidence intervals
confint(fit.lasso, level = 0.95)

\donttest{
## Hierarchical testing using distance matrix based on
## correlation matrix
out.clust <- fit.lasso$clusterGroupTest()
plot(out.clust)

## Fit the lasso projection method without doing the preparations
## for group testing (saves time and memory)
fit.lasso.faster <- lasso.proj(x, y, suppress.grouptesting = TRUE)

## Use the scaled lasso for the initial estimate
fit.lasso.scaled <- lasso.proj(x, y, betainit = "scaled lasso")
which(fit.lasso.scaled$pval.corr < 0.05)

## Use a robust estimate for the standard error
fit.lasso.robust <- lasso.proj(x, y, robust = TRUE)
which(fit.lasso.robust$pval.corr < 0.05)
}

## Perform the Z&Z version of the lasso projection method
fit.lasso <- lasso.proj(x, y, do.ZnZ = TRUE)

which(fit.lasso$pval.corr < 0.05) # typically: '1' and '2' and no other
}
\keyword{models}
\keyword{regression}
