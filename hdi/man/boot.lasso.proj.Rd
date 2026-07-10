\name{boot.lasso.proj}
\title{P-values based on the bootstrapped lasso projection method}
\alias{boot.lasso.proj}
\description{
  Compute p-values based on the lasso projection method, also known as
  the de-sparsified Lasso, using the bootstrap to approximate the
  distribution of the estimator.
}
\usage{
boot.lasso.proj(x, y, family = "gaussian", standardize = TRUE,
                multiplecorr.method = "WY",
                parallel = FALSE, ncores = getOption("mc.cores", 2L),
                betainit = "cv lasso", sigma = NULL, Z = NULL, verbose = FALSE,
                return.Z = FALSE, robust= FALSE,
                B = 1000, boot.shortcut = FALSE,
                return.bootdist = FALSE, wild = FALSE,
                gaussian.stub = FALSE)
}
\arguments{
  \item{x}{Design matrix (without intercept).}
  \item{y}{Response vector.}
  \item{family}{family}
  \item{standardize}{Should design matrix be standardized to unit column
    standard deviation.}
  \item{multiplecorr.method}{Either "WY" or any of
    \code{\link{p.adjust.methods}}.}
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
  %% \item{suppress.grouptesting}{A boolean to optionally suppress the
  %%   preparations made for testing groups. This will avoid quite a bit of
  %%   computation and memory usage. The output will also be smaller.}
  \item{robust}{Uses a robust variance estimation procedure to be able
    to deal with model misspecification.}
  \item{B}{The number of bootstrap samples to be used.}
  \item{boot.shortcut}{A boolean to enable the computational shortcut
    for the bootstrap. If set to true, the lasso is not re-tuned for
    each bootstrap iteration, but it uses the tuning parameter computed
    on the original data instead.}
  \item{return.bootdist}{A boolean specifying if one is to return the
    computed bootstrap distributions to the estimator. (Matrix size:
    ncol(x)*B) If the multiple testing method was chosen to be WY, the
    bootstrap distribution computer under the complete null hypothesis
    is returned as well. This option is required if one wants to compute
    confidence intervals afterwards.}
  \item{wild}{Perform the wild bootstrap based on N(0,1) distributed
    random variables}
  \item{gaussian.stub}{DEVELOPER OPTION. Only enable if you know what
    you are doing.
    A boolean to run stub code instead of actually bootstrapping the
    estimator. It generates a finite sample distribution for each
    estimate by sampling B samples from
    N(0,\\hat\{s.e.\}_j^2). (Note: we do not sample from the multivariate
    gaussian with the covariance matrix. Therefore, no dependencies are 
    modelled at all.) Useful for debugging and for checking if the 
    bootstrap is way off for some reason.} 
}
\value{
  \item{pval}{Individual p-values for each parameter.}
  \item{pval.corr}{Multiple testing corrected p-values for each
    parameter.}
  %% \item{groupTest}{Function to perform groupwise tests. Groups are
  %%   indicated using an index vector with entries in {1,...,p} or a list thereof.}
  %% \item{clusterGroupTest}{Function to perform groupwise tests based on
  %%   hierarchical clustering. You can either provide a distance matrix
  %%   and clustering method or the output of hierarchical clustering from
  %%   the function \code{\link{hclust}} as for
  %%   \code{\link{clusterGroupBound}}. P-values are adjusted for multiple testing.}
  %\item{betahat}{initial estimate by the scaled lasso of \eqn{\beta^0}}
  %\item{bhat}{de-sparsified \eqn{\beta^0} estimate used for p-value calculation}
  \item{sigmahat}{\eqn{\widehat{\sigma}} coming from the scaled lasso.}
  \item{Z}{Only different from NULL if the option return.Z is on. This
    is an intermediate result from the computation which only depends on
    the design matrix x. These are the residuals of the nodewise
    regressions.}
  \item{B}{The number of bootstrap samples used.}
  \item{boot.shortcut}{If the bootstrap shortcut has been used or not.}
  \item{lambda}{What tuning parameter was used for the bootstrap
    shortcut. NULL if no shortcut was used or if no valid lambda was
    available to use for the shortcut.}
  \item{cboot.dist}{Only different from NULL if the option
    return.bootdist is on. This is a ncol(x)*B matrix where each row
    contains the computed centered bootstrap distribution for that
    estimate.}
  \item{cboot.dist.underH0}{Only different from NULL if the option
    return.bootdist is on and if the multiple testing method is WY. This
    is a ncol(x)*B matrix where each row 
    contains the computed centered bootstrap distribution for that
    estimate. These bootstrap distributions were computed under the
    complete null hypothesis (b_1 = ... = b_p = 0).}
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

  Dezeure, R., \enc{Bühlmann}{Buhlmann}, P. and Zhang, C. (2016)
  High-dimensional simultaneous inference with the bootstrap
  \emph{http://arxiv.org/abs/1606.03940}
}
\author{Ruben Dezeure}

\examples{
x <- matrix(rnorm(100 * 10), nrow = 100, ncol = 10)
y <- x[,1] + x[,2] + rnorm(100)

\donttest{
fit.lasso <- boot.lasso.proj(x, y)
which(fit.lasso$pval.corr < 0.05) # typically: '1' and '2' and no other
}

\donttest{
## Use the computational shortcut for the bootstrap to speed up
## computations
fit.lasso.shortcut <- boot.lasso.proj(x, y, boot.shortcut = TRUE)
which(fit.lasso.shortcut$pval.corr < 0.05) # typically: '1' and '2' and no other
}

\donttest{
## Return the bootstrap distribution as well and compute confidence intervals based on it
fit.lasso.allinfo <- boot.lasso.proj(x, y, return.bootdist = TRUE)
confint(fit.lasso.allinfo, level = 0.95)
confint(fit.lasso.allinfo, parm = 1:3)

## Use the scaled lasso for the initial estimate
fit.lasso.scaled <- boot.lasso.proj(x, y, betainit = "scaled lasso")
which(fit.lasso.scaled$pval.corr < 0.05)

## Use a robust estimate for the standard error
fit.lasso.robust <- boot.lasso.proj(x, y, robust = TRUE)
which(fit.lasso.robust$pval.corr < 0.05)
}
}
\keyword{models}
\keyword{regression}
