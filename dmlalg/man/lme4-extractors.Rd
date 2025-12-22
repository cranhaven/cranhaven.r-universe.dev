\name{lme4-extractors}
\alias{fixef}
\alias{fixef.mmdml}
\alias{ranef}
\alias{ranef.mmdml}
\alias{VarCorr}
\alias{VarCorr.mmdml}
\alias{vcov}
\alias{vcov.mmdml}
\title{Extract Components from 'mmdml' Fits Imported from 'lme4'}
\usage{
fixef(object, ...)
\method{fixef}{mmdml}(object, ...)

ranef(object, ...)
\method{ranef}{mmdml}(object, ...)

VarCorr(x, sigma = 1, ...)
\method{VarCorr}{mmdml}(x, ...)

vcov(object, ...)
\method{vcov}{mmdml}(object, ...)
}
\arguments{
\item{object, x}{An object of class \code{mmdml}. This object usually results
    from a function call to \code{\link{mmdml}}.}

\item{sigma}{See \code{\link[lme4]{lmer}} from package \CRANpkg{lme4}.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
See \code{\link[lme4]{lmer}} from package \CRANpkg{lme4}.
}
\description{
Methods for the class \code{mmdml} for generics from \CRANpkg{lme4}.
}

\details{
\code{fixef.mmdml}:
    Extracts the estimator of the linear coefficient \eqn{\beta_0}, which
    is a named and numeric vector.

\code{ranef.mmdml}:
    Extracts the \code{random_eff} entry from \code{object}.

\code{VarCorr.mmdml}:
    The variance and correlation components are computed with the
    \code{sigma} and the \code{theta} entries of \code{x} as in
    \code{\link[lme4]{lmer}}.
    For each of the \code{S} repetitions, \code{sigma} and \code{theta}
    computed
    on the \code{K} sample splits are aggregated by taking the mean.
    Then, the \code{S} mean-aggregated estimates are aggregated by
    the median.
    The variance and correlation components are computed with these
    median-aggregated estimates.

\code{vcov.mmdml}:
    It returns the variance-covariance matrix of the estimator of the linear
    coefficient is extracted.
    It is computed based on the asymptotic Gaussian distribution
    of the estimator.
    First, for each of the \code{S} repetitions, the variance-covariance
    matrices computed
    on the \code{K} sample splits are aggregated by taking the mean.
    Second, the \code{S} mean-aggregated estimates are aggregated by
    adding a term correcting for the randomness in the sample splits
    and by taking the median of these corrected terms.
    This final corrected and median-aggregated matrix is returned.
}

\examples{
## See example(mmdml) for examples
}
\seealso{
\code{\link{mmdml}}
}
