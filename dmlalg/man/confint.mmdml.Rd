\name{confint.mmdml}
\alias{confint.mmdml}
\title{Confidence Intervals for coefficient estimates of mmdml fits}
\usage{
\method{confint}{mmdml}(object, parm = NULL, level = 0.95, ...)
}
\arguments{
\item{object}{An object of class \code{mmdml}. This object usually results
    from a function call to \code{\link{mmdml}}.}

\item{parm}{A vector containing the indices for which \eqn{\beta_0}-entries
    confidence intervals should be computed. By default, it is set to
    \code{NULL}, in which case confidence intervals for all entries of
    \eqn{\beta_0} are computed.}

\item{level}{A number between 0 and 1 representing
    the confidence level. The default is \code{level = 0.95}.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
A matrix with columns giving the lower and upper confidence limits for each
    entry of \eqn{\beta_0}.
    The columns are labelled as
    These will be labelled as \code{(1-level)/2}\% and
    \code{1 - (1-level)/2}\%, by default 2.5\% and 97.5\%.
}
\description{
This is a method for the class \code{mmdml}.
    It computes two-sided
    confidence intervals for testing the two-sided component-wise
    null hypotheses
    \eqn{H_0: \beta_j = 0}
     with the (approximate) asymptotic Gaussian distribution of the coefficient
     estimator. The method can be applied to objects
     of class \code{mmdml} that typically result from a function
     call to \code{\link{mmdml}}.
}
\examples{
## See example(mmdml) for examples
}
\seealso{
\code{\link{mmdml}}
}
