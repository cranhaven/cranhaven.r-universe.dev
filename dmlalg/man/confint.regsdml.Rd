% Please edit documentation in R/methods.R
\name{confint.regsdml}
\alias{confint.regsdml}
\title{Confidence Intervals for coefficient estimates of regsdml fits}
\usage{
\method{confint}{regsdml}(object,
    parm = NULL,
    level = 0.95,
    print_regsDML = NULL,
    print_safety = NULL,
    print_DML = NULL,
    print_regDML = NULL,
    print_regDML_all_gamma = !is.null(parm),
    print_gamma = FALSE, ...)
}
\arguments{
\item{object}{An object of class \code{regsdml}. This object usually
    results from a function call to \code{\link{regsdml}}.}

\item{parm}{A vector containing the indices for which \code{gamma}-values
    the results of the regularized DML estimator, whose results
    are stored in the list \code{regDML_all_gamma_statistics} of \code{object},
    should be included in the output. If \code{parm} is specified,
    it is not necessary to specify \code{print_regDML_all_gamma}.}

\item{level}{A number between 0 and 1 representing
    the confidence level. The default is \code{level = 0.95}.}

\item{print_regsDML}{A boolean. If \code{TRUE}, the results
    of the \code{regsDML} method are returned.}

\item{print_safety}{A boolean. If \code{TRUE}, the results
    of the safety device are returned.}

\item{print_DML}{A boolean. If \code{TRUE}, the results
    of the \code{DML} method are returned.}

\item{print_regDML}{A boolean. If \code{TRUE}, the results
    of the \code{regDML} method with the optimal choice of \code{gamma}
    (including the factor \code{a_N}) are returned.}

\item{print_regDML_all_gamma}{A boolean. If \code{TRUE}, the results
    specified by \code{parm} below are returned.}

\item{print_gamma}{A boolean. If \code{TRUE}, the \code{gamma}-values
    are printed in brackets where the respective regularization methods
    achieved their optimum.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
Confidence intervals for the methods \code{regsDML}, the \code{safety} device,
    \code{DML}, \code{regDML} with the optimal
    choice of \eqn{\gamma} (including the factor \code{a_N}),
    and \code{regDML}
    with prespecified \eqn{\gamma}-values are returned by setting the
    respective arguments.

If none of the printing arguments are set, only the results of regsDML
    are returned if they are available. If they are not available and none of
    the printing arguments are set, the results from all available methods
    are returned. If \code{print_regsDML = FALSE}, only the results from
    those methods are returned that are explicitly specified by the printing
    arguments.
}
\description{
This is a method for the class \code{regsdml}.
    It computes two-sided
    confidence intervals for testing the two-sided component-wise
    null hypotheses that tests if a component equals zero
    with the (approximate) asymptotic Gaussian distribution of the
    coefficient estimator. The method can be applied to objects
    of class \code{regsdml}, which typically result from a function
    call to \code{\link{regsdml}}.
}
\examples{
## See example(regsdml) for examples.
}
\seealso{
\code{\link{regsdml}},
    \code{\link{summary.regsdml}},
    \code{\link{coef.regsdml}},
    \code{\link{vcov.regsdml}}
    \code{\link{print.regsdml}}
}
