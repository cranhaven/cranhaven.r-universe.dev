% Please edit documentation in R/methods.R
\name{coef.regsdml}
\alias{coef.regsdml}
\title{Accessing the coefficients of regsdml fits}
\usage{
\method{coef}{regsdml}(object,
    print_regsDML = NULL,
    print_safety = NULL,
    print_DML = NULL,
    print_regDML = NULL,
    print_regDML_all_gamma = !is.null(parm),
    parm = NULL,
    print_gamma = FALSE, ...)
}
\arguments{
\item{object}{An object of class \code{regsdml}. This object usually
    results from a function call to \code{\link{regsdml}}.}

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

\item{parm}{A vector containing the indices for which \code{gamma}-values
    the results of the regularized DML estimator, whose results
    are stored in the list \code{regDML_all_gamma_statistics} of \code{object},
    should be included in the output. If \code{parm} is specified,
    it is not necessary to specify \code{print_regDML_all_gamma}.}

\item{print_gamma}{A boolean. If \code{TRUE}, the \code{gamma}-values
    are printed in an extra row where the respective regularization methods
    achieved their optimum.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
Coefficients of the methods \code{regsDML}, the \code{safety} device,
    \code{DML}, \code{regDML} with the optimal
    choice of \eqn{\gamma} (including the factor \code{a_N}),
    and \code{regDML}
    with prespecified \eqn{\gamma}-values are returned by setting the
    respective arguments. It is possible to return the respective
    \code{gamma}-values.

If none of the printing arguments are set, only the results of regsDML
    are returned if they are available. If they are not available and none of
    the printing arguments are set, the results from all available methods
    are returned. If \code{print_regsDML = FALSE}, only the results from
    those methods are returned that are explicitly specified by the printing
    arguments.
}
\description{
This is a method for the class \code{regsdml}. It returns the estimated
    coefficients from
    objects of class \code{regsdml}, which typically result from a function
    call to \code{\link{regsdml}}.
}
\examples{
## See example(regsdml) for examples
}
\seealso{
\code{\link{regsdml}},
    \code{\link{summary.regsdml}},
    \code{\link{confint.regsdml}},
    \code{\link{vcov.regsdml}}
    \code{\link{print.regsdml}}
}
