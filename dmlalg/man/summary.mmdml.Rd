\name{summary.mmdml}
\alias{summary.mmdml}
\title{Summarizing mmdml fits}
\usage{
\method{summary}{mmdml}(object,
    correlation = (p <= getOption("lme4.summary.cor.max")),
    nr_res = NULL, ...)
}
\arguments{
\item{object}{An object of class \code{mmdml}. This object usually
results
from a function call to \code{\link{mmdml}}.}

\item{correlation}{Boolean indicating if the variance and correlation
    components (\code{vcov}, \code{\link{VarCorr}}) should be printed.}

\item{nr_res}{Boolean indicating how many sets of residuals among the \code{S}
    should be used to compute the residual information.
    By default, all available sets, that is, \code{nr_res} many that
    are saved in \code{object} resulting from \code{\link{mmdml}}
    are used.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
Prints a summary output similar to \code{\link[lme4]{lmer}}
    from package \CRANpkg{lme4}.
}
\description{
This is a method for the class \code{mmdml}. It summarizes
    objects of class \code{mmdml} that typically result from a function
    call to \code{\link{mmdml}}.
}
\examples{
## See example(mmdml) for examples
}
\seealso{
\code{\link{mmdml}}
}
