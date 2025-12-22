\name{print.mmdml}
\alias{print.mmdml}
\title{Printing mmdml fits}
\usage{
\method{print}{mmdml}(x, digits = max(3, getOption("digits") - 3),
    ranef.comp = "Std.Dev.", ...)
}
\arguments{
\item{x}{An object of class \code{mmdml}. This object usually results
    from a function call to \code{\link{mmdml}}.}

\item{digits}{Number of significant digits for printing;
    also see \code{\link[lme4]{lmer}} from package \CRANpkg{lme4}.}

\item{ranef.comp}{A character vector of length one or two
    indicating if random-effects parameters should be reported
    on the variance and/or standard deviation scale; also see
    \code{\link[lme4]{lmer}}.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
See \code{\link[lme4]{lmer}}.
}
\description{
This is a method for the class \code{mmdml}.
    It prints
    objects of class \code{mmdml} that typically result from a function
    call to \code{\link{mmdml}}.
}
\examples{
## See example(mmdml) for examples
}
\seealso{
\code{\link{mmdml}}
}
