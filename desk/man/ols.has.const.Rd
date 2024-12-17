\name{ols.has.const}
\alias{ols.has.const}
\title{Check if Model has a Constant}
\usage{
ols.has.const(mod)
}
\arguments{
\item{mod}{linear model object of class \code{"desk"} or \code{"lm"}.}
}
\value{
A logical value: \code{TRUE} (has contant) or \code{FALSE} (has no constant).
}
\description{
Checks if a linear model included a constant level parameter (alpha).
}
\examples{
my.modA = ols(y ~ x, data = data.tip)
my.modB = ols(y ~ 0 + x, data = data.tip)
ols.has.const(my.modA)
ols.has.const(my.modB)

}
\concept{constant}
\concept{intercept}
\concept{level parameter}
\concept{linear model}
