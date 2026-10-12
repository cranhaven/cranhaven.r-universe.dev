\name{export.total}
\alias{export.total}
\alias{import.total}

\title{Calculates Total Exports for InputOutput Objects
}
\description{
Uses values of the intermediate transaction matrix (\code{Z}) and when applicable final demand (\code{f}), and either exports (\code{E}) or imports (\code{M}) to calculate the total exports or imports for each region sector combination.

This function is intended to be a helper function for \code{\link{upstream}} and \code{\link{vs}}.
}
\usage{
export.total(io)
import.total(io)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{io}{An \code{InputOutput} class object from \code{\link{as.inputoutput}}}
}

\value{
Produces a nameless vector of total exports.
}

\author{
John J. P. Wade
}


\seealso{
\code{\link{export.coef}}
}
\examples{
data(toy.IO)
class(toy.IO)
export.total(toy.IO)
import.total(toy.IO)
}
