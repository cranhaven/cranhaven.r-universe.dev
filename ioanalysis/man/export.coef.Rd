\name{export.coef}
\alias{export.coef}
\alias{import.coef}

\title{Calculates the Matrix of Trade Coefficients
}
\description{
Uses the matrix of technical input coefficients (\code{A}) to calculate either the matrix of import coefficients or the matrix of export coefficients. It does require that all regions have the same sectors. This can be verified using \code{\link{check.RS}}

This function is intended to be a helper function for \code{\link{vs}}
}
\usage{
export.coef(io, region)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{io}{An \code{InputOutput} class object from \code{\link{as.inputoutput}}}
  \item{region}{Integer. Specific region to be used. The number of the region in the order it appears in \code{RS_label}. You can only do one region at a time.}
}
\details{
Adds appropriate blocks of the matrix of technical input coefficients to calculate the matrix of import/export coefficients. If there is an export matrix or an import matrix as a part of the \code{InputOutput} object, the results in the generated matrix may be biased.
}
\value{
Produces a nxn matrix, where n is the number of sectors.
}

\author{
John J. P. Wade
}



\seealso{
\code{\link{check.RS}}, \code{\link{locate.mismatch}}, \code{\link{upstream}}, \code{\link{vs}}
}
\examples{
data(toy.IO)
class(toy.IO)
import.coef(toy.IO, 1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

