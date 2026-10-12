\name{check.RS}
\alias{check.RS}

\title{
Do all regions have the same sectors?
}
\description{
Produces a logical answer to the question do all regions have the same sectors.
}
\usage{
check.RS(io)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{io}{An \code{InputOutput} class object from \code{\link{as.inputoutput}}}
}
\details{
Uses the \code{RS_label} to determine if all regions have the same sectors
}
\value{
Produces either \code{TRUE} or \code{FALSE}
}

\author{
John J. P. Wade
}




\seealso{
\code{\link{locate.mismatch}}
}
\examples{
data(toy.IO)
class(toy.IO)
check.RS(toy.IO)
}

