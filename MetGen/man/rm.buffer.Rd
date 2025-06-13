\name{rm.buffer}
\alias{rm.buffer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove buffer
}
\description{
Remove buffer after using it for simulation
}
\usage{
rm.buffer(simdata, nstat, bi.length = 480)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{simdata}{
Data frame obtained after simulation using the function \code{\link{projection.lagged}}
}
  \item{nstat}{
Numeric vector specifying the number of sites used 
}
  \item{bi.length}{
Numeric vector that specifies the period of buffer (number of days* number of climatological observations per day)
}
}

\value{
Data frame derived from \code{\link{projection.lagged}} that start from dstart and finish on dend
}

\seealso{
\code{\link{projection.lagged}}
}
