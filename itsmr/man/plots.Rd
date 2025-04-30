\name{plots}
\Rdversion{1.1}
\alias{plots}

\title{
Plot spectrum of data or ARMA model
}

\description{
Plot spectrum of data or ARMA model
}

\usage{
plots(u)
}
\arguments{
\item{u}{Data vector or an ARMA model}
}

\value{
None
}

\seealso{
\code{\link{periodogram}}
}

\examples{
a = specify(ar=c(0,0,.99))
plots(a)
}
