\name{specify}
\Rdversion{1.1}
\alias{specify}

\title{
Specify an ARMA model
}

\description{
Specify an ARMA model
}

\usage{
specify(ar = 0, ma = 0, sigma2 = 1)
}

\arguments{
\item{ar}{Vector of AR coefficients (index number equals coefficient subscript)}
\item{ma}{Vector of MA coefficients (index number equals coefficient subscript)}
\item{sigma2}{White noise variance}
}

\value{
Returns an ARMA model consisting of a list with the following components.
\item{phi}{Vector of AR coefficients (index number equals coefficient subscript)}
\item{theta}{Vector of MA coefficients (index number equals coefficient subscript)}
\item{sigma2}{White noise variance}
}

\examples{
specify(ar=c(0,0,.99))
}
