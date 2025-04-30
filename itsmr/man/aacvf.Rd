\name{aacvf}
\Rdversion{1.1}
\alias{aacvf}

\title{
Autocovariance of ARMA model
}

\description{
Autocovariance of ARMA model
}

\usage{
aacvf(a, h)
}

\arguments{
\item{a}{ARMA model}
\item{h}{Maximum lag}
}

\details{
The ARMA model is a list with the following components.
\cr
\tabular{ll}{
\code{phi}\tab
Vector of AR coefficients (index number equals coefficient subscript)
\cr
\code{theta}\tab
Vector of MA coefficients (index number equals coefficient subscript)
\cr
\code{sigma2}\tab
White noise variance
}}

\value{
Returns a vector of length \code{h+1} to accomodate lag 0 at index 1.
}

\seealso{
\code{\link{arma}}
}

\examples{
a = arma(Sunspots,2,0)
aacvf(a,40)
}
