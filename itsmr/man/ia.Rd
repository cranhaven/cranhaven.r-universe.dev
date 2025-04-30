\name{ia}
\Rdversion{1.1}
\alias{ia}

\title{
Estimate MA coefficients using the innovations algorithm
}

\description{
Estimate MA coefficients using the innovations algorithm
}

\usage{
ia(x, q, m = 17)
}

\arguments{
\item{x}{Time series data (typically residuals from \code{Resid})}
\item{q}{MA order}
\item{m}{Recursion level}
}

\details{
Normally \code{m} should be set to the default value.
The innovations algorithm is used to estimate white noise variance.
}

\value{
Returns an ARMA model consisting of a list with the following components.
\item{phi}{0}
\item{theta}{Vector of MA coefficients (index number equals coefficient subscript)}
\item{sigma2}{White noise variance}
\item{aicc}{Akaike information criterion corrected}
\item{se.phi}{0}
\item{se.theta}{Standard errors for the MA coefficients}
}

\seealso{
\code{\link{arma}}
\code{\link{burg}}
\code{\link{hannan}}
\code{\link{yw}}
}

\examples{
M = c("diff",1)
e = Resid(dowj,M)
a = ia(e,1)
print(a)
}
