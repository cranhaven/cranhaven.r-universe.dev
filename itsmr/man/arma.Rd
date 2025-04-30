\name{arma}
\Rdversion{1.1}
\alias{arma}

\title{
Estimate ARMA model coefficients using maximum likelihood
}

\description{
Estimate ARMA model coefficients using maximum likelihood
}

\usage{
arma(x, p = 0, q = 0)
}

\arguments{
\item{x}{Time series data}
\item{p}{AR order}
\item{q}{MA order}
}

\details{
Calls the standard R function \code{arima} to estimate AR and MA coefficients.
The innovations algorithm is used to estimate white noise variance.
}

\value{
Returns an ARMA model consisting of a list with the following components.
\item{phi}{Vector of AR coefficients (index number equals coefficient subscript)}
\item{theta}{Vector of MA coefficients (index number equals coefficient subscript)}
\item{sigma2}{White noise variance}
\item{aicc}{Akaike information criterion corrected}
\item{se.phi}{Standard errors for the AR coefficients}
\item{se.theta}{Standard errors for the MA coefficients}
}

\seealso{
\code{\link{autofit}}
\code{\link{burg}}
\code{\link{hannan}}
\code{\link{ia}}
\code{\link{yw}}
}

\examples{
M = c("diff",1)
e = Resid(dowj,M)
a = arma(e,1,0)
print(a)
}
