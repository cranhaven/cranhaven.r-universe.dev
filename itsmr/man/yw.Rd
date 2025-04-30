\name{yw}
\Rdversion{1.1}
\alias{yw}

\title{
Estimate AR coefficients using the Yule-Walker method
}

\description{
Estimate AR coefficients using the Yule-Walker method
}

\usage{
yw(x, p)
}

\arguments{
\item{x}{Time series data (typically residuals from \code{Resid})}
\item{p}{AR order}
}

\details{
The innovations algorithm is used to estimate white noise variance.
}

\value{
Returns an ARMA model consisting of a list with the following components.
\item{phi}{Vector of AR coefficients (index number equals coefficient subscript)}
\item{theta}{0}
\item{sigma2}{White noise variance}
\item{aicc}{Akaike information criterion corrected}
\item{se.phi}{Standard errors for the AR coefficients}
\item{se.theta}{0}
}

\seealso{
\code{\link{arma}}
\code{\link{burg}}
\code{\link{hannan}}
\code{\link{ia}}
}

\examples{
M = c("diff",1)
e = Resid(dowj,M)
a = yw(e,1)
}
