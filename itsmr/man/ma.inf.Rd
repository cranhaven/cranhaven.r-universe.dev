\name{ma.inf}
\Rdversion{1.1}
\alias{ma.inf}

\title{
Compute MA infinity coefficients
}

\description{
Compute MA infinity coefficients
}

\usage{
ma.inf(a, n = 50)
}

\arguments{
\item{a}{ARMA model}
\item{n}{Order}
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
Returns a vector of length \code{n+1} to accomodate coefficient 0 at index 1.
}

\seealso{
\code{\link{ar.inf}}
}

\examples{
M = c("diff",12)
e = Resid(deaths,M)
a = arma(e,1,1)
ma.inf(a,10)
}
