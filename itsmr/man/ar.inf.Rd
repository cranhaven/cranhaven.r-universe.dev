\name{ar.inf}
\Rdversion{1.1}
\alias{ar.inf}

\title{
Compute AR infinity coefficients
}

\description{
Compute AR infinity coefficients
}

\usage{
ar.inf(a, n = 50)
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
\code{\link{ma.inf}}
}

\examples{
a = yw(Sunspots,2)
ar.inf(a)
}
