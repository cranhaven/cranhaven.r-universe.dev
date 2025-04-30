\name{sim}
\Rdversion{1.1}
\alias{sim}

\title{
Generate synthetic observations
}

\description{
Generate synthetic observations
}

\usage{
sim(a, n = 100)
}

\arguments{
\item{a}{ARMA model}
\item{n}{Number of synthetic observations required}
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
Returns a vector of \code{n} synthetic observations.
}

\examples{
a = specify(ar=c(0,0,.99))
x = sim(a,60)
plotc(x)
}
