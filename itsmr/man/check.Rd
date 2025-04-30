\name{check}
\Rdversion{1.1}
\alias{check}

\title{
Check for causality and invertibility
}

\description{
Check for causality and invertibility
}

\usage{
check(a)
}

\arguments{
\item{a}{ARMA model}
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
None
}

\examples{
a = specify(ar=c(0,0,.99))
check(a)
}
