\name{def.log}
\alias{def.log}
\title{Lambda Deformed Logarithm}
\usage{
def.log(x, lambda = 0, normalize = FALSE)
}
\arguments{
\item{x}{a numeric value.}

\item{lambda}{deformation parameter. Default value: \code{lambda = 0} (natural log).}

\item{normalize}{normalization (internal purpose).}
}
\value{
The function value of the lambda deformed logarithm at x.
}
\description{
Calculates the lambda deformed logarithm.
}
\examples{
def.log(3)   # Natural log of 3
def.log(3,2) # Deformed by lambda = 2

}
\seealso{
\code{\link{def.exp}}.
}
\concept{deformation}
\concept{logarithm}
