\name{def.exp}
\alias{def.exp}
\title{Lambda Deformed Exponential}
\usage{
def.exp(x, lambda = 0, normalize = FALSE)
}
\arguments{
\item{x}{a numeric value.}

\item{lambda}{deformation parameter. Default value: \code{lambda = 0} (regular exponential).}

\item{normalize}{logical value to indicate normalization.}
}
\value{
The function value of the lambda deformed exponential at x.
}
\description{
Calculates the lambda deformed exponential.
}
\examples{
def.exp(3)   # Natural exponential of 3
def.exp(3,2) # Deformed by lambda = 2

}
\seealso{
\code{\link{def.log}}.
}
\concept{deformation}
\concept{exponential}
