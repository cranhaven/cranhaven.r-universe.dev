\name{ifisher}
\alias{ifisher}
\title{
Information matrix for ARTFIMA
}
\description{
The information matrix for the lambda and d in ARTFIMA model.
At present only the TFD and FD models are supported but it is planned
to extend this to the full ARTFIMA model.
}


\usage{
ifisher(d = numeric(0), lambda = numeric(0), phi = numeric(0), 
  theta = numeric(0), sigma2 = 1, n = 1, obj = NULL, 
  alg = c("Fisher", "Whittle", "approx"))
}

\arguments{
  \item{d}{
d parameter
}
  \item{lambda}{
lambda parameter
}

  \item{phi}{AR coefficients}

  \item{theta}{MA coefficients, Box-Jenkins definition}

  \item{sigma2}{
innovation variance
}

  \item{n}{
series length
}

  \item{obj}{object of class artfima}

  \item{alg}{"Fisher", "Whittle" or "approx"}
}

\details{
This is the expected information matrix. The artfima() function returns
the component varbeta that is the inverse of the observed information
for a fitted model computed from the Hessian matrix.
}

\value{
  \item{se }{standard errors}
  \item{f }{information matrix}
}

\references{
TBA
}

\author{
A. I. McLeod
}

\seealso{
\code{\link{artfima}}
}

\examples{
ifisher(d=0.2, lambda=0.0025)
ifisher(d=0.2, lambda=0.0025, alg="Whittle")
ifisher(d=0.2, lambda=0.0025, alg="approx")
}

\keyword{ ts }
