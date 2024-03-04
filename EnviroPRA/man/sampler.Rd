\name{sampler}
\alias{sampler}

\title{
Execute sampling with replacement
}
\description{
Auxiliar function
}
\usage{
sampler(n, a)
}

\arguments{
  \item{n}{
Number of sampling iterations
}
  \item{a}{
data vector 
}
}

\value{
Resampled vector of length n
}

\author{
F. Barrio-Parra
}

\examples{

a <- rnorm (n = 20, mean = 0, sd = 1)

b <- sampler (n = 100, a = a)
}

\keyword{ methods }
