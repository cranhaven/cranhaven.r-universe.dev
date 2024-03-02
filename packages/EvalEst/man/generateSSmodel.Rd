\name{generateSSmodel}
\alias{generateSSmodel}
\title{Randomly generate a state space model}
\description{Randomly generate a state space model.}
\usage{
    generateSSmodel(m,n,p, stable=FALSE)
}
\arguments{
    \item{n,m,p}{Input, state and output dimensions.}
    \item{stable}{TRUE or FALSE indicating if the model must be stable.}
}
\value{An SS TSmodel.}
\details{
Randomly generate a state space model. If stable is true then the
largest root will have magnitude less than 1.0.
}
\examples{
z <-  generateSSmodel(2,3,1)
}
\concept{DSE}
\keyword{ts}

