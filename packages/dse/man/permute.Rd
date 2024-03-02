\name{permute}
\alias{permute}
\title{Permute}
\description{
    Return matrix with rows indicating all possible selections of
    elements from seq(M). 0 in the result indicates omit. 
    M is usually a positive integer. M=0 gives NULL.
    Neg. M give -permute(abs(M)).
}
\usage{
    permute(M)
}
\arguments{
    \item{M}{An integer.}
}
\value{A matrix.}
\examples{
    permute(4)
}
\concept{DSE}
\keyword{ts}

