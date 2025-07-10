\name{polar}
\alias{polar}
\title{Polar/Jordan Form of a Matrix}

\description{
Creates the polar/Jordan form of the P and D matrices after performing eigenvalue decomposition where the eigenvalue values are complex.
}
\usage{
polar(P,D)
}
\arguments{
\item{P}{: the eigenvectors from an eigenvalue decomposition.}
\item{D}{: the eigenvalues from an eigenvalue decomposition.}
}
\value{
P the polar form (real-valued) matrix of eigenvectors.

D the polar form (real-valued) matrix of eigenvalues.
}
\author{
Kyle Caudle

Randy Hoover

Jackson Cates
}
\examples{
z <- complex(real = rnorm(16), imag = rnorm(16))
M <- matrix(z,nrow=4)
decomp <- eigen(M)
polar(decomp$vectors,decomp$values)
}

