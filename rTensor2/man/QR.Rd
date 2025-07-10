\name{QR}
\alias{QR}
\title{QR Decomposition of a Complex Matrix Without Pivoting}

\description{
Performs QR Decomposition of a Complex Matrix without pivoting.
}
\usage{
QR(A)
}
\arguments{
\item{A}{: an  \eqn{n} x \eqn{n} matrix}
}
\value{
an orthogonal matrix Q and an upper triangular matrix R so that A = QR.
}
\author{
Kyle Caudle

Randy Hoover

Jackson Cates
}
\examples{
z <- complex(real = rnorm(16), imag = rnorm(16))
A <- matrix(z,nrow=4)
QR(A)
}

