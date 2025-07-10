\name{tEIGdwt}
\alias{tEIGdwt}
\title{Tensor Eigenvalue Decomposition Using  the Discrete Wavelet Transform}

\description{
Performs a Eigenvalue decomposition of 3-mode tensor using the discrete Wavelet transform (Haar Wavelet).
}
\usage{
tEIGdwt(tnsr)
}
\arguments{
\item{tnsr}{: a 3-mode tensor}
}
\value{
a \href{/library/rTensor/help/Tensor-class}{Tensor-class} object

If Eigenvalue decomposition is performed on a \eqn{n} x \eqn{n} x \eqn{k} tensor, the components in the returned value are:

    P: A tensor of Eigenvectors (\eqn{n} x \eqn{n} x \eqn{k})

    D: An diagonal tensor of Eigenvalues (\eqn{n} x \eqn{n} x \eqn{k})

}
\author{
Kyle Caudle

Randy Hoover

Jackson Cates
}
\references{

G. Strang and T. Nguyen, Wavelets and filter banks. SIAM, 1996.

A. Haar, “Zur theorie der orthogonalen funktionensysteme,” Mathema-
tische annalen, vol. 69, no. 3, pp. 331–371, 1910.

}
\examples{
T <- rand_tensor(modes=c(2,2,4))
print(tEIGdwt(T))
}

