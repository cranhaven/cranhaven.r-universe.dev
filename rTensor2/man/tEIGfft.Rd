\name{tEIGfft}
\alias{tEIGfft}
\title{Tensor Eigenvalue Decomposition Using  the Discrete Fourier Transform}

\description{
Performs a Eigenvalue decomposition of 3-mode tensor using the discrete Fourier transform.
}
\usage{
tEIGfft(tnsr)
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
\examples{
T <- rand_tensor(modes=c(2,2,4))
print(tEIGfft(T))
}
