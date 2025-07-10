\name{tLUfft}
\alias{tLUfft}
\title{Tensor LU Decomposition Using  the Discrete Fourier Transform}

\description{
Performs a LU decomposition of 3-mode tensor using the discrete Fourier transform.
}
\usage{
tLUfft(tnsr)
}
\arguments{
\item{tnsr}{: a 3-mode tensor}

}
\value{
a \href{/library/rTensor/help/Tensor-class}{Tensor-class} object

If LU decomposition is performed on a \eqn{n x n x k} tensor, the components in the returned value are:

    L: The lower triangular tensor object (\eqn{n x n x k})

    U: The upper triangular tensor object (\eqn{n x n x k})
}
\author{
Kyle Caudle kyle.caudle@sdsmt.edu
}
\examples{
T <- rand_tensor(modes=c(2,2,4))
print(tLUfft(T))
}

