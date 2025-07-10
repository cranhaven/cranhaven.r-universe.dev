\name{tINVfft}
\alias{tINVfft}
\title{Inverse of a 3-mode Tensor Using the Discrete Fourier Transform}

\description{
Performs the inverse of a tensor using the discrete Fourier transform.
}
\usage{
tINVfft(tnsr)
}
\arguments{
\item{tnsr}{: a 3-mode tensor}
}
\value{
a \href{/library/rTensor/help/Tensor-class}{Tensor-class} object
}
\author{
Kyle Caudle

Randy Hoover

Jackson Cates
}
\examples{
T <- rand_tensor(modes=c(2,2,4))
print(tINVfft(T))
}

