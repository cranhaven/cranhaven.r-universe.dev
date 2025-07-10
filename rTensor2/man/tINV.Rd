\name{tINV}
\alias{tINV}
\title{Inverse of a 3-mode Tensor Using any Discrete Transform}

\description{
Performs the inverse of a tensor using the any discrete transform.
}
\usage{
tINV(tnsr,tform)
}
\arguments{
\item{tnsr}{: a 3-mode tensor}

\item{tform}{: Any discrete transform.  Supported transforms are:

    fft: Fast Fourier Transform

    dwt: Discrete Wavelet Transform (Haar Wavelet)

    dct: Discrete Cosine transform

    dst: Discrete Sine transform

    dht: Discrete Hadley transform

    dwht: Discrete Walsh-Hadamard transform}
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
print(tINV(T,"dst"))
}

