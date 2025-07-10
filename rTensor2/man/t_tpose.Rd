\name{t_tpose}
\alias{t_tpose}
\title{Transpose 3-mode Tensor}

\description{
Performs the transpose of a symmetric 3-mode tensor using any discrete transform.
}
\usage{
t_tpose(tnsr,tform)
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
T <- rand_tensor(modes=c(2,3,4))
print(t_tpose(T,"dct"))
}
