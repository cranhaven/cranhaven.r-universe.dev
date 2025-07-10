\name{tQR}
\alias{tQR}
\title{Tensor QR Decomposition Using Using Any Discrete Transform}

\description{
Performs a tensor QR decomposition on any 3-mode tensor using any discrete transform.
}
\usage{
tQR(tnsr,tform)
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

If the QR decomposition is performed on a \eqn{n \times n \times k} tensor, the components in the returned value are:

    Q: The left singular value tensor object (\eqn{n \times n \times k})

    R: The right singular value tensor object (\eqn{n \times n \times k})

}
\author{
Kyle Caudle kyle.caudle@sdsmt.edu
}
\references{
Kernfeld, E., Kilmer, M., & Aeron, S. (2015). Tensor-tensor products with invertible linear transforms. Linear Algebra and its Applications, 485, 545-570.

M. E. Kilmer, C. D. Martin, and L. Perrone, “A third-order generalization
of the matrix svd as a product of third-order tensors,” Tufts University,
Department of Computer Science, Tech. Rep. TR-2008-4, 2008

K. Braman, "Third-order tensors as linear operators on a space of
matrices", Linear Algebra and its Applications, vol. 433, no. 7, pp.
1241-1253, 2010.
}
\examples{
T <- rand_tensor(modes=c(2,2,4))
tQR(T,"dst")
}

