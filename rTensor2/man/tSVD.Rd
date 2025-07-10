\name{tSVD}
\alias{tSVD}
\title{Tensor Singular Value Decomposition Using Any Discrete Transform}

\description{
Performs a tensor singular value decomposition on any 3-mode tensor using any discrete transform.
}
\usage{
tSVD(tnsr,tform)
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

If the SVD is performed on a \eqn{m} x \eqn{n} x \eqn{k} tensor, the components in the returned value are:

    U: The left singular value tensor object (\eqn{m} x \eqn{m} x \eqn{k})

    V: The right singular value tensor object (\eqn{n} x \eqn{n} x \eqn{k})

    S: A diagonal tensor (\eqn{m} x \eqn{n} x \eqn{k})
}
\author{
Kyle Caudle

Randy Hoover

Jackson Cates
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
T <- rand_tensor(modes=c(2,3,4))
print(tSVD(T,"dst"))
}

