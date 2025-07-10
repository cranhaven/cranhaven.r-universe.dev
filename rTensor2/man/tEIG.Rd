\name{tEIG}
\alias{tEIG}
\title{Tensor Eigenvalue Decomposition Using any Discrete Transform}

\description{
Performs a Eigenvalue decomposition of 3-mode tensor using any discrete transform.
}
\usage{
tEIG(tnsr, tform)
}
\arguments{
\item{tnsr}{: a 3-mode tensor, \eqn{n} x \eqn{n} x \eqn{k}}

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
tEIG(T,"dst")
}

