\name{tIDWT}
\alias{tIDWT}
\title{Discrete Inverse Wavelet Transform of a 3-D Tensor}

\description{
Performs the Discrete Inverse Wavelet Transform of a 3-D Tensor.
}
\usage{
tIDWT(tnsr)
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
\references{
G. Strang and T. Nguyen, Wavelets and filter banks. SIAM, 1996.

A. Haar, "Zur theorie der orthogonalen funktionensysteme", Mathematische annalen, vol. 69, no. 3, pp. 331-371, 1910.
}
\examples{
T <- rand_tensor(modes=c(2,3,4))
print(tIDWT(T))
}

