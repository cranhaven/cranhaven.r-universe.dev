\name{tQRdct}
\alias{tQRdct}
\title{Tensor QR Decomposition Using  the Discrete Cosine Transform}

\description{
Performs a QR decomposition of 3-mode tensor using the discrete Cosine transform.
}
\usage{
tQRdct(tnsr)
}
\arguments{
\item{tnsr}{: a 3-mode tensor}

}
\value{
a \href{/library/rTensor/help/Tensor-class}{Tensor-class} object

If QR decomposition is performed on a \eqn{n} x \eqn{n} x \eqn{k} tensor, the components in the returned value are:

    Q: An orthogonal tensor (\eqn{n} x \eqn{n} x \eqn{k}).

    R: An upper triangular tensor (\eqn{n} x \eqn{n} x \eqn{k})

}
\author{
Kyle Caudle

Randy Hoover

Jackson Cates
}

\examples{
T <- rand_tensor(modes=c(2,2,4))
print(tQR(T,"dct"))
}

