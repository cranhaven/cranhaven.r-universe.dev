\name{tSVDdst}
\alias{tSVDdst}
\title{Tensor Singular Value Decomposition Using  the Discrete Sine Transform}

\description{
Performs a tensor singular value decomposition on any 3-mode tensor using the discrete Sine transform.
}
\usage{
tSVDdst(tnsr)
}
\arguments{
\item{tnsr}{: a 3-mode tensor}

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
\examples{
T <- rand_tensor(modes=c(2,3,4))
print(tSVDdst(T))
}

