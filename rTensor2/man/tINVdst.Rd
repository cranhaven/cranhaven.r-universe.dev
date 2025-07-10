\name{tINVdst}
\alias{tINVdst}
\title{Inverse of a 3-mode Tensor Using the Discrete Sine Transform}

\description{
Performs the inverse of a tensor using the discrete sine transform.
}
\usage{
tINVdst(tnsr)
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
print(tINVdst(T))
}

