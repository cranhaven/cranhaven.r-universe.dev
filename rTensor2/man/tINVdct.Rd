\name{tINVdct}
\alias{tINVdct}
\title{Inverse of a 3-mode Tensor Using the Discrete Cosine Transform}

\description{
Performs the inverse of a tensor using the discrete cosine transform.
}
\usage{
tINVdct(tnsr)
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
print(tINVdct(T))
}

