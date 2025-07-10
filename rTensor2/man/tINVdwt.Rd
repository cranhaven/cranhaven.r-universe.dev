\name{tINVdwt}
\alias{tINVdwt}
\title{Inverse of a 3-mode Tensor Using the Discrete Wavelet Transform}

\description{
Performs the inverse of a tensor using the discrete wavelet transform (Haar Wavelet).
}
\usage{
tINVdwt(tnsr)
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
print(tINVdwt(T))
}

