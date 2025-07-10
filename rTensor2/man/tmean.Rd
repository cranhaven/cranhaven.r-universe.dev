\name{tmean}
\alias{tmean}
\title{Find the mean of a tensor}

\description{
Find the mean of a 3-mode tensor.
}
\usage{
tmean(tnsr)
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
tnsr <- rand_tensor(modes=c(3,4,5))
tmean(tnsr)
}

