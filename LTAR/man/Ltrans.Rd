\name{Ltrans}
\alias{Ltrans}
\title{Tensor Transformation}

\description{
Performs a tensor transformation of a 3D tensor using the discrete cosine transform along mode 3.
}

\usage{

Ltrans(tnsr)
}

\arguments{
\item{tnsr}{: A 3D tensor}
}
\value{
a \href{/library/rTensor/help/Tensor-class}{Tensor-class} object that has been transformed using the DCT.
}


\author{
Kyle Caudle

Randy Hoover

Jackson Cates
}
\examples{
require(rTensor)
data(tensor)
tnsr <- as.tensor(tensor)
trans_tensor <- Ltrans(tnsr)
# print first lateral slice
trans_tensor[,1,]
}
