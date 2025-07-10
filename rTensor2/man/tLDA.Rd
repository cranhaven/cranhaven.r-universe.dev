\name{tLDA}
\alias{tLDA}
\title{Linear Discriminate Analysis of a 3-mode Tensor Using any Discrete Transform}

\description{
Performs linear discriminate analysis on a tensor using any discrete transform.  Assumes tensor is sorted by classes.
}
\usage{
tLDA(tnsr,nClass,nSamplesPerClass,tform)
}
\arguments{
\item{tnsr}{: a 3-mode tensor}

\item{nClass}{: Number of classes}

\item{nSamplesPerClass}{: Samples in each class}

\item{tform}{: one of six-discrete transforms. Supported transforms are:

    fft: Fast Fourier Transform

    dwt: Discrete Wavelet Transform (Haar Wavelet)

    dct: Discrete Cosine transform

    dst: Discrete Sine transform

    dht: Discrete Hadley transform

    dwht: Discrete Walsh-Hadamard transform}
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
data("Mnist")
T <- Mnist$train$images
myorder <- order(Mnist$train$labels)
# tLDA need to be sorted by classes
T_sorted <- as.tensor(T[,myorder,])
# Using small tensor, 2 images for each class for demonstration
T <- T_sorted[,c(1:2,1001:1002,2001:2002,3001:3002,
      4001:4002,5001:5002,6001:6002,7001:7002,
      8001:8002,9001:9002),]
tLDA(T,10,2,"dct")
}

