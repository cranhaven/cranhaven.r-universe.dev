\name{normdiff}
\alias{normdiff}
\alias{normdiff.maha}
\alias{normdiff.KL}
\alias{normdiff.J}
\alias{normdiff.Chisq}
\alias{normdiff.Hellinger}
\alias{normdiff.L2}
\alias{maha}
\alias{tt}
\alias{print.normdiff}
\title{Difference measures for multivariate Gaussian pdfs}
\description{Various difference measures for Gaussian pdfs are
  implemented: Euclidean distance of the means, Mahalanobis
  distance, Kullback-Leibler divergence, J-Coefficient, 
  Minkowski L2-distance, Chi-square divergence and the Hellinger
  coefficient which is a similarity measure.
}
\usage{normdiff(mu1,sigma1=NULL,mu2,sigma2=sigma1,inv=FALSE,s=0.5,
method=c("Mahalanobis","KL","J","Chisq",
"Hellinger","L2","Euclidean"))
}
\arguments{
  \item{mu1}{mean value of pdf 1, a vector}
  \item{sigma1}{covariance matrix of pdf 1}
  \item{mu2}{mean value of pdf 2, a vector}
  \item{sigma2}{covariance matrix of pdf 2}
  \item{method}{difference measure to be used, see below}
  \item{inv}{if TRUE, 1-Hellinger is reported, default: \code{inv=FALSE}} 
  \item{s}{exponent for Hellinger coefficient, default: \code{s=0.5}}
}
\value{A scalar object of class \code{normdiff} reporting the distance.
}
\details{Equations can be found in H.-H. Bock, \cite{Analysis of
    Symbolic Data}, Chapter \cite{Dissimilarity Measures for Probability
    Distributions}
}
\references{
  H.-H. Bock, \cite{Analysis of Symbolic Data}, Chapter
  \cite{Dissimilarity measures for Probabilistic Distributions}
}
\examples{
library(gaussDiff)
mu1 <- c(0,0,0)
sig1 <- diag(c(1,1,1))
mu2 <- c(1,1,1)
sig2 <- diag(c(0.5,0.5,0.5))

## Euclidean distance
normdiff(mu1=mu1,mu2=mu2,method="Euclidean")

## Mahalanobis distance
normdiff(mu1=mu1,sigma1=sig1,mu2=mu2,method="Mahalanobis")

## Kullback-Leibler divergence
normdiff(mu1=mu1,sigma1=sig1,mu2=mu2,sigma2=sig2,method="KL")

## J-Coefficient
normdiff(mu1=mu1,sigma1=sig1,mu2=mu2,sigma2=sig2,method="J")

## Chi-sqr divergence
normdiff(mu1=mu1,sigma1=sig1,mu2=mu2,sigma2=sig2,method="Chisq")

## Minkowsi L2 distance
normdiff(mu1=mu1,sigma1=sig1,mu2=mu2,sigma2=sig2,method="L2")

## Hellinger coefficient
normdiff(mu1=mu1,sigma1=sig1,mu2=mu2,sigma2=sig2,method="Hellinger")
}
\author{
  Henning Rust, \email{henning.rust@met.fu-berlin.de}
}
\keyword{distribution}
\keyword{multivariate}
\keyword{cluster}
