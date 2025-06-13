\name{funcen}
\alias{funcen}

\title{Bivariate kernel density estimation under random censoring}
\description{
Computes the kernel density estimation of the bivariate vector \eqn{(marker, time-to-event)} with the time-to-event variable subject to right censorship, according to the procedure exposed in \url{https://doi.org/10.1177/0962280217740786}.
}
\usage{
funcen(data, H, bw, adj, ...)
}
\arguments{
  \item{data}{matrix with three columns: time-to-event, censoring status (0=censored/1=uncensored) and marker.}
  \item{H}{2x2 bandwidth matrix when it is specified in an explicit way.}
  \item{bw}{method for computing the bandwidth matrix. Most of the methods included in the \code{\link{kde}} function can be used: Hpi, Hpi.diag, Hlscv, Hlscv.diag, Hbcv, Hbcv.diag, Hscv, Hscv.diag, Hucv and Hucv.diag. Other considered methods are naive.pdf (diag(N^{-1/5}, N^{-1/5})^2) and naive.cdf (diag(N^{-1/3}, N^{-1/3})^2), where N is the sample size.}
  \item{adj}{adjusment parameter for calculating the bandwidth matrix. Default value 1.}
  \item{...}{\code{\link{kde}} function arguments can also be used for specifying the way in which the kernel density function estimation should be computed.}
 }
\details{

The matrix of bandwidths can be defined by using H=matrix() or automatically selected by the method indicated in bw.

Given the matrix of bandwidths, H, the argument adj modifies it and the final computed matrix is adj^2 H.

If H is missing, the naive.pdf method is used for obtaining the kernel density estimation.

Function \code{funcen} generates, from the original set of data, a collection of pseudodata through an iterative  weights allocation process, with two main goals: keep the information from the censored observations represented in the sample and prepare data so they can be used as incoming parameters in the \code{\link{kde}} function included in the \pkg{ks} package. A weighted kernel density estimation is therefore finally computed.

There should be at least two uncensored observations for computing the density estimation.

Omitted parameters are considered to be the default ones in the \code{\link{kde}} function.}

\value{
An object  of class \code{\link{kde}} is returned. It is a list where the most relevant values are:
 \item{x}{matrix containing the pseudodata values. It has two columns: marker and time-to-event.}
 \item{eval.points}{list of points where the bivariate kernel estimation is calculated.}
 \item{estimate}{values of the density estimation.}
 \item{H}{bandwidth matrix.}
 \item{names}{variable names.}
 \item{w}{weights calculated by the function and allocated to pseudodata.}
}


\examples{
library(smoothROCtime)
require(KMsurv)
require(lattice)

data(kidtran)

# Preparing data: a logarithmic transformationof the time-to-event variable is made
  DT <- cbind(log(kidtran$time),kidtran$delta,kidtran$age)
  n  <-length(log(kidtran$time))

# Directly definition of the bandwidth matrix
  H  <- diag((c(sd(kidtran$age),sd(log(kidtran$time)))*n^(-0.2))^2)

# Kernel density function estimation
  density <- funcen(data=DT,H=H)

# Plot graphics
  wireframe(density$estimate, row.values=density$eval.points[[1]],
            column.values=density$eval.points[[1]],zlab="density")
  contour(x=density$eval.points[[1]],
          y=density$eval.points[[2]],
          z=density$estimate,
          ylim=c(6,10))
}


\references{
 {P. Martinez-Camblor and J. C. Pardo-Fernandez. Smooth time-dependent receiver operating characteristic
  curve estimators. Statistical Methods in Medical Research, 27(3):651-674, 2018. \url{https://doi.org/10.1177/0962280217740786}.
}

 {T. Duong. Bandwidth matrices for multivariate kernel density estimation. Ph.D. Thesis, University of
 Western, Australia, 2004. \url{http://www.mvstat.net/tduong}.
 }
}




