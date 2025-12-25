\name{StepSignalMargiLike-package}
\alias{StepSignalMargiLike-package}
\alias{StepSignalMargiLike}
%\alias{est.changepoints}
%\alias{PlotChangePoints}
%\alias{est.mean.norm}
%\alias{prior.norm.A}
%\alias{prior.norm.B}
%\alias{prior.norm.C}
%\alias{est.mean.pois}
%\alias{prior.pois}
\docType{package}
\title{Estimating Change Points Using Marginal Likelihood
}
\description{
(See the Manual.pdf file in data folder for a detail description of all functions, and a walkthrough tutorial.)

This packages provides function to estimate multiple change points using marginal likelihood method proposed by Du, Kao and Kou (2015), which we would denoted as DKK2015 afterward. \code{est.changepoints } estimates change-points. \code{PlotChangePoints} plots. Other functions are for the normal and Poisson examples in DKK2015.
}

\author{
Chao Du, Chu-Lan Michael Kao, Samuel Kou

Maintainer: Chu-Lan Michael Kao <chulankao@gmail.com>
}
\references{
Chao Du, Chu-Lan Michael Kao and S. C. Kou (2016), "Stepwise Signal Extraction via Marginal Likelihood"
}
\examples{
n <- 5
data.x <- rnorm(n, 1, 1)
data.x <- c(data.x, rnorm(n, 10,1))
data.x <- c(data.x, rnorm(n, 2,1))
data.x <- c(data.x, rnorm(n, 10,1))
data.x <- c(data.x, rnorm(n, 1,1))
data.t <- 1:(5*n)

prior <- prior.norm.A(data.x)
max.segs <- 10

index.ChPT <- est.changepoints(data.x, mode="normal", prior)
est.mean <- est.mean.norm(data.x, index.ChPT, prior)
PlotChangePoints(data.x, data.t, index.ChPT, est.mean)

PlotChangePoints(data.x, data.t, index.ChPT, est.mean, type.data="p",
  col.data="green", col.est="black", main="Stepwise Signal Estimation",
  sub="Using Marginal Likelihood", xlab="time", ylab="value")
}

\details{
\tabular{ll}{
Package: \tab StepSignalMargiLike\cr
Type: \tab Package\cr
Version: \tab 2.5.9\cr
Date: \tab 2017-8-22\cr
License: \tab GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007\cr
}

}
