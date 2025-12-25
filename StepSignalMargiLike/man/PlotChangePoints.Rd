\name{PlotChangePoints}
\alias{PlotChangePoints}
\title{PlotChangePoints}
\usage{
PlotChangePoints(data.x, data.t, index.ChPT, est.mean, type.data, col.data,
  col.est, main.plot, sub.plot, xlab.plot, ylab.plot)
}
\arguments{
  \item{data.x}{Observed data in vector form where each element represents a single observation.}

  \item{data.t}{The one-dimensional time or seqeuential
  labeling for the data.}

  \item{index.ChPT}{The set of the index of change points
  in a vector. Must be in accending order. This could be
  obtained by \code{est.changepoints}.}

  \item{est.mean}{The estimated mean in each segments in a
  vector. The length must be one plus the length of
  \code{index.ChPT}. \cr For normal and Poisson cases as in
  DKK2013, apply \code{est.mean.norm} and
  \code{est.mean.pois} respectively.}

  \item{type.data}{(Opt.) The line type for the data.
  Options are the same as in plot() argument. Default is
  "l".}

  \item{col.data}{(Opt.) The line color for the data.
  Options are the same as in plot() argument. Default is
  "red".}

  \item{col.est}{(Opt.) The line color for the estimated
  stepwise signal. Options are the same as in plot()
  arguent. Default is "blue".}

  \item{main.plot}{(Opt.) The overall title used in the
  plot, which is like the main in plot(). Default is
  NULL.}\item{sub.plot}{(Opt.) The sub title used in the
  plot, which is like the main in plot(). Default is NULL.}

  \item{xlab.plot}{(Opt.) The title for the x axis used in
  the plot, which is like the main in plot(). Default is
  "data.t".}

  \item{ylab.plot}{(Opt.) The title for the y axis used in
  the plot, which is like the main in plot(). Default is
  "data.x".}
}
\value{
Plot for the data and the estimated change-points. Note
that this function only apply to one-dimensional
observation.
}
\description{
This function plots the data and the estimated stepwise
signal given the estimated change points and means. The
function only applies to one dimensional data.
}
\details{
See Manual.pdf in "data" folder.
}
\examples{
library(StepSignalMargiLike)

n <- 5
data.x <- rnorm(n, 1, 1)
data.x <- c(data.x, rnorm(n, 10,1))
data.x <- c(data.x, rnorm(n, 2,1))
data.x <- c(data.x, rnorm(n, 10,1))
data.x <- c(data.x, rnorm(n, 1,1))
data.x <- matrix(data.x, 1)
data.t <- 1:(5*n)

index.ChPT <- c(n,2*n,3*n,4*n)
est.mean <- c(1,10,2,10,2)
PlotChangePoints(data.x, data.t, index.ChPT, est.mean)

PlotChangePoints(data.x, data.t, index.ChPT, est.mean, type.data="p",
  col.data="green", col.est="black", main="Stepwise Signal Estimation",
  sub="Using Marginal Likelihood", xlab="time", ylab="value")
}

