\name{gma-package}
\alias{gma-package}
\docType{package}
\title{
Granger Mediation Analysis
}
\description{
gma performs Granger Mediation Analysis (GMA) of time series. This package includes a single level GMA model and a two-level GMA model, for time series with hierarchically nested structure. The single level GMA model for the time series of a single participant performs the causal mediation analysis which integrates the structural equation modeling and the Granger causality frameworks. A multivariate autoregressive model of order p is employed to account for the spatiotemporal dependencies in the data. Meanwhile, the model introduces the unmeasured confounding effect through a nonzero correlation parameter. Under the two-level model, by leveraging the variabilities across participants, the parameters are identifiable and consistently estimated using a two-stage method or a block coordinate descent method.
}
\details{
\tabular{ll}{
Package: \tab gma \cr
Type: \tab Package \cr
Version: \tab 1.0 \cr
Date: \tab 2017-08-23 \cr
License: \tab GPL (>=2) \cr
}
}
\author{
	Yi Zhao <zhaoyi1026@gmail.com> and Xi Luo <xi.rossi.luo@gmail.com> \cr

Maintainer: Yi Zhao <zhaoyi1026@gmail.com>
}
\references{Zhao, Y., & Luo, X. (2017). \emph{Granger Mediation Analysis of Multiple Time Series with an Application to fMRI.} arXiv preprint arXiv:1709.05328.
}
\keyword{ package }
