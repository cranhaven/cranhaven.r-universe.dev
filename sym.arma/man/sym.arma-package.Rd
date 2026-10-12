\name{sym.arma-package}
\alias{sym.arma-package}
\alias{sym.arma}
\docType{package}
\title{
\packageTitle{sym.arma}
}
\description{
This package provides a set of functions to fitting of autoregressive and moving average symmetric models.
}
\details{
Package: sym.arma

Type: Package

Version: 1.0

Date: 2018-09-23

License: GPL-2
}
\author{
Vinicius Quintas Souto Maior and Francisco Jose A. Cysneiros

Maintainer: Vinicius Quintas Souto Maior <vinicius@de.ufpe.br>
}
\references{
Maior, V. Q. S. and Cysneiros, F. J. A. (2018). SYMARMA: a new dynamic model for temporal data on conditional symmetric distribution. \emph{Statitical Paper},  \strong{59}, 75-97. doi: \href{https://doi.org/10.1007/s00362-016-0753-z}{10.1007/s00362-016-0753-z}.

Wei, W. W. S. (2006). \emph{Time Series Analysis: Univariate and Multivariate Methods}, 2nd edition. Pearson Addison Wesley. Section 7.2.1.

Efron, B. and Tibshirani, R. (1993). \emph{An Introduction to the Bootstrap}. Chapman and Hall, New York, London.

Thode, Henry C. (2002). \emph{Testing for normality}, New York: Marcel Dekker.

Cook, R.D. (1986). Assessment of local influence (with discussion). \emph{Journal of the Royal Statistical Society}, \strong{B 48}, 133-169.
}
\examples{
library(sym.arma)
data(assets)
fit <-  elliptical.ts(assets$msf[2122:2240],order=c(1,0,0),trace=TRUE)
qqplot(fit,envelope=FALSE)
}
