\name{predict}
\alias{predict}
\title{Forecasts from a fitted SYMARMA model}
\description{See \emph{Maior and Cysneiros} (2018) for details on this function.}

\usage{predict(model, h, xreg = NULL)}

\arguments{
  \item{model}{a result of a call to \code{\link{elliptical.ts}}.}
  \item{h}{number of periods for forecasting. If xreg is used, h is ignored and the number of forecast periods is set to the number of rows of xreg.}
  \item{xreg}{future values of an regression variables.}
}

\value{
  \item{pred}{predicted values.}
}

\references{
Brockwell, P. J. and Davis, R. A. (1991). \emph{Time Series and Forecasting Methods}. Second edition. Springer, New York. Section 11.4.

Brockwell, P. J. and Davis, R. A. (1996). \emph{Introduction to Time Series and Forecasting}. Springer, New York. Sections 5.1 and 7.6.

Maior, V. Q. S. and Cysneiros, F. J. A. (2018). SYMARMA: a new dynamic model for temporal data on conditional symmetric distribution. \emph{Statitical Paper}, \strong{59}, 75-97. doi: \href{https://doi.org/10.1007/s00362-016-0753-z}{10.1007/s00362-016-0753-z}.
}

\author{
Vinicius Quintas Souto Maior and Francisco Jose A. Cysneiros

Maintainer: Vinicius Quintas Souto Maior <vinicius@de.ufpe.br>
}

\examples{
data(assets)
attach(assets)

# Return in the prices on Microsoft and SP500 index

N = length(msf)
.sp500 = ((sp500[2:N]-sp500[1:(N-1)])/sp500[1:(N-1)])*100
.msf = ((msf[2:N]-msf[1:(N-1)])/msf[1:(N-1)])*100

# The T-bill rates were divided by 253 to convert to a daily rate

.tbill = tbill/253

# Excess return in the d prices on Microsoft and SP500 index

Y = .msf - .tbill[1:(N-1)]
X = .sp500 - .tbill[1:(N-1)]

# Period from April 4, 2002 to October 4, 2002

serie = Y[2122:2240]
aux = cbind(X[2122:2240])

# Fit SYMARMA models

fit.1 = elliptical.ts(serie,order=c(0,0,1),include.mean=FALSE,
 family="Normal")

# Forecasts

predict(fit.1, h=10)
}

