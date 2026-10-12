\name{qqplot}
\alias{qqplot}
\title{Quantile-Quantile Plots}
\description{This function produces Q-Q plot with envelopes for a time series following conditional symmetric distribution.}

\usage{qqplot(model, envelope = 0.95 , B = 400)}
\arguments{
  \item{model}{a result of a call to \code{\link{elliptical.ts}}.}
  \item{envelope}{confidence level for point-wise confidence envelope, or FALSE for no envelope.}
  \item{B}{integer; number of bootstrap replications for confidence envelope. Default is 400 iterations.}
}

\references{
Cleveland, W.S. (1994). \emph{The Elements of Graphing Data}, Hobart Press.

Thode, Henry C. (2002). \emph{Testing for normality}, New York: Marcel Dekker.
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

fit.1 = elliptical.ts(serie,order=c(0,0,1),xreg=aux,include.mean=FALSE,
 family="Normal")
fit.2 = elliptical.ts(serie,order=c(0,0,1),xreg=aux,include.mean=FALSE,
 family="Student",index1=4)

# Q-Q Plots

qqplot(fit.1, B = 50)
qqplot(fit.2, envelope = FALSE)
}

