\name{elliptical.ts}
\alias{elliptical.ts}
\title{Autoregressive and Moving Average Symmetric Models}
\description{Fit an SYMARMA model to a univariate time series. Fitting method: conditional maximum likelihood estimation.}

\usage{
elliptical.ts(Y, family="Normal", order=c(0,0,0), xreg=NULL, 
include.mean=TRUE, epsilon=0.0001, maxit=100, trace="TRUE",
index1=NULL, index2=NULL, fixed=NULL)
}

\arguments{
  \item{Y}{a univariate time series.}
  \item{family}{a description of the conditional distribution of each \emph{Y[t]}, given the set of past information. Symmetric distributions available for adjustment: Normal (\emph{Normal}), Student-\emph{t} (\emph{Student}), Generalized Student-\emph{t} (\emph{Gstudent}), Exponential Power (\emph{ExpPower}) (by Box & Tiao, 1973, ch 3), Logistic I (\emph{LogisticI}), Logistic II (\emph{LogisticII}), Generalized Logistic (\emph{Glogistic}), Cauchy (\emph{Cauchy}) and Contamined Normal (\emph{Cnormal}). The default is to normal distribution.}
  \item{order}{a specification of the SYMARMA model: the three integer components (p, d, q) are the AR order, the degree of differencing, and the MA order.}
  \item{xreg}{optionally, a vector or matrix of external regressors, which must have the same number of rows as \emph{Y}.}
  \item{include.mean}{should the SYMARMA model include a mean/intercept term? The default is TRUE.}
  \item{epsilon}{positive convergence tolerance \emph{e}; the iterations converge when |fit - fit_{old}|/|fit| < \emph{e}. Default is \emph{e=1e-04}.}
  \item{maxit}{integer giving the maximal number of iterations. Default is 100 iterations.}
  \item{trace}{a logical indicating if output should be produced.}
  \item{index1}{The parameter to Student-\emph{t} and Exponential Power distributions or the first argument to Generalized Student-\emph{t}, Generalized Logistic and Contamined Normal distributions.}
  \item{index2}{The second argument to Generalized Student-\emph{t}, Generalized Logistic (\emph{index2 = index2(index1)}) and Contamined Normal distributions.}
  \item{fixed}{a optional numeric vector of the same length as the total number of parameters. If supplied, only NA entries in fixed will be varied.}
}

\details{
Different definitions of autoregressive and moving average models have different signs for the AR and/or MA coefficients.
 The dynamic component in SYMARMA model used here has

\eqn{Y[t] =  X[t]Beta + phi[1](Y[t-1] - X[t-1]Beta) + ... + phi[np](Y[t-np] - X[t-np]Beta) + theta[1]erro[t-1] + ... + theta[nq]erro[t-nq] + erro[t].}

The estimation of the parameters that index the SYMARMA model is obtained by maximum conditional likelihood method on the first \emph{m} observations, where \emph{m} = max(np,nq). 

The variance matrix of the estimates is found from the Hessian of the log-likelihood, and so may only be a rough guide.

}

\value{
A list of class \dQuote{Symarma} with components:

\item{coefficients}{a vector of estimated AR, MA and regression coefficients.}
\item{dispersion}{the estimated dispersion parameter.}
\item{resid.raw}{the ordinary residuals.}
\item{resid.stand}{the standardized residuals.}
\item{fitted.values}{the fitted mean values.}
\item{loglik}{the maximized log-likelihood.}
\item{aic}{the AIC value corresponding to the log-likelihood.}
\item{bic}{the BIC value corresponding to the log-likelihood.}
\item{rmse}{the Root Mean Squared Error value corresponding to the ajusted model.}
\item{iter}{the number of iterations used in the fitting.}
\item{n}{the number of observations in the series.}
\item{sd.coef}{a vector of estimated standard deviation of the coefficients.}
\item{sd.disp}{estimated standard deviation of the dispersion parameter.}
\item{family}{the family object used.}
\item{X}{if requested, the vector or matrix of external regressors.}
}

\references{
Maior, V. Q. S. and Cysneiros, F. J. A. (2018). SYMARMA: a new dynamic model for temporal data on conditional symmetric distribution. \emph{Statitical Paper}, \strong{59}, 75-97. doi: \href{https://doi.org/10.1007/s00362-016-0753-z}{10.1007/s00362-016-0753-z}.

Wei, W. W. S. (2006). \emph{Time Series Analysis: Univariate and Multivariate Methods}, 2nd edition. Pearson Addison Wesley. Section 7.2.1.

Box, M. J. and Tiao, G. C. (1973). \emph{Bayesian inference in statistical analysis}. Londen: Addison-Wesley.
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

# Excess return in the prices on Microsoft and SP500 index

Y = .msf - .tbill[1:(N-1)]
X = .sp500 - .tbill[1:(N-1)]

# Period from April 4, 2002 to October 4, 2002

serie = Y[2122:2240]
aux = cbind(X[2122:2240])

# Returns best ARIMA model according to either AIC value.
# auto.arima(Y,xreg=aux,seasonal=FALSE,ic=c("aic"))

# Fit SYMARMA models

fit.1 = elliptical.ts(serie,order=c(0,0,1),xreg=aux,include.mean=FALSE,
 family="Normal")
fit.2 = elliptical.ts(serie,order=c(0,0,1),xreg=aux,include.mean=FALSE,
 family="Student", index1=4)
fit.3 = elliptical.ts(serie,order=c(3,0,1),xreg=aux,family="ExpPower",
 index1=0, fixed=c(0,0,NA,NA,NA,NA))
}
