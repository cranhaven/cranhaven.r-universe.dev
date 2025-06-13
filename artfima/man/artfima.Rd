\name{artfima}
\alias{artfima}

\title{MLE for ARTFIMA model}

\description{
Maximum likelihood estimation of the ARTFIMA model as well as the edge cases
ARIMA and ARFIMA. Exact MLE and Whittle approximate MLE are implemented.
}
\usage{
artfima(z, glp = c("ARTFIMA", "ARFIMA", "ARIMA"), arimaOrder = c(0, 0, 0), 
  likAlg = c("exact", "Whittle"),  fixd = NULL, b0 = NULL, 
         lambdaMax = 3, dMax = 10)
}

\arguments{
  \item{z}{time series data}
  \item{glp}{general linear process type: ARTFIMA, ARFIMA or ARMA.}
  \item{arimaOrder}{c(p,D,q), where p is the AR order, D is the regular difference
  parameter and q is the MA order.}
  \item{likAlg}{"exact" or "Whittle" or "Whittle2"}
  \item{fixd}{only used with ARTFIMA, default setting fixd=NULL means the MLE
  for the parameter d is obtained other if fixed=d0, where d0 is a numeric
  value in the interval (-2, 2) the d parameter in ARTFIMA is fixed at this 
  value while the remaining parameters are estimated.}
  \item{b0}{initial estimates - use only for high order AR models. See Details
  and Example.}
  \item{lambdaMax}{ARTFIMA boundard setting - upper limit for lambda}
  \item{dMax}{ARTFIMA boundard setting - absolute magnitude for d. See Note and
     Example}
}

\details{
The ARFIMA and ARIMA are subsets or edge-cases
of the ARTFIMA model. The likelihood and probability density function
for these models is defined by the multivariate normal distribution.
The log-likelihood, AIC and BIC are comparable across models.
When the Whittle MLE algorithm is used, the final log-likelihood
is obtained by plugging this estimates into the exact log-likelihood.

The argument b0 is provided for fitting for fitting high order AR models
with ARTFIMA. That is ARTFIMA(p,0,0) when p is large. This fitting is best done
by fitting values with p=1,2,...,pmax. For p>1, set b0 equal to c(ans$b0, 0),
where ans is the output from artfima for the p-1 order model. An example
is given below. This technqiue is used by bestModels with q=0 and p>3.
}

\note{
Note: ARTFIMA parameters d and lambda on the boundary.
The output from this function is normally viewed using the print method that
has been implemented for class artfima. Check this output to see if any of 
the estimates are on the boundary. This may happen with the lambda or d 
parameter estimates in ARTFIMA. Another famous case is with the MA(1) models.
Often when this happens the model is not statistically adequate because it is
too parsimonious or otherwise mis-specified. For example an AR(1) instead
of an MA(1). See the R code for artfima if you wish to change the boundary
limits set on the parameters - only for researchers not recommended otherwise.
}

\value{A lengthy list is produced. A terse summary is provided by the associated
print method.
}

\references{
McLeod, A.I., Yu, Hao and Krougly, Z. (2007).  
Algorithms for Linear Time Series Analysis: With R Package.  
Journal of Statistical Software  23/5 1-26.
}  

\author{A. I. McLeod, aimcleod@uwo.ca}

\seealso{
\code{\link{bestModels}}
}

\examples{
artfima(Nile) #Nile is a built in dataset in R
artfima(Nile, likAlg = "exact")
#
#fitting a high-order AR using recursion
\dontrun{
#This may take 3 to 6 hours if exact MLE used!
#But Whittle MLE doesn't work properly for this example!!
 data(SB32)
 z <- SB32
 likAlg <- "exact"
 pmax <- 30
 startTime <- proc.time()[3]
 ic <- matrix(numeric(0), ncol=3, nrow=pmax+1)
 out <- artfima(z, arimaOrder=c(0,0,0), likAlg=likAlg)
 ic[1, 1] <- out$aic
 ic[1, 2] <- out$bic
 ic[1, 3] <- out$LL
 b1 <- c(out$b0, 0)
 for (i in 1:pmax) {
  out <- artfima(z, arimaOrder=c(i,0,0), b0=b1, likAlg=likAlg)
  b1 <- c(out$b0, 0)
  ic[i+1, 1] <- out$aic
  ic[i+1, 2] <- out$bic
  ic[i+1, 3] <- out$LL
 }
 endTime <- proc.time()[3]
 (totTime <- endTime-startTime)
 plot(0:pmax, ic[,1], xlab="AR order", ylab="AIC", pch=20, col="blue")
 indBest <- which.min(ic[,1])
 pBest <- indBest-1
 icBest <- ic[indBest,1]
 abline(h=icBest, col="brown")
 abline(v=pBest, col="brown")
 plot(0:pmax, ic[,2], xlab="AR order", ylab="BIC", pch=20, col="blue")
 indBest <- which.min(ic[,2])
 pBest <- indBest-1
 icBest <- ic[indBest,2]
 abline(h=icBest, col="brown")
 abline(v=pBest, col="brown")
 plot(0:pmax, ic[,3], xlab="AR order", ylab="log-lik", pch=20)
 }#end dontrun
#
#setting new boundary limit
\dontrun{
data(SB32)
#ARTFIMA(1,0,2) - MLE for d on boundar, dHat = 10
artfima(SB32, arimaOrder=c(1,0,2))
#note:
#log-likelihood = -10901.14, AIC = 21816.29, BIC = 21862.41
#Warning: estimates converged to boundary!
#mean     -0.5558988 8.443794e-02
#d         9.9992097 1.396002e-05
#lambda    2.9304658 8.050071e-02
#phi(1)    0.9271892 6.862294e-03
#theta(1)  0.8440911 1.709824e-02
#theta(2) -0.3650004 2.744227e-02
#
#now reset upper limit dMax and lambdaMax
#NOTE - there is only a very small improvement in the log-likelihood
artfima(SB32, arimaOrder=c(1,0,2), lambdaMax=20, dMax=40)
#ARTFIMA(1,0,2), MLE Algorithm: exact, optim: BFGS
#snr = 4.665, sigmaSq = 3.38228734331338
#log-likelihood = -10900.56, AIC = 21815.12, BIC = 21861.25
#               est.    se(est.)
#mean     -0.5558988  0.08443794
#d        27.0201256 36.94182328
#lambda    3.9412050  1.38296970
#phi(1)    0.9276901  0.00676589
#theta(1)  0.8342879  0.01715041
#theta(2) -0.3644787  0.02691869
} 
 
}

\keyword{ts}