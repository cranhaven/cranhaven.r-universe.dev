\name{interMLE}
\alias{interMLE}

\title{
Binomial Maximum Likelihood Estimator
}
\description{
The inter correlation parameter can be estimated by maximizing the Vasicek-binomial log-likelihood. The default process in the Vasicek model follows a binomial distribution, conditional on the realisation of the systematic factor. Hence, the inter correlation of the systematic factor can be estimated by maximizing the resulting log likelihood. 
}
\usage{
interMLE(d1, n1, d2, n2, rho1, rho2, B = 0, DB=c(0,0), JC = FALSE,
CI, plot=FALSE)
}

\arguments{
  \item{d1}{a vector, containing the default time series of sector 1.

}
  \item{n1}{a vector, containing the number of obligors at the beginning of the period in sector 1.

}
  \item{d2}{a vector, containing the default time series of sector 2.

}
  \item{n2}{a vector, containing the number of obligors at the beginning of the period in sector 2.

}
  \item{rho1}{estimated intra correlation of sector 1.

}
  \item{rho2}{estimated intra correlation of sector 2.

}
    \item{B}{an integer, indicating how many bootstrap repetitions should be used for the single bootstrap corrected estimate.
}
\item{DB}{a combined vector, indicating how many bootstrap repetitions should be used for the inner (first entry) and outer loop (second entry) to correct the bias using the double bootstrap.
} 
  \item{JC}{ a logical variable, indicating if the jackknife corrected estimate should be calculated.

}

  \item{CI}{a number, indicating the desired asymptotic confidence bound of the estimate. 

}
 
 \item{plot}{a logical variable, indicating whether a plot of the single bootstrap density should be generated.
}
}
\details{
This function estimates the inter correlation of the systematic factor. In general, the inter correlation can be estimated for the asset variables or the systematic factors. To ensure the traceability of the estimation, the intra correlation estimates will be used as plug-in estimates. Hence only one parameter (inter correlation) must be estimated. The inter correlation of the systematic factor can be transformed to the correlation of the asset variables as follows:

\deqn{rho_Asset= rho_Systematic*sqrt(rho_1*rho_2)}

The estimated inter correlation of the systematic factors lies between -1 and 1.

If \code{DB} is specified, the single bootstrap corrected estimate will be calculated using the bootstrap values of the outer loop (\code{oValues}).
}
\value{

The returned value is a list, containing the following components (depending on the selected arguments):

\item{Original}{Estimate of the original method}
\item{Bootstrap}{Bootstrap corrected estimate}
\item{Double_Bootstrap}{Double bootstrap corrected estimate}
\item{Jackknife}{Jackknife corrected estimate}
\item{CI}{Selected two-sided asymptotic bootstrap confidence interval}
\item{CI_Boot}{Selected two-sided bootstrap confidence interval}
\item{bValues}{Estimates from the bootstrap resampling}
\item{iValues}{Estimates from the double bootstrap resampling- inner loop}
\item{oValues}{Estimates from the double bootstrap resampling- outer loop}
}
\references{
\insertRef{chang2015double}{AssetCorr}

\insertRef{efron1994introduction}{AssetCorr}


\insertRef{gordy2010small}{AssetCorr}
}



\seealso{
\code{\link{interJDP}}, \code{\link{interCopula}}, \code{\link{interCMM}}, \code{\link{interCov}}}
\examples{

\donttest{
d1=defaultTimeseries(1000,0.1,10,0.01)
d2=defaultTimeseries(1000,0.2,10,0.01)
n1=n2=rep(1000,10)

InterCorr=interMLE(d1,n1,d2,n2,0.1,0.2, CI=0.95)

InterCorr=interMLE(d1,n1,d2,n2,0.1,0.2, JC=TRUE)

InterCorr=interMLE(d1,n1,d2,n2,0.1,0.2, B=1000, CI_Boot=0.95)

InterCorr=interMLE(d1,n1,d2,n2,0.1,0.2, DB=c(10,50))
}



}
\keyword{interMLE}
\keyword{interMLE}
