\name{interCMM}
\alias{interCMM}

\title{ Corrected Asymptotic Method of Moments Estimator of \insertCite{frei2017moment;textual}{AssetCorr}

}
\description{
This method provides analytical bias correction via Taylor series expansion. Additionally the bias due to autocorrelated default time series can be corrected. The estimated parameter is the inter correlation of the asset variables (in contrast to all other  inter correlation methods of this package).
}
\usage{
interCMM(d1,n1,d2,n2,rho,l=0, B=0,DB=c(0,0), JC=FALSE,
CI_Boot, type="bca", plot=FALSE)
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
  \item{rho}{estimated inter asset correlation of another estimator.

}
  \item{l}{a number, indicating how many lags of autocorrelation should be used for the correction.

}
    \item{B}{an integer, indicating how many bootstrap repetitions should be used for the single bootstrap corrected estimate.
}
\item{DB}{a combined vector, indicating how many bootstrap repetitions should be used for the inner (first entry) and outer loop (second entry) to correct the bias using the double bootstrap.
}
  \item{JC}{ a logical variable, indicating if the jackknife corrected estimate should be calculated.

}

  \item{CI_Boot}{a number, indicating the desired confidence level if the single bootstrap correction is specified. By default, the interval is calculated as the bootstrap corrected and accelerated confidence interval (Bca). Furthermore, the analytical confidence intervals are provided, using the same value as \code{CI_Boot}.
} 
  \item{type}{a string, indicating the desired method to calculate the confidence intervals. For more details see \code{\link[boot]{boot.ci}}.
} 
 \item{plot}{a logical variable, indicating whether a plot of the single bootstrap density should be generated.
}
}
\details{
This function estimates the inter correlation of the asset variables. In general, the inter correlation can be estimated for the asset variables or the systematic factors. This estimator uses a inter correlation estimate of another method to correct the bias due to small sample or autocorrelation. Only one parameter (inter correlation) must be estimated.  Additionally, asymptotic confidence interval can be provided, as shown by \insertCite{frei2017moment;textual}{AssetCorr}. The inter correlation of the asset variables can be transformed to the correlation of the systematic factors as follows:

\deqn{rho_Systematic= rho_Asset/sqrt(rho_1*rho_2)}

If \code{DB} is specified, the single bootstrap corrected estimate will be calculated using the bootstrap values of the outer loop (\code{oValues}).
}
\value{

The returned value is a list, containing the following components (depending on the selected arguments):

\item{Original}{Estimate of the original method}
\item{Bootstrap}{Bootstrap corrected estimate}
\item{Double_Bootstrap}{Double bootstrap corrected estimate}
\item{Jackknife}{Jackknife corrected estimate}
\item{CI_Boot}{Selected two-sided bootstrap confidence interval}
\item{bValues}{Estimates from the bootstrap resampling}
\item{iValues}{Estimates from the double bootstrap resampling- inner loop}
\item{oValues}{Estimates from the double bootstrap resampling- outer loop}
}
\references{
\insertRef{chang2015double}{AssetCorr}

\insertRef{efron1994introduction}{AssetCorr}

\insertRef{frei2017moment}{AssetCorr}


}




\seealso{
\code{\link{interJDP}}, \code{\link{interCopula}}, \code{\link{interMLE}}, \code{\link{interCov}}}
\examples{
d1=defaultTimeseries(1000,0.1,10,0.01)
d2=defaultTimeseries(1000,0.2,10,0.01)
n1=n2=rep(1000,10)

#Using the Covariance method to estimate the plug-in inter correlation.
inter_sys=interCov(d1,n1,d2,n2,0.1,0.2)$Original
inter_asset= inter_sys*sqrt(0.1*0.2)

interCMM(d1,n1,d2,n2,inter_asset,l=0)

\donttest{
InterCorr=interCMM(d1,n1,d2,n2,inter_asset, JC=TRUE)
InterCorr=interCMM(d1,n1,d2,n2,inter_asset, B=1000, CI_Boot=0.95, plot=TRUE)


InterCorr=interCMM(d1,n1,d2,n2,inter_asset, DB=c(10,50))
}

}
\keyword{interCMM}
\keyword{interCMM}
