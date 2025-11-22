\name{intraAMM}
\alias{intraAMM}

\title{ Asymptotic Method of Moments Estimator

}
\description{
The intra asset correlation will be estimated by fitting the first two moments of the default rate time series to the theoretical moments of the default rate and backing out the remaining correlation parameter numerically. Additionally, bootstrap and jackknife corrections are implemented.

}
\usage{
intraAMM(d, n, B = 0, DB=c(0,0), JC = FALSE,CI_Boot, type="bca", plot=FALSE)
}

\arguments{
 \item{d}{a vector, containing the default time series of the sector.

}
  \item{n}{a vector, containing the number of obligors at the beginning of the period over time.

}
  \item{B}{an integer, indicating how many bootstrap repetitions should be used for the single bootstrap corrected estimate.
} 
 \item{DB}{a combined vector, indicating how many bootstrap repetitions should be used for the inner (first entry) and outer loop (second entry) to correct the bias using the double bootstrap.
} 
  \item{JC}{ a logical variable, indicating if the jackknife corrected estimate should be calculated.

}
 
  \item{CI_Boot}{a number, indicating the desired confidence interval if the single bootstrap correction is specified. By default, the interval is calculated as the bootstrap corrected and accelerated confidence interval (Bca).
} 
  \item{type}{a string, indicating the desired method to calculate the bootstrap confidence intervals. For more details see \code{\link[boot]{boot.ci}}. Studendized confidence intervals are not supported.
} 
  \item{plot}{a logical variable, indicating whether a plot of the single bootstrap density should be generated.
} 
}
\details{
As stated by \insertCite{vasicek1991;textual}{AssetCorr} and \insertCite{vasicek2002;textual}{AssetCorr}, the asset variables follow a bivariate normal distribution. Since this distribution can be parametrized by the first two moments, namely the expected value and the variance, the remaining correlation parameter can be backed out numerically. The expected value will be approximated by the mean of the default rate and the variance is defined as the sample variance of the default rate. For theoretical background, see \insertCite{gordy2000comparative;textual}{AssetCorr}. If \code{DB} is specified, the single bootstrap corrected estimate will be calculated by using the bootstrap values of the outer loop (\code{oValues}).

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

\insertRef{gordy2000comparative}{AssetCorr}

\insertRef{vasicek1991}{AssetCorr}

\insertRef{vasicek2002}{AssetCorr}


}
\seealso{
\code{\link{intraFMM}}, \code{\link{intraJDP1}}, \code{\link{intraJDP2}},
\code{\link{intraCMM}}, \code{\link{intraMLE}}, \code{\link{intraAMLE}},
\code{\link{intraMode}},\code{\link{intraBeta}}

}
\examples{
set.seed(111)
d=defaultTimeseries(1000,0.3,20,0.01)
n=rep(1000,20)

IntraCorr=intraAMM(d,n)

#Jackknife correction
IntraCorr=intraAMM(d,n, JC=TRUE)

\donttest{
#Bootstrap correction with confidence intervals
IntraCorr=intraAMM(d,n, B=1000, CI_Boot=0.95 )

#Bootstrap correction with confidence intervals and plot
IntraCorr=intraAMM(d,n, B=1000, CI_Boot=0.95, plot=TRUE )

#Double Bootstrap correction with 10 repetitions in the inner loop and 50 in the outer loop
IntraCorr=intraAMM(d,n, DB=c(10,50))
}

}
\keyword{ AMM}
\keyword{ AMM}
