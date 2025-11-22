\name{intraCMM}
\alias{intraCMM}

\title{Corrected Asymptotic Method of Moments Estimator of \insertCite{frei2017moment;textual}{AssetCorr}

}
\description{
The estimator is based on \insertCite{frei2017moment;textual}{AssetCorr}, who introduced an analytic bias correction via Taylor series expansion. Additionally, the bias due to autocorrelated default time series can be corrected. Furthermore, additional bootstrap and jackknife bias corrections are implemented.
}
\usage{
intraCMM(d,n,l=0, B=0, DB=c(0,0), JC=FALSE , CI_Boot,type="bca", plot=FALSE)
}


 \arguments{
 \item{d}{a vector, containing the default time series of the sector.

}
  \item{n}{a vector, containing the number of obligors at the beginning of the period in the sector.

}
  \item{l}{a number, indicating how many lags of autocorrelation should be used for the correction.

}
 \item{B}{an integer, indicating how many bootstrap repetitions should be used for the single bootstrap corrected estimate.
} 

 \item{DB}{a combined vector, indicating how many bootstrap repetitions should be used for the inner (first entry) and outer loop (second entry) to correct the bias using the double bootstrap.
} 
   \item{JC}{ a logical variable, indicating if the jackknife corrected estimate should be calculated.

}



  \item{CI_Boot}{a number, indicating the desired confidence level if the single bootstrap correction is specified. By default, the interval is calculated as the bootstrap corrected and accelerated confidence interval (Bca).Furthermore, the analytical confidence intervals are provided, using the same value as \code{CI_Boot}.
} 

  \item{type}{a string, indicating the desired method to calculate the confidence intervals. For more details see \code{\link[boot]{boot.ci}}.
}
 \item{plot}{a logical variable, indicating whether a plot of the single bootstrap density should be generated.
} 
}
\details{
\insertCite{frei2017moment;textual}{AssetCorr} introduced an estimator which simultaneously corrects the bias of a short default time series and an bias due to autocorrelation. Additionally, further corrections are implemented. If \code{DB} is specified, the single bootstrap corrected estimate will be calculated by using the bootstrap values of the outer loop (\code{oValues}).}
\value{

The returned value is a list, containing the following components (depending on the selected arguments):

\item{Original}{Estimate of the original method}
\item{Bootstrap}{Bootstrap corrected estimate}
\item{Double_Bootstrap}{Double bootstrap corrected estimate}
\item{Jackknife}{Jackknife corrected estimate}
\item{CI_Boot}{Selected two-sided bootstrap confidence interval}
\item{bValues}{Estimates from the single bootstrap resampling}
\item{iValues}{Estimates from the double bootstrap resampling- inner loop}
\item{oValues}{Estimates from the double bootstrap resampling- outer loop}
}
\references{
\insertRef{chang2015double}{AssetCorr}

\insertRef{efron1994introduction}{AssetCorr}

\insertRef{frei2017moment}{AssetCorr}


}

\seealso{
\code{\link{intraAMM}}, \code{\link{intraFMM}}, \code{\link{intraJDP2}},
\code{\link{intraMLE}}, \code{\link{intraJDP1}}, \code{\link{intraAMLE}},
\code{\link{intraMode}},\code{\link{intraBeta}}
}
\examples{
set.seed(10)
d=defaultTimeseries(1000,0.1,10,0.01)
n=rep(1000,10)

IntraCorr=intraCMM(d,n,l=0)


#Estimation with  two lags of autocorrelation
IntraCorr=intraCMM(d,n, l=2 )

#Jackknife correction
IntraCorr=intraCMM(d,n, JC=TRUE)



\donttest{
#Bootstrap correction with  bootstrap confidence intervals
IntraCorr=intraCMM(d,n, B=1000, CI_Boot=0.95 )

#Bootstrap correction with  bootstrap confidence intervals and plot
IntraCorr=intraCMM(d,n, B=1000, CI_Boot=0.95, plot=TRUE )

#Double Bootstrap correction with 10 repetitions in the inner loop and 50 in the outer loop
IntraCorr=intraCMM(d,n, DB=c(10,50))

}
}
\keyword{CMM}
\keyword{CMM}
