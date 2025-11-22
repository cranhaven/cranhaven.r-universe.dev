\name{intraJDP1}
\alias{intraJDP1}

\title{Joint Default Probability Matching Estimator, Lucas (1995)

}
\description{
The intra asset correlation will be estimated by fitting the empirical joint default probability (JDP) of the default rate time series to the theoretical one and backing out the remaining correlation parameter numerically. The unbiased estimator of \insertCite{lucas1995default;textual}{AssetCorr} will be used for the empirical JDP. Additionally, bootstrap and jackknife corrections are implemented.
}
\usage{
intraJDP1(d, n, B = 0, DB=c(0,0), JC = FALSE, CI_Boot, type="bca", plot=FALSE)
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

\insertCite{kalkbrener2010validating;textual}{AssetCorr} for example showed that the JDP of the default rates follow a bivariate normal distribution. The intra asset correlation will be estimated by fitting the empirical JDP of the default rate time series on the theoretical JDP of the default rate and backing out the remaining correlation parameter numerically. 
The empirical JDP is calculated by the (unbiased) estimator of \insertCite{lucas1995default;textual}{AssetCorr}. If \code{DB} is specified, the single bootstrap corrected estimate will be calculated by using the bootstrap values of the outer loop (\code{oValues}).
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

\insertRef{kalkbrener2010validating}{AssetCorr}

\insertRef{lucas1995default}{AssetCorr}
}


\seealso{
\code{\link{intraAMM}}, \code{\link{intraFMM}}, \code{\link{intraJDP2}},
\code{\link{intraCMM}}, \code{\link{intraMLE}}, \code{\link{intraAMLE}},
\code{\link{intraMode}},\code{\link{intraBeta}}
}
\examples{
set.seed(111)
d=defaultTimeseries(1000,0.3,20,0.01)
n=rep(1000,20)

IntraCorr=intraJDP1(d,n)
#Jackknife correction
IntraCorr=intraJDP1(d,n, JC=TRUE)

\donttest{
#Bootstrap correction with confidence intervals
IntraCorr=intraJDP1(d,n, B=1000, CI_Boot=0.95 )

#Bootstrap correction with confidence intervals and plot
IntraCorr=intraJDP1(d,n, B=1000, CI_Boot=0.95, plot=TRUE )

#Double Bootstrap correction with 10 repetitions in the inner loop and 50 in the outer loop
IntraCorr=intraJDP1(D1,N1, DB=c(10,50))

}

}
\keyword{JDP1}
\keyword{JDP1}
