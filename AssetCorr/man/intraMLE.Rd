\name{intraMLE}
\alias{intraMLE}

\title{Binomial Maximum Likelihood Estimator

}
\description{
The default process in the Vasicek model follows a binomial distribution, conditional on the realization of the systematic factor. Hence, the intra asset correlation can be estimated by maximizing the Vasicek-binomial log likelihood.
}
\usage{
intraMLE(d, n, B =0, DB=c(0,0), JC = FALSE, CI, CI_Boot, type="bca", plot=FALSE)
}

\arguments{
 \item{d}{a vector, containing the default time series of the sector.

}
  \item{n}{a vector, containing the number of obligors at the beginning of the period in the sector.

}
 \item{B}{an integer, indicating how many bootstrap repetitions should be used for the single bootstrap corrected estimate.
} 
 \item{DB}{a combined vector, indicating how many bootstrap repetitions should be used for the inner (first entry) and outer loop (second entry) to correct the bias using the double bootstrap.
} 
  \item{JC}{ a logical variable, indicating if the jackknife corrected estimate should be calculated.

}
 \item{CI}{a number, indicating the desired asymptotic confidence interval of the estimate. 

}

 \item{CI_Boot}{a number, indicating the desired bootstrap confidence interval of the estimate, if the single bootstrap correction is specified. By default, the interval is calculated as the bootstrap corrected and accelerated confidence interval (Bca). Furthermore, the analytical confidence intervals are provided, using the same value as \code{CI_Boot}.

}
 \item{type}{a string, indicating  the type of bootstrap intervals. For more details see \code{\link[boot]{boot.ci}}.

}
 \item{plot}{a logical variable, indicating whether a plot of the single bootstrap density should be generated.
} 
}
\details{
In the Vasicek model the default process can be expressed as independent binomial trials, conditional on the realization of the systematic factor. Therefore \insertCite{gordy2010small;textual}{AssetCorr} introduced an estimator of the intra asset correlation based on the log-likelihood maximization. For theoretical background see \insertCite{gordy2010small;textual}{AssetCorr}. 

The log-likelihood will be maximized for values between 0 and 1. The asymptotic confidence interval will be estimated using maximum likelihood theory. If \code{DB} is specified, the single bootstrap corrected estimate will be calculated by using the bootstrap values of the outer loop (\code{oValues}).
}
\value{

The returned value is a list, containing the following components (depending on the selected arguments):

\item{Original}{Estimate of the original method}
\item{Bootstrap}{Bootstrap corrected estimate}
\item{Double_Bootstrap}{Double bootstrap corrected estimate}
\item{Jackknife}{Jackknife corrected estimate}
\item{CI}{Selected two-sided asymptotic confidence interval}
\item{CI_Boot}{Selected two-sided bootstrap confidence interval}
\item{bValues}{Estimates from the single bootstrap resampling}
\item{iValues}{Estimates from the double bootstrap resampling- inner loop}
\item{oValues}{Estimates from the double bootstrap resampling- outer loop}
}
\references{
\insertRef{chang2015double}{AssetCorr}

\insertRef{efron1994introduction}{AssetCorr}

\insertRef{gordy2010small}{AssetCorr}

}

\seealso{
\code{\link{intraAMM}}, \code{\link{intraFMM}}, \code{\link{intraJDP2}},
\code{\link{intraCMM}}, \code{\link{intraJDP1}}, \code{\link{intraAMLE}},
\code{\link{intraMode}},\code{\link{intraBeta}}
}
\examples{
set.seed(11)
d=defaultTimeseries(1000,0.3,20,0.01)
n=rep(1000,20)

IntraCorr=intraMLE(d,n)

#Estimation with confidence intervals
IntraCorr=intraMLE(d,n, CI=0.95 )





\donttest{
#Jackknife correction
IntraCorr=intraMLE(d,n, JC=TRUE)

#Bootstrap correction with bootstrap confidence intervals
IntraCorr=intraMLE(d,n, B=1000, CI_Boot=0.95 )

#Bootstrap correction with bootstrap confidence intervals and plot
IntraCorr=intraMLE(d,n, B=1000, CI_Boot=0.95, plot=TRUE )

#Double Bootstrap correction with 10 repetitions in the inner loop and 50 in the outer loop
IntraCorr=intraMLE(D1,N1, DB=c(10,50))

}

}
\keyword{MLE}
\keyword{MLE}