\name{intraAMLE}
\alias{intraAMLE}

\title{Asymptotic Maximum Likelihood Estimator
}
\description{
This estimator is based on the assumption of infinitely large portfolio size and a sufficient length of the default time series. In the asymptotic case, neither the default rate of 0 or a 1 occur. Hence one has to make an adjustment in these cases.

}
\usage{
intraAMLE(d, n, B = 0, DB=c(0,0),JC = FALSE, Adjust = 0,
CI_1,CI_2, CI_Boot,VaR=0.99, VaR_CI=0.95, ES=0.975, ES_CI=0.95,
type="bca", plot=FALSE)
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
 
  \item{Adjust}{a number, which should be added to a observed default rate of 0 or subtracted form a observed default rate of 1.

}

 \item{CI_1}{a number, indicating the desired analytical confidence interval of the estimate. The interval is computed by asymptotic Cramer-Rao lower bound for the standard deviation of the estimate based on \insertCite{dullmann2004systematic;textual}{AssetCorr}. Additionally the asymptotic confidence interval for the unconditional PD is computed.

}
 \item{CI_2}{a number, indicating the desired analytical confidence interval of the estimate. The interval is computed by constructing a confidence interval for the variance, which can be transferred to the estimate based on \insertCite{hose2011confidence;textual}{AssetCorr}.

}

  \item{CI_Boot}{a number, indicating the desired confidence interval if the single bootstrap correction is specified.
  By default, the interval is calculated as the bootstrap corrected and accelerated confidence interval (Bca). Furthermore, the analytical confidence intervals are provided, using the same value as \code{CI_Boot}.
}
  \item{VaR}{a number, indicating the desired confidence level for the asymptotic Value-at-Risk.
}
\item{VaR_CI}{ a number, indicating the desired confidence interval for the asymptotic Value-at-Risk, derived by the delta method.}

  \item{ES}{a number, indicating the desired confidence level for the asymptotic Expected Shortfall.
}
\item{ES_CI}{ a number, indicating the desired confidence interval for the asymptotic Expected Shortfall, derived by the delta method.}
  \item{type}{a string, indicating the desired method to calculate the confidence intervals. For more details see \code{\link[boot]{boot.ci}}.
} 
 \item{plot}{a logical variable, indicating whether a plot of the single bootstrap density should be generated.
}
}
\details{
This estimator is based on the asymptotic assumptions of the Vasicek model, especially the assumption of an infinite large portfolio. If a 0 or 1 occurs at least once in the default rate time series, the estimator can not converge. Therefore, an adjustment (\code{Adjust}) can be made. Nevertheless, \insertCite{meyer2009estimation;textual}{AssetCorr} argued that the adjustment can harm the estimation efficiency.

Additionally two different asymptotic confidence intervals are provided. Bootstrap and jackknife corrections are also possible. If \code{DB} is specified, the single bootstrap corrected estimate will be calculated using the bootstrap values of the outer loop (\code{oValues}).

}
\value{

The returned value is a list, containing the following components (depending on the selected arguments):
\item{PD}{Estimate of the unconditional PD based on \insertCite{dullmann2004systematic;textual}{AssetCorr}}
\item{PD_CI_1}{
[Two-sided asymptotic confidence interval for the unconditional PD based on \insertCite{dullmann2004systematic;textual}{AssetCorr}}
\item{Original}{Estimate of the original method}
\item{Bootstrap}{Bootstrap corrected estimate}
\item{Double_Bootstrap}{Double bootstrap corrected estimate}
\item{Jackknife}{Jackknife corrected estimate}
\item{CI_1}{Selected two-sided asymptotic confidence interval based on \insertCite{dullmann2004systematic;textual}{AssetCorr}}
\item{CI_2}{Selected two-sided asymptotic confidence interval based on \insertCite{hose2011confidence;textual}{AssetCorr}}
\item{CI_Boot}{Selected two-sided bootstrap confidence interval}
\item{VaR}{Asymptotic Value-at-Risk}
\item{VaR_CI}{Confidence interval for the asymptotic Value-at-Risk}
\item{ES}{Asymptotic Expected Shortfall}
\item{ES_CI}{Confidence interval for the asymptotic Expected Shortfall}
\item{bValues}{Estimates from the single bootstrap resampling}
\item{iValues}{Estimates from the double bootstrap resampling- inner loop}
\item{oValues}{Estimates from the double bootstrap resampling- outer loop}
}
\references{
\insertRef{chang2015double}{AssetCorr}

\insertRef{dullmann2004systematic}{AssetCorr}

\insertRef{efron1994introduction}{AssetCorr}

\insertRef{hose2011confidence}{AssetCorr}

\insertRef{meyer2009estimation}{AssetCorr}

\insertRef{Pfeuffer2018}{AssetCorr}

}



\seealso{
\code{\link{intraAMM}}, \code{\link{intraFMM}}, \code{\link{intraJDP2}},
\code{\link{intraMLE}}, \code{\link{intraJDP1}}, \code{\link{intraCMM}},
\code{\link{intraMode}},\code{\link{intraBeta}}}
\examples{
set.seed(10)
d=defaultTimeseries(100,0.01,10,0.01)
n=rep(100,10)

#Sensitivity to the adjustment
intraAMLE(d,n,Adjust=0.001)
intraAMLE(d,n,Adjust=0.0001)

#Estimation with confidence intervals- I
IntraCorr=intraAMLE(d,n, Adjust=0.001, CI_1=0.95 )

#Estimation with confidence intervals- II
IntraCorr=intraAMLE(d,n, Adjust=0.001, CI_2=0.95 )

#Jackknife correction
IntraCorr=intraAMLE(d,n,Adjust=0.001, JC=TRUE)

\donttest{
#Bootstrap correction with  bootstrap confidence intervals
IntraCorr=intraAMLE(d,n, Adjust=0.001, B=1000, CI_Boot=0.95 )

#Bootstrap correction with  bootstrap confidence intervals and plot
IntraCorr=intraAMLE(d,n, B=1000, Adjust=0.001, CI_Boot=0.95, plot=TRUE )

#Double Bootstrap correction with 10 repetitions in the inner loop and 50 in the outer loop
IntraCorr=intraAMLE(d,n,Adjust=0.001, DB=c(10,50))

}


}
\keyword{AMLE}
\keyword{AMLE}
