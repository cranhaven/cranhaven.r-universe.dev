\name{intraBeta}
\alias{intraBeta}

\title{ Parametric Approach of \insertCite{botha2010implied;textual}{AssetCorr}- Beta Distribution

}
\description{
The intra asset correlation will be estimated by fitting a beta distribution onto the default rate time series, then calculating the Value-at-Risk (VaR) of this beta distribution and fit it to the theoretical VaR of the Vasicek distribution. The correlation parameter will be backed out numerically. Additionally, bootstrap and jackknife corrections are implemented.

}
\usage{
intraBeta(d, n, Quantile=0.999,B = 0, DB=c(0,0), JC = FALSE,
CI_Boot, type="bca", plot=FALSE)
}

\arguments{
 \item{d}{a vector, containing the default time series of the sector.

}
  \item{n}{a vector, containing the number of obligors at the beginning of the period over time.

}
\item{Quantile}{a number, indicating the desired confidence level of the Value-at-Risk.

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
As stated by \insertCite{botha2010implied;textual}{AssetCorr} one can estimate the intra correlation by matching VaR of a parametrized beta distribution onto the VaR of the Vasicek distribution. To do so, the shape parameters (alpha and beta) of the beta distribution are estimated according to \insertCite{botha2010implied;textual}{AssetCorr}. Afterwards, the VaR_Beta at the confidence level of \code{Quantile} will be estimated. In a third step, this VaR_Beta is matched with the theoretical VaR of the Vasicek distribution, given by \insertCite{vasicek1991;textual}{AssetCorr}:

\deqn{VaR_Vasicek= Phi((Phi^-1(PD)+sqrt(rho)*Phi^-1(Quantile))/sqrt(1-rho))}

Since \code{Quantile} and the corresponding VaR_Beta is known, the intra correlation parameter can be backed out numerically.
This estimator is sensitive to the chosen \code{Quantile}. \insertCite{botha2010implied;textual}{AssetCorr} suggested to use \code{Quantile=0.999}, but for validation purposes one may choose different values of \code{Quantile} to infer information about the robustness of the correlation estimate.

If \code{DB} is specified, the single bootstrap corrected estimate will be calculated by using the bootstrap values of the outer loop (\code{oValues}).

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
\insertRef{botha2010implied}{AssetCorr} 

\insertRef{chang2015double}{AssetCorr}

\insertRef{efron1994introduction}{AssetCorr}

\insertRef{gordy2000comparative}{AssetCorr}

\insertRef{vasicek1991}{AssetCorr}




}
\seealso{
\code{\link{intraFMM}}, \code{\link{intraJDP1}}, \code{\link{intraJDP2}}
\code{\link{intraCMM}}, \code{\link{intraMLE}}, \code{\link{intraAMLE}}
\code{\link{intraMode}}
}
\examples{
set.seed(111)
d=defaultTimeseries(1000,0.3,20,0.01)
n=rep(1000,20)

IntraCorr=intraBeta(d,n)

#Jackknife correction
IntraCorr=intraBeta(d,n, JC=TRUE)

\donttest{
#Bootstrap correction with confidence intervals
IntraCorr=intraBeta(d,n, B=1000, CI_Boot=0.95 )

#Bootstrap correction with confidence intervals and plot
IntraCorr=intraBeta(d,n, B=1000, CI_Boot=0.95, plot=TRUE )

#Double Bootstrap correction with 10 repetitions in the inner loop and 50 in the outer loop
IntraCorr=intraBeta(D1,N1, DB=c(10,50))
}

}
\keyword{ Beta}
\keyword{ Beta}
