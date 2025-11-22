\name{analyze_AssetCorr}
\alias{analyze_AssetCorr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to evaluate several default time series simultaneously

}
\description{
To give a first insight of several default time series, this function combines multiple estimator functions (intra and inter) and visualize the results.
}
\usage{
analyze_AssetCorr(DTS,N, B=NA, DB=NA, JC=FALSE, CI_Boot=NA, Adjust=0.0001,
type="bca", Intra=c("AMM","FMM","CMM","JDP1","JDP2","MLE","AMLE","Beta","Mode"),
Inter=c("Copula","Cov","JDP","MLE"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DTS}{a matrix, containing the default time series of each sector.
%%     ~~Describe \code{d} here~~
}
  \item{N}{a matrix, containing the number of obligors at the beginning of the period of sector.
%%     ~~Describe \code{n} here~~
}
 \item{B}{an integer, indicating how many bootstrap repetitions should be used for the single bootstrap corrected estimate (intra and inter).
}

 \item{DB}{a combined vector, indicating how many bootstrap repetitions should be used for the inner (first entry) and outer loop (second entry) to correct the bias using the double bootstrap (intra and inter).
} 
  \item{JC}{ a logical variable, indicating if the jackknife corrected estimate should be calculated (intra and inter).
%%     ~~Describe \code{JC} here~~
}

  \item{CI_Boot}{a number, indicating the desired confidence interval if the single bootstrap correction is specified.
  By default, the interval is calculated as the bootstrap corrected and accelerated confidence interval (Bca). Furthermore, the analytical confidence intervals are provided, using the same value as \code{CI_Boot}(intra and inter).
}
 
\item{Adjust}{a number, which should be added to a observed default rate of 0 or subtracted form a observed default rate of 1 (only for the intraAMLE).}


  \item{type}{a string, indicating the desired method to calculate the bootstrap confidence intervals. For more details see \code{\link[boot]{boot.ci}}. Studendized confidence intervals are not supported.} 
  
 \item{Intra}{a combined string, indicating which intra correlation estimators should be used. All estimators are set as default.
}
 \item{Inter}{a combined string, indicating which inter correlation estimators should be used. All estimators are set as default.
}

}




\details{
To give an first insight, the function provides an overview of the several default time series and estimates using different estimators (intra and inter) simultaneously. The plug-in estimates of the intra correlation using inter correlation methods will be estimated via \code{intraMLE}.
If \code{DB} is specified, the single bootstrap corrected estimate will be calculated by using the bootstrap values of the outer loop.
}

\value{



The returned value is a list, containing the following entries:

Estimators_Intra

\item{Sector}{Number of the sector}
\item{Sector_Name}{Name of the sector}
\item{Estimator}{Name of the applied estimator}
\item{Estimate}{Value of the calculated estimate}
\item{Type}{String, which indicating corrected/non-corrected estimates}
\item{correction}{Name of the correction method}
\item{B}{Number of single bootstrap repetitions}
\item{DB}{Number of the double bootstrap repetitions}
\item{CI_Boot}{Selected two-sided bootstrap confidence interval}
\item{CI}{A string, indicating if the corresponding value is the upper or lower bound}


Estimators_Inter

\item{Sector_1}{Number of the sector}
\item{Sector_Name_1}{Name of the sector}
\item{Sector_2}{Number of the sector}
\item{Sector_Name_2}{Name of the sector}
\item{Estimator}{Name of the applied estimator to Sector_1 and Sector_2}
\item{Estimate}{Value of the calculated estimateto Sector_1 and Sector_2}
\item{Type}{String, which indicating corrected/non-corrected estimates}
\item{correction}{Name of the correction method}
\item{B}{Number of single bootstrap repetitions}
\item{DB}{Number of the double bootstrap repetitions}
\item{CI_Boot}{Selected two-sided bootstrap confidence interval}
\item{CI}{A string, indicating if the corresponding value is the upper or lower bound}





}

\author{
Kevin Jakob
}

\references{
\insertRef{botha2010implied}{AssetCorr} 

\insertRef{chang2015double}{AssetCorr}

\insertRef{de2002default}{AssetCorr}

\insertRef{dullmann2004systematic}{AssetCorr}

\insertRef{efron1994introduction}{AssetCorr}

\insertRef{frei2017moment}{AssetCorr}

\insertRef{gordy2000comparative}{AssetCorr}

\insertRef{gordy2010small}{AssetCorr}

\insertRef{kalkbrener2010validating}{AssetCorr}

\insertRef{lucas1995default}{AssetCorr}

\insertRef{meyer2009estimation}{AssetCorr}

\insertRef{teetor2011r}{AssetCorr}

}



\seealso{
\code{\link{intraAMM}}, \code{\link{intraFMM}}, \code{\link{intraJDP2}},
\code{\link{intraMLE}}, \code{\link{intraJDP1}}, \code{\link{intraCMM}},
\code{\link{intraMode}},\code{\link{intraBeta}},
\code{\link{interJDP}}, \code{\link{interCopula}},\code{\link{interCMM}}, 
\code{\link{interCov}}, \code{\link{interMLE}}, \code{\link{intraALL}}, 
\code{\link{interALL}}
}
\examples{

\donttest{
library(mvtnorm)
set.seed(111)
NoO=1000 #Number of obligors in each sector
Years=20
AC=0.3
PD=0.01

Psi=rmvnorm(Years,sigma=matrix(c(1,0.5,0.5,0.5,1,0.5,0.5,0.5,1),3))
PDcond1=pnorm((qnorm(PD)-sqrt(AC)*Psi[,1])/sqrt(1-AC))
PDcond2=pnorm((qnorm(PD)-sqrt(AC/2)*Psi[,2])/sqrt(1-AC/2))
PDcond3=pnorm((qnorm(PD)-sqrt(AC*2)*Psi[,3])/sqrt(1-AC*2))

DTS=cbind(rbinom(Years,NoO,PDcond1),rbinom(Years,NoO,PDcond2),rbinom(Years,NoO,PDcond3))
N=matrix(NoO,nrow = Years,ncol = 3)

Output<-analyze_AssetCorr(DTS,N)

#Bootstrap Correction and CIs

Output<-analyze_AssetCorr(DTS,N,B=100,CI_Boot=0.95)

#Double Bootstrap Correction and Jackknife
Output<-analyze_AssetCorr(DTS,N,DB=c(50,50),JC=TRUE)



}


}
\keyword{ALL}
\keyword{ALL}% __ONLY ONE__ keyword per line
