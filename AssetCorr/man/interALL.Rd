\name{interALL}
\alias{interALL}

\title{Function to use multiple estimators simultaneously

}
\description{
To give a first insight of the default time series, this function combines multiple estimator functions and visualize the results.
}
\usage{
interALL(d1,n1,d2,n2,rho1,rho2, B=NA, DB=NA, JC=FALSE, CI_Boot=NA, plot=FALSE, 
type="bca",Estimator=c("Copula","Cov","JDP","MLE"),show_progress=FALSE)
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

  \item{CI_Boot}{a number, indicating the desired confidence interval if the single bootstrap correction is specified.
  By default, the interval is calculated as the bootstrap corrected and accelerated confidence interval (Bca). Furthermore, the analytical confidence intervals are provided, using the same value as \code{CI_Boot}.
}

 \item{plot}{a logical variable, indicating whether a plot of the default time series and the estimates should be generated using the \code{multiplot} function of \insertCite{teetor2011r;textual}{AssetCorr}.
}
  \item{type}{a string, indicating the desired method to calculate the bootstrap confidence intervals. For more details see \code{\link[boot]{boot.ci}}. Studendized confidence intervals are not supported.} 
  
 \item{Estimator}{a combined string, indicating which estimators should be used. All estimators are set as default.
}
 \item{show_progress}{a logical variable, indicating whether a progress bar should be displayed.
}
}




\details{
To give an first insight, the function provides an overview of the default time series and estimates using different estimators simultaneously. 
If \code{DB} is specified, the single bootstrap corrected estimate will be calculated by using the bootstrap values of the outer loop.
}

\value{

The returned value is a data frame, containing the following columns:

\item{Estimator}{Name of the applied estimator}
\item{Estimate}{Value of the calculated estimate}
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
\code{\link{intraAMM}}, \code{\link{intraFMM}}, \code{\link{intraJDP2}}
\code{\link{intraMLE}}, \code{\link{intraJDP1}}, \code{\link{intraCMM}}}
\examples{

\donttest{
set.seed(111)
Psi=rmvnorm(20,sigma=matrix(c(1,0.5,0.5,1),2))
PDcond1=pnorm((qnorm(0.01)-sqrt(0.05)*Psi[,1])/sqrt(1-0.05))
PDcond2=pnorm((qnorm(0.01)-sqrt(0.2)*Psi[,2])/sqrt(1-0.2))

D1=rbinom(20,1000,PDcond1)
D2=rbinom(20,1000,PDcond2)

N1=N2=rep(1000,20)

DTS=cbind(D1,D2)
N=cbind(N1,N2)

rho1=intraMLE(D1,N1)$Original
rho2=intraMLE(D2,N2)$Original

#Point Estimates
interALL(D1,N1,D2,N2,rho1,rho2, plot=TRUE)

#Bootstrap corrected estimates of all available estimators:
InterCorr=interALL(D1,N1,D2,N2,rho1,rho2, B=500, CI_Boot=0.95 , plot=TRUE, show_progress=TRUE)


#Jackknife correction
InterCorr=interALL(D1,N1,D2,N2,rho1,rho2, JC=TRUE, plot=TRUE)

#Double Bootstrap correction with 10 repetitions in the inner loop and 50 in the outer loop
InterCorr=interALL(D1,N1,D2,N2,rho1,rho2, DB=c(10,50), plot=TRUE)

}


}
\keyword{ALL}
\keyword{ALL}
