\name{interCopula}
\alias{interCopula}

\title{
Copula Based Maximum Likelihood Estimator
}
\description{
The inter correlation parameter can be estimated by fitting a Gaussian copula with Vasicek distributed margins on two observed default rate time series. The estimated parameter is the inter correlation of the systematic factors.
}
\usage{
interCopula(df1, df2, B = 0, DB=c(0,0),JC = FALSE, 
 CI, CI_Boot, type="bca", plot=FALSE)
}

\arguments{
  \item{df1}{a vector, containing the default rate time series of sector 1.

}
  \item{df2}{a vector, containing the default rate times eries of sector 2.

}
   \item{B}{an integer, indicating how many bootstrap repetitions should be used for the single bootstrap corrected estimate.
}
\item{DB}{a combined vector, indicating how many bootstrap repetitions should be used for the inner (first entry) and outer loop (second entry) to correct the bias using the double bootstrap.
} 
  \item{JC}{ a logical variable, indicating if the jackknife corrected estimate should be calculated.

}
  

  \item{CI}{a number, indicating the desired asymptotic confidence interval of the estimate. 

}
  \item{CI_Boot}{a number, indicating the desired bootstrap confidence interval if the single bootstrap correction is specified. By default, the interval is calculated as the bootstrap corrected and accelerated confidence interval (Bca). Furthermore, the analytical confidence intervals are provided, using the same value as \code{CI_Boot}.
} 
  \item{type}{a string, indicating the desired method to calculate the confidence intervals. For more details see \code{\link[boot]{boot.ci}}. 
}
 \item{plot}{a logical variable, indicating whether a plot of the single bootstrap density should be generated.
}
}
\details{
This function estimates the inter correlation of the systematic factors. In general the inter correlation can be estimated for the asset variables or the systematic factors. This method estimates the inter correlation of the systematic factors. The parameter will be estimated via Maximum Likelihood estimation, which ensures that an asymptotic confidence interval can be provided. The inter correlation of the systematic factors can be transformed to the correlation of the asset variables as follows:

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

\insertRef{Pfeuffer2018}{AssetCorr}


}

\seealso{
\code{\link{interJDP}}, \code{\link{interCMM}}, \code{\link{interMLE}}, \code{\link{interCov}}
}
\examples{
set.seed(10)
d1=defaultTimeseries(1000,0.1,10,0.01)
d2=defaultTimeseries(1000,0.2,10,0.01)
n=rep(1000,10)

df1=d1/n
df2=d2/n

InterCorr=interCopula(df1,df2, CI=0.95)


\donttest{
InterCorr=interCopula(df1,df2, JC=TRUE)

InterCorr=interCopula(df1,df2, B=1000, CI_Boot=0.95, plot=TRUE)

InterCorr=interCopula(df1,df2, DB=c(50,50))

}
}
\keyword{Copula}

