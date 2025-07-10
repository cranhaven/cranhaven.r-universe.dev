
\name{VIRF}
\alias{VIRF}
\title{Volatility Impulse Response Function }
\description{
Provide information about the impact of independent shocks on volatility.
}
\usage{
VIRF(data, shock)
}
\arguments{
  \item{data}{log return multivariate time series}
  \item{shock}{shock time point from time series}
    }
\value{
    \item{virfresult}{List containing variance and covariance values}
  }
\references{
Anthony, N.R. and Ahammad, S.M. 2016. Investigating the interdependency of
agricultural production volatility spillovers between Bangladesh, India, and Pakistan. Review of Urban and Regional Development Studies, 28, 32 to 54
Jin, X., Lin, S.X. and Tamvakis, M. 2012. Volatility transmission and volatility impulse response functions in crude oil markets.Energy Economics, 34, 2125 to 2134
}
\examples{


k=3 #number of series
p=6 # maximum lag order
ns=100 #number of simulations
B=matrix(0,nrow=k,ncol=p*k)
A1<- matrix(c(.4,-.02,.01,-.02,.3,.02,.01,.04,.3),ncol=3,nrow=3)
A2 <- matrix(c(.2,0,0,0,.3,0,0,0,.13),ncol=3,nrow=3)
B[,1:k]=A1
B[,(4*k+1):(5*k)]=A2
A <- BigVAR::VarptoVar1MC(B,p,k)
Y <-BigVAR::MultVarSim(k,A,p,.1*diag(k),ns)
lr<-VIRF(Y,40) # Y: multivariate time series data, shock time point: 40
print(lr)

}
