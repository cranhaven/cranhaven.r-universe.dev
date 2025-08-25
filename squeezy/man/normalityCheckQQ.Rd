\name{normalityCheckQQ}
\alias{normalityCheckQQ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Visual posterior check of multivariate normality of the linear predictors
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Produce a qq-plot to visually check whether the assumption of multivariate normality of the linear predictors is valid for the data and model fit with 'squeezy'.
}
\usage{
normalityCheckQQ(X,groupset,fit.squeezy,nSim=500)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
Observed data; (nxp)-dimensional matrix (p: number of covariates) with each row the observed high-dimensional feature vector of a sample.
}
  \item{groupset}{
%%     ~~Describe \code{groupset} here~~
Co-data group set; list with G groups.  Each group is a vector containing the indices of the covariates in that group.
}
  \item{fit.squeezy}{
%%     ~~Describe \code{fit.ecpc} here~~
Model fit obtained by the function \link{squeezy}.
}
  \item{nSim}{
%%     ~~Describe \code{fit.ecpc} here~~
Number of simulated vectors of linear predictors.
}

}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
The qqplot of the empirical versus theoretical quantiles is plotted. If `ggplot2' is installed, the plot is returned as `ggplot' object.
}

\examples{
#Simulate toy data
n<-100
p<-300
X <- matrix(rnorm(n*p),n,p)
Y <- rnorm(n)
groupset <- list(1:(p/2),(p/2+1):p)
sigmahat <- 2
alpha <- 0.5
tauMR <- c(0.01,0.005)

#Fit group-regularised elastic net model with squeezy
fit.squeezy <- squeezy(Y,X,groupset,alpha=alpha,
                       lambdas=sigmahat/tauMR,sigmasq=sigmahat,
                       lambdaglobal=mean(sigmahat/tauMR))
                       
#Check qq-plot
normalityCheckQQ(X,groupset,fit.squeezy)
}