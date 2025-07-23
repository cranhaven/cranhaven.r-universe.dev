\name{simDat}
\alias{simDat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Simulate data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Simulate toy data with linear or logistic response.
}
\usage{
simDat(n, p, n2 = 20, muGrp, varGrp, indT, sigma = 1, 
  model = c("linear","logistic"), flag = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
%%     ~~Describe \code{n} here~~
Number of samples for the training set.
}
  \item{p}{
%%     ~~Describe \code{p} here~~
Number of covariates.
}
  \item{n2}{
%%     ~~Describe \code{n2} here~~
Number of independent samples for the test set.
}
  \item{muGrp}{
%%     ~~Describe \code{muGrp} here~~
Prior mean for different groups.
}
  \item{varGrp}{
%%     ~~Describe \code{varGrp} here~~
Prior variance for different groups.
}
  \item{indT}{
%%     ~~Describe \code{indT} here~~
True group index of each covariate; p-dimensional vector.
}
  \item{sigma}{
%%     ~~Describe \code{sigma} here~~
Variance parameter for linear model.
}
  \item{model}{
%%     ~~Describe \code{model} here~~
Type of model.
}
  \item{flag}{
%%     ~~Describe \code{flag} here~~
Should linear predictors and true response be plotted?
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A list with
\item{beta}{Simulated regression coefficients}
\item{Xctd}{Simulated observed data for training set}
\item{Y}{Simulated response data for test set}
\item{X2ctd}{Simulated observed data for test set}
\item{Y2}{Simulated response data for test set}
}

\examples{
n<-10
p<-30
#simulate beta from two normal distributions; beta_k ~ N(mu_k,tau^2_k)
muGrp <- c(0,0.1) #mean (mu_1,mu_2)
varGrp <- c(0.05,0.01) #variance (tau^2_1,tau^2_2)
#group number of each covariate; first half in group 1, second half in group 2
indT <- rep(c(1,2),each=15)

dataLin <- simDat(n, p, n2 = 20, muGrp, varGrp, indT, sigma = 1, model = "linear",
    flag = TRUE)
dataLog <- simDat(n, p, n2 = 20, muGrp, varGrp, indT, model = "logistic",
    flag = TRUE)

}
