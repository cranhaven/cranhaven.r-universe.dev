\name{surv_data_simulation}
\alias{surv_data_simulation}
\title{Simulate Survival Data}
\usage{
surv_data_simulation(lambda,gamma,x,betas=NULL, dropoutrate=0,
gammac=1,censordist='exponential',timeinterval=NULL,trt_timeinterval=NULL)

}

\arguments{

  \item{lambda}{lambda for event hazard function}
  \item{gamma}{gamma for event hazard function}
  \item{x}{design matrix}
  \item{betas}{coefficients for the covariates. The length of betas should be the same of number of columns of x}
   \item{dropoutrate}{Patient dropout rate with range [0,1).  If dropoutrate contains only one number. The program will control the dropout rate at population level(treatment + control).  If dropoutrate contains two numbers (ie. c(0.2,0.1)), the program will control the dropout rate of control and treatment arm seperately, with the first dropout rate number for control and the second number for treatment. Default value is "0" (no dropout)}
 
  \item{gammac}{gamma for censor hazard function. Default is 1 (exponential)}
  \item{censordist}{censor hazard distribution. Default is exponential}
  \item{timeinterval}{time intervals if the baseline hazard function is piecewise.}
  \item{trt_timeinterval}{Time windows for piecewise hazard ratios}
}




\description{
Simulate survival data given design matrix and covariates (betas).
}

\examples{
N<-400
x<-data.frame(arm=rbinom(N,1,0.5), factor1=rbinom(N,1,0.7), factor2=rbinom(N,1,0.8))
betas<-c(arm=-0.35667,factor1=0.3,factor2=-0.1)

data<-surv_data_simulation(lambda=0.2,gamma=2,x=x,betas=betas,dropoutrate=0.2)




}
