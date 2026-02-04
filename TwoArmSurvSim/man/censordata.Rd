\name{censordata}
\alias{censordata}
\title{Censor Events Given a Fixed Dropout Rate}
\usage{
censordata(simdata,lambda,gamma, dropoutrate,ebx=1,gammac=1,groupfreq=1,
censordist='exponential',timeinterval=NULL,HRPW=FALSE)
  

}

\arguments{
  \item{simdata}{Simulated event dataset}


  \item{lambda}{lambda for event hazard function}
  \item{gamma}{gamma for event hazard function}
  \item{dropoutrate}{Patient dropout rate with range [0,1).  If dropoutrate contains only one number. The program will control the dropout rate at population level(treatment + control).  If dropoutrate contains two numbers (ie. c(0.2,0.1)), the program will control the dropout rate of control and treatment arm seperately, with the first dropout rate number for control and the second number for treatment. Default value is "0" (no dropout)}
   \item{ebx}{exp(beta*x), if there's no covariates, ebx=1.}
  \item{groupfreq}{frequence of each level of ebx value}

  \item{timeinterval}{time intervals for piecewise baseline hazard function}
  \item{gammac}{gamma for censor hazard function. Default is 1 (exponential)}
  \item{censordist}{censor hazard distribution. Default is exponential}
  \item{HRPW}{Indicator of piecewise hazard ratios. TURE for piecewise. FALSE for non piecewise }
}




\description{
Censor events given a fixed dropout rate
}

