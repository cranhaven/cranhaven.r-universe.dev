\name{weibullsim}
\alias{weibullsim}
\title{Simulate Event Time with weibull Hazard Function}
\usage{
weibullsim(N=NULL,lambda,gamma,x=NULL,betas=NULL)


}

\arguments{
  \item{N}{Total number of patients }


  \item{lambda}{lambda for event hazard function}
  \item{gamma}{gamma for event hazard function}
  \item{x}{design matrix}
  \item{betas}{covariates for design matrix}
}


\description{
Simulate event time with weibull hazard function
}
\examples{

data<-weibullsim(N=300,lambda=0.03,gamma=0.9)


}
