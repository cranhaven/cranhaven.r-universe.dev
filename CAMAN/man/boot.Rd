\name{CAMANboot}
\alias{CAMANboot}
\title{ Parametric bootstrap }
\description{Parametric bootstrap for bivariate normally distributed data}
\usage{
CAMANboot(obs1, obs2, var1, var2, lambda11, lambda12, 
     prob1, lambda21, lambda22, prob2, rep, 
     data,numiter=10000,acc=1.e-7)}
\arguments{
  \item{obs1}{the first column of the observations}\cr
  \item{obs2}{the second column of the observations}\cr 
  \item{data}{a data frame}\cr 
  \item{var1}{Variance of the first column of the observations(except meta-analysis)} \cr 
  \item{var2}{Variance of the second column of the observations (except meta-analysis)} \cr 
  \item{lambda11}{first means of the first column of the observations}\cr 
  \item{lambda12}{first means of the second column of the observations}\cr 
  \item{prob1}{first mixing weight}\cr 
  \item{lambda21}{second means of the first column of the
observations}\cr 
  \item{lambda22}{second means of the second column of the observations}\cr 
  \item{prob2}{second mixing weight}\cr 
\item{numiter}{parameter to control the maximal number of iterations in the VEM and EM loops. Default is 5000.} \cr
\item{acc}{convergence criterion. Default is 1.e-7} \cr
\item{rep}{number of repetitions}\cr
}
\examples{
# Parametric bootstrap for bivariate normally distributed data
data(CT)
library(mvtnorm)
hom1<-c(3.142442)
hom2<-c(-1.842393)
p1<-c(1)
start1<-c(2.961984,3.226141)
start2<-c(-2.578836, -1.500823)
pvem<-c(0.317,0.683)
CAMANboot(obs1=logitTPR, obs2=logitTNR, var1=varlogitTPR, var2=varlogitTNR,
          lambda11=hom1, lambda12=hom2, prob1=p1, 
          lambda21=start1, lambda22=start2, prob2=pvem,rep=3,data=CT)
}
\concept{parametric bootstrap}
