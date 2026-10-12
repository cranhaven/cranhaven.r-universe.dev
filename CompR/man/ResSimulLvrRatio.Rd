\name{ResSimulLvrRatio}
\alias{ResSimulLvrRatio}
\title{
Log Likelihood Ratio Test for Paired comparison data
}
\description{
Returns the result of Log Likelihood Ratio Test of the number of classes for Paired comparison data (T classes versus (T+1) classes)
}
\usage{
ResSimulLvrRatio(Data,ResH0,Constraint,nsimul,level,eps=1e-04,eps1=1e-04)
}
\arguments{
  \item{Data}{
Object of class \code{DataPairComp}}
  \item{ResH0}{
Object of class \code{BradleyEstim} corresponding to the result of \code{BradleyEstim()} function for T classes (H0)
}
  \item{Constraint}{
Kind of constraint on Bradley's scores. If \code{Constraint=0}, the sum of Bradley's scores should be equal to 1. For other values for \code{Constraint}, the product of Bradley's scores should be equal to 1 (default \code{Constraint=0}).
}
  \item{nsimul}{
number of Monte Carlo simulations}
  \item{level}{
level of the Log Likelihood Ratio test defined by the user (default \code{level=0.05}).}
  \item{eps}{
    value of the convergence criteria for the EM algorithm (default \code{eps=1e-04}).
}
  \item{eps1}{
    value of the criteria convergence for Dykstra algorithm (default \code{eps1=1e-04}).
}
}
\details{
The likelihood ratio test is based on a Monte Carlo procedure. A simulation of \code{nsimul} data set is done. We perform estimation of the different parameters for the number of classes defined in the object \code{ResH0} of class \code{BradleyEstim} (corresponding to
the null hymothesis) and for one more class corresponding to the alternative hypothesis.

We obtain a set of Log Likelihoods under the null and alternative hypothesis on the basis of simulated data and so of the Log Likelihood Ratio Statistic. 

We replace the observed value of this statistic for the true data set. And we conclude on the acceptation or not of the null hypothesis (no differences between T and T+1 classes).}
\value{
Object of class \code{LvrRatio} with the following components:
\item{Simu}{Matrix with the number of classes under H0, Loglikelihoods under H0 and H1, difference between these Loglikelihoods.} 
 
\item{Test}{Matrix with the level of the test and the associated quantile}
}
\examples{
data(Cocktail)
ResCock1<-EstimBradley(Cocktail,Constraint=0,Tcla=1,eps=1e-04,eps1=1e-04,TestPi=TRUE)
Res_LvrRatio1<-ResSimulLvrRatio(Cocktail,ResCock1,0,3,level=0.05,eps=0.001,eps1=0.001)
getSimu(Res_LvrRatio1)
getTest(Res_LvrRatio1)
}