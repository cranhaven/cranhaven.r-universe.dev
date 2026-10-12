\name{dynpanel-package}
\alias{dynpanel-package}
\alias{dynpanel}
\docType{package}
\title{
Dynamic Panel Data Models
}
\description{
This package computes the first stage GMM estimate of a dynamic linear model with p lags of the dependent variables.
}
\details{
\tabular{ll}{
Package: \tab dynpanel\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2016-08-26\cr
License: \tab GPL-3\cr
}
In this package, we apply the generalized method of moments to estimate the dynamic panel data models.

}
\author{
Taha Zaghdoudi

Taha Zaghdoudi <zedtaha@gmail.com>
}
\references{

Anderson, T. W.; Hsiao, Cheng (1981). Estimation of dynamic models with error components. \emph{ournal of the American Statistical Association}. \bold{76} (375) ,pp. 598-606.

  Arellano, Manuel; Bond, Stephen (1991). Some tests of specification for panel data: Monte Carlo evidence and an application to employment equations. \emph{Review of Economic Studies}. \bold{58}, pp.2)-277.
Cameron, A. Colin; Trivedi, Pravin K. (2005). Dynamic Models. \emph{Microeconometrics: Methods and Applications}. New York: Cambridge University Press. pp. 763-768.

Hsiao, Cheng (2014). Dynamic Simultaneous Equations Models. Analysis of Panel Data. \emph{New York: Cambridge University Press}. pp. 397-402.

Munnell AH (1990). Why has Productivity Growth Declined? Productivity and Public Investment, \emph{New England Economic Review}, pp. 3-22.


}
\examples{
 # Load data
 data(Produc)
 # Fit the dynamic panel data using the Arellano Bond (1991) instruments
 reg<-dpd(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,Produc,index=c("state","year"),1,4)
 summary(reg)
 # Fit the dynamic panel data using an automatic selection of appropriate IV matrix
 #reg<-dpd(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,Produc,index=c("state","year"),1,0)
 #summary(reg)
 # Fit the dynamic panel data using the GMM estimator with the smallest set of instruments
 #reg<-dpd(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,Produc,index=c("state","year"),1,1)
 #summary(reg)
 # Fit the dynamic panel data using a reduced form of IV from method 3
 #reg<-dpd(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,Produc,index=c("state","year"),1,2)
 #summary(reg)
 # Fit the dynamic panel data using the IV matrix where the number of moments grows with kT
 # K: variables number and T: time per group
 #reg<-dpd(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,Produc,index=c("state","year"),1,3)
 #summary(reg)
}
