\name{GEE.var.wl}
\alias{GEE.var.wl}
\title{
A Modified GEE Variance Estimator Proposed by Wang and Long (2011). 
}
\description{
Calculate the modified GEE variance estimator proposed by Wang and Long (2011).  
}
\usage{
GEE.var.wl(formula,id,family=gaussian,data,corstr="independence")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{formula}{
an object of class "formula" (or one that can be coerced to that class): a formula expression as for other regression models to be fitted, of the form response ~ predictors. The details of formula specification can be seen in glm() and gee().
     }
  \item{id}{
a vector which identifies the clusters. The length of id should be the same as the total number of observations. Data is assumed to be sorted so that observations on a cluster are contiguous rows for all entities in the formula.
  }
  \item{family}{
a family object: a list of functions and expressions for defining link and variance functions to be used in the model. This can be a character string naming a family function including "gaussian", "binomial", and "poisson". See family for details of family functions in glm() and gee().
}
  \item{data}{
an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which glm is called.
}
  \item{corstr}{
a character string specifying working correlation structure: "independence", "AR-M","exchangeable", "unstructured"  are possible.
}
}
\details{
Fit the model by GEE and calculate the variance estimator proposed by Wang and Long (2011). Two assumptions need to be satisfied: 1) The conditional variance is correctly specified; 2) A common correlation structure exists across all subjects. 
}
\value{
  \item{cov.beta }{estimate of the variance for beta}
  \item{cov.var }{estimate of the variance-covariance matrix for the variance estimator}
}
\references{
Wang M and Long Q. Modified robust variance estimator for generalized estimating equations with improved small-sample performance. Statistics in Medicine 2011;30(11): 1278-1291. 
}
\author{
Ming Wang <mwang@phs.psu.edu>

Maintainer: Zheng Li <zheng.li@outlook.com>
}
\examples{
data(dental)
data_alt <- reshape(dental, direction="long", timevar="Time", 
                   varying=names(dental)[3:6], v.names="response", times=c(8,10,12,14))
data_alt <- data_alt[order(data_alt$subject),]
data_alt$gender <- as.numeric(data_alt$gender)
data_alt$Time <- sqrt(data_alt$Time)
formula <- response~Time+gender
wl.ind <- GEE.var.wl(formula,id="subject",family=gaussian,
       data_alt,corstr="independence") ##Independence correlation structure;            
wl.exch <- GEE.var.wl(formula,id="subject",family=gaussian,
       data_alt,corstr="exchangeable") ##Exchangeable correlation structure;               
wl.ar1 <- GEE.var.wl(formula,id="subject",family=gaussian,
       data_alt,corstr="AR-M") ##AR-1 correlation structure;            
wl.unstr <- GEE.var.wl(formula,id="subject",family=gaussian,
         data_alt,corstr="unstructured") ##Unstructured correlation structure;
             
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{efficiency}
\keyword{Gee}% __ONLY ONE__ keyword per line
