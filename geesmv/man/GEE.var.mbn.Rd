\name{GEE.var.mbn}
\alias{GEE.var.mbn}
\title{
A modified GEE Variance Estimator Proposed by Morel, Bokossa and Neerchal (2003).
}
\description{
Calculate the modified GEE variance estimator proposed by Morel, Bokossa and Neerchal (2003).
}
\usage{
GEE.var.mbn(formula,id,family=gaussian,data,corstr="independence",d=2,r=1)
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
 \item{d}{adjustment factor in the variance estimator. The default value is 2. Details see Morel, Bokossa and Neerchal (2003).
 }
 \item{r}{adjustment factor in the variance estimator. The default value is 1. Details see Morel, Bokossa and Neerchal (2003).
 }
}
\details{
Fit the model by GEE and calculate the variance estimator proposed by Morel, Bokossa and Neerchal (2003). }
\value{
\item{cov.beta }{estimate of the variance for beta}
  \item{cov.var }{estimate of the variance-covariance matrix for the variance estimator}
}
\references{
Morel JG, Bokossa MC, and Neerchal NK. Small sample correction for the variance of GEE estimators. Biometrical Journal 2003;45(4): 395-409. 
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
mbn.ind <- GEE.var.mbn(formula,id="subject",family=gaussian,
       data_alt,corstr="independence",d=2,r=1) ##Independence correlation structure;            
mbn.exch <- GEE.var.mbn(formula,id="subject",family=gaussian,
      data_alt,corstr="exchangeable",d=2,r=1) ##Exchangeable correlation structure;             
mbn.ar1 <- GEE.var.mbn(formula,id="subject",family=gaussian,
       data_alt,corstr="AR-M",d=2,r=1) ##AR-1 correlation structure;                   
mbn.unstr <- GEE.var.mbn(formula,id="subject",family=gaussian,
       data_alt,corstr="unstructured",d=2,r=1) ##Unstructured correlation structur;
}
\keyword{GEE}
\keyword{Morel}