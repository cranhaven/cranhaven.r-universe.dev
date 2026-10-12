\name{GEE.var.md}
\alias{GEE.var.md}
\title{
A Modified GEE Variance Estimator Proposed by Mancl and DeRouen (2001).
}
\description{
Calculate the modified GEE variance estimator proposed by Mancl and DeRouen (2001).
}
\usage{
GEE.var.md(formula,id,family=gaussian,data,corstr="independence")
}
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
Fit the model by GEE and calculate the variance estimator proposed by Mancl and DeRouen (2001). 
}
\value{
 \item{cov.beta }{estimate of the variance for beta}
  \item{cov.var }{estimate of the variance-covariance matrix for the variance estimator}
}
\references{
Mancl LA and DeRouen TA. A Covariance Estimator for GEE with Improved Small- Sample Properties. Biometrics 2001;57: 126-134. 
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
md.ind <- GEE.var.md(formula,id="subject",family=gaussian,
          data_alt,corstr="independence") ##Independence correlation structure;                    
md.exch <- GEE.var.md(formula,id="subject",family=gaussian,
         data_alt,corstr="exchangeable") ##Exchangeable correlation structure;                 
md.ar1 <- GEE.var.md(formula,id="subject",family=gaussian,
          data_alt,corstr="AR-M") ##AR-1 correlation structure;                     
md.unstr <- GEE.var.md(formula,id="subject",family=gaussian,
         data_alt,corstr="unstructured") ##Unstructured correlation structure;
}
\keyword{Mancl}
\keyword{GEE}