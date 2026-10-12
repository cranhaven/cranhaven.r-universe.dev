\name{GEE.var.lz}
\alias{GEE.var.lz}
\title{
GEE Sandwich Variance Estimator Proposed by Liang and Zeger (1986).
}
\description{
Calculate the GEE original variance estimator proposed by Liang and Zeger (1986).
}
\usage{
GEE.var.lz(formula,id="subject",family=gaussian,data,corstr="independence")
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
a character string specifying working correlation structure: "independence", "AR-M","exchangeable", "unstructured"  are possible.}
}
\details{
Fit the model by GEE and calculate the original variance estimator proposed by Liang and Zeger (1986).
}
\value{
   \item{cov.beta }{estimate of the variance for beta}
  \item{cov.var }{estimate of the variance-covariance matrix for the variance estimator}
}
\references{
Zeger SL and Liang KY. Longitudinal data analysis for discrete and continuous outcomes. Biometrics 1986: 121-130.}
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
lz.ind <- GEE.var.lz(formula,id="subject",family=gaussian,
          data_alt,corstr="independence") ##Independence correlation structure;                   
lz.exch <- GEE.var.lz(formula,id="subject",family=gaussian,
         data_alt,corstr="exchangeable") ##Exchangeable correlation structure;                    
lz.ar1 <- GEE.var.lz(formula,id="subject",family=gaussian,
          data_alt,corstr="AR-M") ##AR-1 correlation structure;                     
lz.unstr <- GEE.var.lz(formula,id="subject",family=gaussian,
        data_alt,corstr="unstructured") ##Unstructured correlation structure;
}
\keyword{ Liang}
\keyword{GEE}% __ONLY ONE__ keyword per line
