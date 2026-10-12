\name{GEE.var.gst}
\alias{GEE.var.gst}
\title{
A modified GEE Variance Estimator Proposed by Gosho et al.(2014)
}
\description{
Calculate the modified GEE variance estimator proposed by Gosho et al. (2014). 
}
\usage{
GEE.var.gst(formula,id,family=gaussian,data,corstr="independence")
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
an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula).
}
  \item{corstr}{
a character string specifying working correlation structure: "independence", "AR-M","exchangeable", "unstructured"  are possible.
}
}
\details{
Fit the model by GEE and provide the variance estimator based on Gosho et al.(2014). Gosho et al (2014) proposed to made an additional modification on Pan's estimator. 
}
\value{
\item{cov.beta }{estimate of the variance for beta}
  \item{cov.var }{estimate of the variance-covariance matrix for the variance estimator}
}
\references{
Gosho, M., Sato, Y. and Takeuchi, H. Robust covariance estimator for small-sample adjustment in the generalized estimating equations: A simulation study. Science Journal of Applied Mathematics and Statistics 2014;2(1):20-25.
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
gst.ind <- GEE.var.gst(formula,id="subject",family=gaussian,
        data_alt,corstr="independence") ##Independence correlation structure;              
gst.exch <- GEE.var.gst(formula,id="subject",family=gaussian,
        data_alt,corstr="exchangeable") ##Exchangeable correlation structure;     
gst.ar1 <- GEE.var.gst(formula,id="subject",family=gaussian,
        data_alt,corstr="AR-M") ##AR-1 correlation structure;          
gst.unstr <- GEE.var.gst(formula,id="subject",family=gaussian,
         data_alt,corstr="unstructured") ##Unstructured correlation structure;
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Gosho}
\keyword{GEE}% __ONLY ONE__ keyword per line
