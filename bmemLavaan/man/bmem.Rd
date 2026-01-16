\name{bmem}
\alias{bmem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Mediation analysis based on bootstrap}
\description{Mediation analysis based on bootstrap}
\usage{
bmem(data, model, v, method='list', ci='perc', cl=.95, 
     boot=1000, m=10, varphi=.1, st='i', robust=FALSE, 
     max_it=500, parallel=FALSE, ncore=1,  ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A data set}
  \item{model}{RAM path for the mediaiton model}
  \item{v}{Indices of variables used in the mediation model. If omitted, all variables are used.}
  \item{method}{\code{list}: listwise deletion, \code{pair}: pairwise deletion, \code{mi}: multiple imputation, \code{em}: EM algorithm.}
  \item{ci}{\code{norm}: normal approximation CI, \code{perc}: percentile CI, \code{bc}: bias-corrected CI, \code{bca}: BCa}
  \item{cl}{Confidence level. Can be a vector.}
  \item{boot}{Number of bootstraps}
  \item{m}{Number of imputations}
  \item{varphi}{Percent of data to be downweighted in robust method}
  \item{st}{Starting values}
  \item{robust}{Whether to use roubst method}
  \item{max_it}{Maximum number of iterations in EM}
  \item{parallel}{Whether to use parallel method to calculate.}
  \item{ncore}{Number of cores for parallel method.}
  \item{\dots}{Other options for \code{\link[lavaan]{sem}} function can be used.}
}
\details{
The indirect effect can be specified using equations such as \code{a*b}, \code{a*b+c}, and \code{a*b*c+d*e+f}, which can be defined in 'model' parameter. }

\value{
The on-screen output includes the parameter estimates, bootstrap standard errors, and CIs.
}
\examples{
data("PoliticalDemocracy")

model_l <- '
ind60 =~ x1 + g*x2 + h*x3
dem60 =~ y1 + d*y2 + e*y3 + f*y4
dem65 =~ y5 + d*y6 + e*y7 + f*y8

dem60 ~ a * ind60
dem65 ~ c * ind60 + b * dem60

y1 ~~ y5
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8
ind := a*b
'

fit_l <- bmem(data=PoliticalDemocracy, model = model_l, method='list', 
      ci='perc', boot=50, parallel = TRUE, ncore = 8)
summary(fit_l)
}
\author{
Zhiyong Zhang, Shuigen Ming and Lijuan Wang
}
\references{
Zhang, Z., & Wang, L. (2013). Methods for mediation analysis with missing data. Psychometrika, 78(1), 154-184.
\doi{https://doi.org/10.1007/s11336-012-9301-5}

Yuan, KH., Zhang, Z. Robust Structural Equation Modeling with Missing Data and Auxiliary Variables. Psychometrika 77, 803-826 (2012).
\doi{https://doi.org/10.1007/s11336-012-9282-4}
}
