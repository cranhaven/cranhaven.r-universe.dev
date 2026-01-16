\name{summary.bmem}
\alias{summary.bmem}
\alias{summary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sumarize the results of function 'bmem'}
\description{Sumarize the results of function 'bmem'}
\usage{
\method{summary}{bmem}(object, estimates=TRUE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An output object from the function \code{\link{bmem}}}
  \item{estimates}{Whether output a more detailed results of parameters and values of statistics}
  \item{...}{other options can be used for the generic summary function.}
}
\details{
The other type of confidence intervals can be constructed from the output of the function \code{\link{bmem}}. Note if the BCa is required, the \code{ci='BCa'} should have been specified in the function \code{\link{bmem}}.
}
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
ind := a*b'

fit_l <- bmem(data=PoliticalDemocracy, model = model_l, method='list',
    ci='perc', boot=30, parallel = TRUE, ncore = 8)
summary.bmem(fit_l)
}