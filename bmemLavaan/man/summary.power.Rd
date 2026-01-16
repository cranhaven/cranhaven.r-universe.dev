\name{summary.power}
\alias{summary.power}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Organize the results into a table
}
\description{
This function is adapted from the \code{\link[lavaan]{lavaan}} summary function to put the results in a table. 
}
\usage{
\method{summary}{power}(object,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Output from the function either \code{\link{power.bmem}}.
}
\item{...}{Other options}
}
\value{
The on-screen output includes the basic information of this power analysis, parameters' true values, parameter estimates, average bootstrap standard error, standard deviation of the parameter estimates, powers, standard error of the estimated powers and empirical coverage probability of the constructed CIs.
}
\examples{
\donttest{
ex1model<-'
math ~ c*ME + start(0)*ME + b*HE + start(0.39)*HE
HE ~ a*ME + start(0.39)*ME
ab := a*b
'

N <- 50

system.time(power_robust <- power.bmem(ex1model, method = "robust", nobs = N,
        nrep=100, parallel=TRUE, ncore=8))
summary.power(power_robust)
}
}