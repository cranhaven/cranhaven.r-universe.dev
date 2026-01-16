\name{power.bmem}
\alias{power.bmem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Conducting power analysis
}
\description{
A comprehensive power analysis function, it can conduct power analysis based on normal, bootstrap and robust Huber-type confidence intervals.
}
\usage{
power.bmem(model, method="normal", nobs = 100, nrep = 1000, nboot = 1000, 
alpha = 0.95, skewness = NULL, kurtosis = NULL, ovnames = NULL,  
ci='perc', boot.type='default', 
se = "default", estimator = "default", parallel = FALSE, 
ncore = 1, verbose=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
A model specified using lavaan notation and above. See \code{\link[lavaan]{model.syntax}} for basic model specification. 

For the power analysis, the population parameter values should be provided in the following way. For example, the coefficient between math and HE is .39. Then it is specified as start(.39). If the parameter will be referred in the mediation effect, a label should be given as a modifier as b*HE+start(.39)*HE.

It also specify the indirect or other composite effects  using lavaan notation.

model<-'
math ~ c*ME+start(0)*ME + b*HE+start(.39)*HE
HE ~ a*ME+start(.39)*ME
ab := a*b
abc := a*b + c
'
}
  \item{method}{Type of confidence intervals based on. Must be "normal", "boot" or "robust", which correspond to the normal, bootstrap or robust Huber-type confidence interval, respectively.}
  \item{nobs}{
Number of observations for power analysis. If it is a vector, multiple group analysis will be conducted.
}
  \item{nrep}{
Number of replications for Monte Carlo simulation. At least 1,000 is recommended.
}
\item{nboot}{Number of bootstrap replicates. It's only required when bootstrap method is used.}
  \item{alpha}{
The alpha level is used to obtain the confidence interval for model parameters.
}
  \item{skewness}{
A vector to give the skewness for the observed variables.
}
  \item{kurtosis}{
A vector to give the kurtosis for the observed variables.
}
  \item{ovnames}{
A vector to give the variable names for the observed variables. This is only needed when the skewness and kurtosis are provided. The skewness, kurtosis and variable names should be in the same order.
}
  \item{se}{
How to calculate the standard error, for example, robust standard error can be specified using se="robust".
}
  \item{estimator}{
Estimation methods to be used here.
}
  \item{parallel}{Whether to use parallel method to calculate.}
  \item{ncore}{
Number of cores to be used in parallel.
}
  \item{ci}{Type of bootstrap confidence intervals. By default, the percentile one is used. otherwise get the bias-corrected one. It's only required when bootstrap method is used.}
  \item{boot.type}{Type of bootstrap method. By default, the nonparametric one is used. Changing it to "BS" to use the Bollen-Stine method. It's only required when bootstrap method is used.}
  \item{verbose}{
Whether to print power information.
}
  \item{\dots}{
Other named arguments for lavaan can be passed here.
}
}

\value{
\item{power}{power for all parameters and required ones in the model}
\item{coverage}{coverage probability}
\item{pop.value}{Population parameter values}
\item{results}{A list to give all intermediate results}
\item{data}{The last data set generated for checking purpose}
}

\examples{
\donttest{
ex1model<-'
math ~ c*ME + start(0)*ME + b*HE + start(0.39)*HE
HE ~ a*ME + start(0.39)*ME
ab := a*b
'

N <- 50

system.time(power_normal <- power.bmem(ex1model, method = "normal", nobs = N, 
  nrep=100, parallel=TRUE, skewness=c(-.3, -.7, 1.3), 
  kurtosis=c(1.5, 0, 5), ovnames=c('ME', 'HE', 'math'), ncore=8))
summary(power_normal)
}
}
\author{
Zhiyong Zhang, Shuigen Ming and Lijuan Wang
}
\references{
Zhang, Z. Monte Carlo based statistical power analysis for mediation models: methods and software. Behav Res 46, 1184-1198 (2014).
\doi{https://doi.org/10.3758/s13428-013-0424-0}
}