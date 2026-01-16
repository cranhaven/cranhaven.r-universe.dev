\name{power.curve}
\alias{power.curve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a power curve
}
\description{
Generate a power curve either based on Sobel test or bootstrap
}
\usage{
power.curve(model, nobs=seq(100, 2000, 200), method='normal', nrep=1000, 
nboot=1000, alpha=.95, skewness=NULL, kurtosis=NULL, ovnames=NULL,  
ci='perc', boot.type='default', 
se="default", estimator="default", parallel=FALSE, 
ncore=1, interactive=TRUE, ...)
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
Number of observations for power analysis. It is typically should be a vector for single group analysis. For multiple group analysis, it should be a matrix.
}
  \item{nrep}{
Number of replications for Monte Carlo simulation. At least 1,000 is recommended.
}
  \item{nboot}{Number of bootstraps to conduct.}
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
  \item{parallel}{
Parallel methods, snow or multicore, can be used here. 
}
  \item{ncore}{
Number of cores to be used in parallel. By default, the maximum number of cores are used.
}
\item{ci}{Type of bootstrap confidence intervals. By default, the percentile one is used. To get the bias-corrected one, use ci='BC'}
\item{boot.type}{Type of bootstrap method. By default, the nonparametric one is used. Changing it to "BS" to use the Bollen-Stine method.}
\item{interactive}{Whether to get the figure interactively.}
  \item{\dots}{
Other named arguments for lavaan can be passed here.
}
}
\value{
Generate the nobs-power curves for all relationships given in the model.}

\examples{
\donttest{
ex1model<-'
math ~ c*ME + start(0)*ME + b*HE + start(0.39)*HE
HE ~ a*ME + start(0.39)*ME
ab := a*b
'

nobs <- seq(50, 200, by=50)

power.curve(model=ex1model, nobs=nobs, method='normal', 
nrep = 100, parallel=TRUE, ncore=8)
}
}