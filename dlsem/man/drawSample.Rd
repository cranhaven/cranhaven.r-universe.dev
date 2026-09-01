\name{drawSample}
\alias{drawSample}
\title{Sampling from a distributed-lag linear structural equation model}
\description{A future sample from a distributed-lag linear structural equation model is drawn.}
\usage{drawSample(x, n)}
\arguments{
  \item{x}{An object of class \code{dlsem}.}
  \item{n}{The sample size (temporal horizon).}
}
\value{An object of class \code{data.frame}.}
\note{
Sampling is conditioned to the most recent observed value of all the variables.

For variables subdued to logarithmic transformation and/or differencing,
the sampled values are in logarithmic scale and/or after differencing, as well.

If a group factor was specified for the model, a sample of size \code{n} is drawn for each group.
%If the model contains exogenous variables, sampling is conditioned to their mean value (quantitative variables) or reference category (qualitative variables).
}
%\references{}
\seealso{\link{dlsem}.}
\examples{
data(industry)
indus.code <- list(
  Consum~ecq(Job,0,5),
  Pollution~ecq(Job,1,8)+ecq(Consum,1,7)
  )
indus.mod <- dlsem(indus.code,group="Region",time="Year",exogenous=c("Population","GDP"),
  data=industry,log=TRUE)
drawSample(indus.mod,10)
}