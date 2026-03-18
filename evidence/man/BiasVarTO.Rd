\name{BiasVarTO}
\alias{BiasVarTO}
\title{
a simple example of the bias--variance trade-off.
}
\description{
A total of eight models are fitted to a data set consisting of seven
predictors. The response is the exact fit with a variable amount of 
zero-mean noise added. This is repeated a certain number of times (by
default, 100 times). Plots of Bias^2 and variance vs. the number of
parameters are produced.
}
\usage{
BiasVarTO(times = 100)
}
\arguments{
  \item{times}{
the number of repeats to average bias and variance over (default 100)
}
}
\value{
none produced, the function produces two plots
}
\references{
van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\author{
Robert van Hulst
}

\keyword{models}

