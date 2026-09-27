\name{chisq.power}
\alias{chisq.power}
\alias{chisq.power.dif}
\alias{chisq.sample.size}
\title{
  Power and sample size for the chi-square distribution
}
\description{
Power and sample size for the chi-square distribution given
non-centrality, degrees of freedom, alpha, N (for chisq.power), and
power (for chisq.sample.size) 
}
\usage{
chisq.power(n, nc, df, alpha)
chisq.power.dif(n, nc, df, alpha, power)
chisq.sample.size(nc, df=df, alpha, power, lower=20, upper=100000)
}
\arguments{

\item{n}{
sample size (for power)
}
\item{nc}{
non-centrality parameter
}
\item{df}{
degrees of freedom
}
\item{alpha}{
type-I error rate
}
\item{power}{
desired power (for sample size)
}
\item{lower}{
lower bound for search space for sample size
}
\item{upper}{
upper bound for search space for sample size
}
}
\value{
power, the difference in power from target power, and sample size,
respectively for the three different functions
}
\keyword{power}
% docclass is function
% Converted by Sd2Rd version 43268.
