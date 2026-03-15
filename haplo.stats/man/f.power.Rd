\name{f.power}
\alias{f.power}
\alias{f.power.dif}
\alias{f.sample.size}
\title{
  Power and sample size for the F distribution
}
\description{
Power and sample size for the F distribution given
non-centrality, degrees of freedom, alpha, N (for f.power), and
power (for f.sample.size) 
}
\usage{
f.power(n, nc, df1, alpha)
f.power.dif(n, nc, df1, alpha, power)
f.sample.size(nc, df1, alpha, power, lower=20, upper=10000)
}
\arguments{

\item{n}{
sample size
}
\item{nc}{
non-centrality parameter
}
\item{df1}{
degrees of freedom for numerator of f distribution
}
\item{alpha}{
type-I error
}
\item{power}{
desired power (for sample size)
}
\item{lower}{
lower limit for search space for sample size solution
}
\item{upper}{
upper limit for search space for sample size solution
}
}
\value{
power, the difference in power from target power, and sample size,
respectively for the three functions, assuming an F distribution for
the test statistic
}
\keyword{power}
% docclass is function
% Converted by Sd2Rd version 43268.
