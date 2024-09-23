\name{bhpm.ptheta}
\alias{bhpm.ptheta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reports the posterior probability that theta (the increase in the log-odds) is greater than zero, zero, and less than zero for each outcome}
\description{
This function reports the posterior probability that theta is positive negative or zero, i.e. that there is an increase, decrease, or no difference in the log
odds of an outcome being associated with treatment.
}
\usage{
	bhpm.ptheta(raw)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{raw}{
The output from a call to one of bhpm.cluster.BB.hier3, bhpm.cluster.1a.hier3, bhpm.cluster.BB.hier2, bhpm.cluster.1a.hier2.
}
}
\value{
A data frame containing the columns: \emph{Trt.Grp}, \emph{Cluster}, \emph{Outcome.Grp}, \emph{Outcome}, \emph{ptheta}, \emph{ptheta.pos}, \emph{ptheta.zero}, \emph{ptheta.neg}.
}
\author{
R. Carragher
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

data(bhpm.cluster.data1)
data <- subset(bhpm.cluster.data1, Cluster == '0.0-180.0' | Cluster == '180.0-360.0')
raw = bhpm.npm(data, burnin = 10, iter = 100)
p = bhpm.ptheta(raw)
head(p, 2)

\donttest{
data(bhpm.cluster.data1)
raw = bhpm.npm(bhpm.cluster.data1)
p = bhpm.ptheta(raw)
head(p, 2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{bhpm.ptheta}
\keyword{Bayesian} % __ONLY ONE__ keyword per line
\keyword{Hierarchy} % __ONLY ONE__ keyword per line
\keyword{Cluster} % __ONLY ONE__ keyword per line
\keyword{Adverse event} % __ONLY ONE__ keyword per line
\keyword{Adverse outcome} % __ONLY ONE__ keyword per line
