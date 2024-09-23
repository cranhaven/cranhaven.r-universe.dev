\name{bhpm.sim.control.params}
\alias{bhpm.sim.control.params}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generate a template for the individual model parameter simulation control parameters.}
\description{
This function generates a template for overriding the global simulation parameters used by the model simulation functions (e.g. bhpm.cluster.BB.hier3).
}
%\details{
%}

\usage{
	bhpm.sim.control.params(cluster.data, model = "1a")
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{cluster.data}{
A file or data frame containing the cluster data.
}
  \item{model}{
Allowed values are "BB" and "1a" for point-mass and non-point-mass models respectively.
}
}


\value{
A dataframe containing the simulation parameters template.
}
\author{
R. Carragher
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(bhpm.cluster.data1)
s.c.p <- bhpm.sim.control.params(bhpm.cluster.data1)
head(s.c.p)
\donttest{
data(bhpm.cluster.data1)
s.c.p <- bhpm.sim.control.params(bhpm.cluster.data1)
head(s.c.p)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{bhpm.sim.control.params}
\keyword{Bayesian} % __ONLY ONE__ keyword per line
\keyword{Hierarchy} % __ONLY ONE__ keyword per line
\keyword{Cluster} % __ONLY ONE__ keyword per line
\keyword{Adverse event} % __ONLY ONE__ keyword per line
\keyword{Adverse outcome} % __ONLY ONE__ keyword per line
