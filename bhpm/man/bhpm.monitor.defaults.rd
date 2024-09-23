\name{bhpm.monitor.defaults}
\alias{bhpm.monitor.defaults}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generate default variable monitor list for a model.}
\description{
Generate default variable monitor list for a model.
}
%\details{
%}

\usage{
	bhpm.monitor.defaults(model = "BB", hier = 3)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{model}{
Allowed values are "BB" and "1a" for point-mass and non-point-mass models respectively.
}
  \item{hier}{
Allowed values are 2 and 3 two-level and three-level hierarchies repsectively.
}
}


\value{
A dataframe containing the default monitored variables.
}
\author{
R. Carragher
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
mon <- bhpm.monitor.defaults("BB", 3)
print(mon)
\donttest{
mon <- bhpm.monitor.defaults("BB", 3)
print(mon)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{bhpm.monitor.defaults}
\keyword{Bayesian} % __ONLY ONE__ keyword per line
\keyword{Hierarchy} % __ONLY ONE__ keyword per line
\keyword{Cluster} % __ONLY ONE__ keyword per line
\keyword{Adverse event} % __ONLY ONE__ keyword per line
\keyword{Adverse outcome} % __ONLY ONE__ keyword per line
