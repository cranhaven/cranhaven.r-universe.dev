\name{bhpm.global.sim.param.defaults}
\alias{bhpm.global.sim.param.defaults}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generate default global simulation parameters for a model.}
\description{
Generate default global simulation parameters for a model.
}
%\details{
%}

\usage{
	bhpm.global.sim.param.defaults(model = "BB", hier = 3)
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
A dataframe containing the global simulation parameters.
}
\author{
R. Carragher
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
g.s.p <- bhpm.global.sim.param.defaults("BB", 3)
print(g.s.p)
\donttest{
g.s.p <- bhpm.global.sim.param.defaults("BB", 3)
print(g.s.p)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{bhpm.global.sim.param.defaults}
\keyword{Bayesian} % __ONLY ONE__ keyword per line
\keyword{Hierarchy} % __ONLY ONE__ keyword per line
\keyword{Cluster} % __ONLY ONE__ keyword per line
\keyword{Adverse event} % __ONLY ONE__ keyword per line
\keyword{Adverse outcome} % __ONLY ONE__ keyword per line
