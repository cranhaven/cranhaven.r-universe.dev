\name{bhpm.hyper.param.defaults}
\alias{bhpm.hyper.param.defaults}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generate default hyperparameter values for a model.}
\description{
Generate default hyperparameter values for a model.
}
%\details{
%}

\usage{
	bhpm.hyper.param.defaults(model = "BB", hier = 3)
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
A list containing the default parameter values.
}
\author{
R. Carragher
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
h.p <- bhpm.hyper.param.defaults("BB", 3)
print(h.p$mu.gamma.0.0)
\donttest{
h.p <- bhpm.hyper.param.defaults("BB", 3)
print(h.p$mu.gamma.0.0)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{bhpm.hyper.param.defaults}
\keyword{Bayesian} % __ONLY ONE__ keyword per line
\keyword{Hierarchy} % __ONLY ONE__ keyword per line
\keyword{Cluster} % __ONLY ONE__ keyword per line
\keyword{Adverse event} % __ONLY ONE__ keyword per line
\keyword{Adverse outcome} % __ONLY ONE__ keyword per line
