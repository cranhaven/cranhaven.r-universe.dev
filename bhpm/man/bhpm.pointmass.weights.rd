\name{bhpm.pointmass.weights}
\alias{bhpm.pointmass.weights}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generate a template for the point-mass weightings.}
\description{
This function generate a template for weights for the proposal distribution used to sample \emph{theta} variables in models which 
use a point-mass.
}
%\details{
%}

\usage{
	bhpm.pointmass.weights(cluster.data)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{cluster.data}{
A file or data frame containing the cluster data for analysis.
}
}


\value{
A dataframe containing the weightings template for each outcome grouping, outcome and, if required, cluster.
}
\author{
R. Carragher
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(bhpm.cluster.data1)
pmw <- bhpm.pointmass.weights(bhpm.cluster.data1)
head(pmw, 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{bhpm.pointmass.weights}
\keyword{Bayesian} % __ONLY ONE__ keyword per line
\keyword{Hierarchy} % __ONLY ONE__ keyword per line
\keyword{Cluster} % __ONLY ONE__ keyword per line
\keyword{Adverse event} % __ONLY ONE__ keyword per line
\keyword{Adverse outcome} % __ONLY ONE__ keyword per line
