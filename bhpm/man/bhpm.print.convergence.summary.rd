\name{bhpm.print.convergence.summary}
\alias{bhpm.print.convergence.summary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Print a Summary of the Convergence Diagnostics of the Simulation}
\description{
The function prints the maximum and minimum values of either Gelman-Rubin diagnostic or the Geweke diagnostic
for each group of samples, e.g. theta, gamma, mu.gamma etc.
}
\usage{
	bhpm.print.convergence.summary(conv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conv}{
The output from a call to \emph{bhpm.check.convergence}.
}
}
\value{
Nothing
}
\author{
R. Carragher
}
\note{
The Geweke statistic is a Z-score calculated from a single chain. Due to the large number of variables sampled
it is possible that a certain number will be deemed significant (at the 5\% level) even though the simulation
may have converged.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(bhpm.cluster.data1)
data <- subset(bhpm.cluster.data1, Cluster == '0.0-180.0' | Cluster == '180.0-360.0')
raw = bhpm.npm(data, burnin = 100, iter = 200)
conv = bhpm.convergence.diag(raw)
bhpm.print.convergence.summary(conv)
\donttest{
data(bhpm.cluster.data1)
raw = bhpm.npm(bhpm.cluster.data1)
conv = bhpm.convergence.diag(raw)
bhpm.print.convergence.summary(conv)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{bhpm.print.convergence.summary}
\keyword{Bayesian} % __ONLY ONE__ keyword per line
\keyword{Hierarchy} % __ONLY ONE__ keyword per line
\keyword{Gelman-Rubin}
\keyword{Cluster} % __ONLY ONE__ keyword per line
\keyword{Adverse event} % __ONLY ONE__ keyword per line
\keyword{Adverse outcome} % __ONLY ONE__ keyword per lin
