\name{bhpm.convergence.diag}
\alias{bhpm.convergence.diag}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convergence Diagnostics of the Simulation}
\description{
The function applies either Gelman-Rubin or the Geweke diagnostic to the raw output of model simulation (e.g. bhpm.cluster.BB.hier3).
It returns the convergence diagnostics and, if applicable, the acceptance rates for the sampling distributions.
}
\details{
This function applies one of two convergence diagnostics to the raw output of a model simulation
in order to allow convergence to be assessed. The two diagnotics are:

i) Gelman-Rubin diagnostic - used when there is more than one chain. A value close to 1 is consistent with
an MCMC simulation which has converged. The `coda' diagnostic returns a point estimate and upper confidence
limits.

ii) Geweke diagnostic - used when there is a single chain. A Z-score which is consistent with a standard normal
distribution is expected from an MCMC simulation which has converged.

The raw sample data is converted to `coda' format (mcmc objects) and the `coda' methods gelman.diag and
geweke.diag are used to perform the checks.
}
\usage{
	bhpm.convergence.diag(raw, debug_diagnostic = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{raw}{
The output from a model simulation.
}
  \item{debug_diagnostic}{
Unused parameter.
}
}
\value{
Returns a list of the diagnostics for each sampled variable. Each individual element of the list is a
data.frame containing at least the columns \emph{type}, which is the type of diagnostic
(`Gelman-Rubin' or `Geweke'), \emph{stat}, which is the value of the dignostic, and \emph{upper_ci} which is
the upper confidence interval for the Gelman-Rubin diagnostic. For the Geweke diagnostic \emph{upper_ci}
contains the value NA. Depending on the simulation performed the return from \emph{bhpm.convergence.diag} will contain different
variables.
Columns which may be used to indentify the individual samples are \emph{Trt.Grp}, \emph{Outcome.Grp},
\emph{Outcome}, and \emph{Cluster}.
}
\author{
R. Carragher
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(bhpm.cluster.data1)
data <- subset(bhpm.cluster.data1, Cluster == '0.0-180.0' | Cluster == '180.0-360.0')
raw = bhpm.npm(data, burnin = 100, iter = 200)
conv = bhpm.convergence.diag(raw)

\donttest{
data(bhpm.cluster.data1)
raw = bhpm.npm(bhpm.cluster.data1)
conv = bhpm.convergence.diag(raw)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{bhpm.convergence.diag}
\keyword{Bayesian} % __ONLY ONE__ keyword per line
\keyword{Hierarchy} % __ONLY ONE__ keyword per line
\keyword{Gelman-Rubin}
\keyword{Cluster} % __ONLY ONE__ keyword per line
\keyword{Adverse event} % __ONLY ONE__ keyword per line
\keyword{Adverse outcome} % __ONLY ONE__ keyword per line
