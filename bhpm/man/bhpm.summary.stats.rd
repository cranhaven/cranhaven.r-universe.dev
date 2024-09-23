\name{bhpm.summary.stats}
\alias{bhpm.summary.stats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Summary Statistics for the Posterior Distributions in the model.}
\description{
Returns the Summary Statistics for the Posterior Distributions in the model.
}
\details{
The function reports the mean, upper and lower bounds of the HPI (highest probabily interval), the 
standard deviation and MCMC standard error.
}
\usage{
	bhpm.summary.stats(raw, prob)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{raw}{
The output from a model simulation (e.g. bhpm.cluster.BB.hier3).
}
  \item{prob}{
The probability level for HPI. Default is 0.95.
}
}
\value{
Returns a list of the summary statistics for each sampled variable.
Each element of the list is a data.frame containing at least the columns \emph{mean}, \emph{median}, \emph{hpi_lower},
\emph{hpi_upper}, \emph{SD} and \emph{SE}.
Columns which may be used to indentify the individual variables are \emph{Outcome.Grp}, \emph{Outcome}, and \emph{Cluster}.
}
\note{
The MCMC error is found using the 'coda' summary function.
}
\author{
R. Carragher
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(bhpm.cluster.data1)
data <- subset(bhpm.cluster.data1, Cluster == '0.0-180.0')
raw = bhpm.npm(data, burnin = 100, iter = 200)
summ = bhpm.summary.stats(raw)

\donttest{
data(bhpm.cluster.data1)
raw = bhpm.npm(bhpm.cluster.data1)
summ = bhpm.summary.stats(raw)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{bhpm.summary.stats}
\keyword{Bayesian} % __ONLY ONE__ keyword per line
\keyword{Hierarchy} % __ONLY ONE__ keyword per line
\keyword{Cluster} % __ONLY ONE__ keyword per line
\keyword{Adverse event} % __ONLY ONE__ keyword per line
\keyword{Adverse outcome} % __ONLY ONE__ keyword per line
