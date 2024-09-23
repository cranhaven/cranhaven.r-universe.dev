\name{bhpm.cluster.1a.hier2}
\alias{bhpm.cluster.1a.hier2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A Two-Level Hierarchical Model for Grouped Data with Clusters and without Point-Mass.}
\description{
Implementation of a Two-Level Hierarchical for Grouped Data with Clusters and without Point-Mass.}

\usage{
	bhpm.cluster.1a.hier2(cluster.data, sim_type = "SLICE", burnin = 10000,
	iter = 40000, nchains = 3,
	global.sim.params = data.frame(type = c("MH", "SLICE"),
	param = c("sigma_MH", "w"), value = c(0.2,1), control = c(0,6),
	stringsAsFactors = FALSE),
	sim.params = NULL,
	monitor = data.frame(variable = c("theta", "gamma", "mu.gamma",
	"mu.theta", "sigma2.theta", "sigma2.gamma"),
	monitor = c(1, 1, 1, 1, 1, 1),
	stringsAsFactors = FALSE),
	initial_values = NULL,
	level = 1,
	hyper_params = list(mu.gamma.0 = 0, tau2.gamma.0 = 10, mu.theta.0 = 0,
	tau2.theta.0 = 10, alpha.gamma = 3, beta.gamma = 1,
	alpha.theta = 3, beta.theta = 1),
	memory_model = "HIGH")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cluster.data}{
A file or data frame containing the cluster data. It must contain the columns \emph{Cluster}, \emph{Outcome.Grp}, \emph{Outcome}, \emph{Trt.Grp} (1 - control, 2,... comparator treatments), \emph{Count} (total number of events), \emph{Exposure} (total exposure of time of all patients for the Trt.Grp in the Cluster).
}
  \item{sim_type}{
The type of MCMC method to use for simulating from non-standard distributions. Allowed values are \emph{"MH"}
and \emph{"SLICE"} for Metropis_Hastings and Slice sampling respectively.
}
  \item{burnin}{
The burnin period for the monte-carlo simulation. These are discarded from the returned samples.
}
  \item{iter}{ The total number of iterations for which the monte-carlo simulation is run. This includes the burnin period.
The total number of samples returned is \emph{iter - burnin}
}
  \item{nchains}{
The number of independent chains to run.
}
\item{global.sim.params}{
A data frame containing the parameters for the simuation type \emph{sim_type}. For \emph{"MH"} the parameter
is the variance of the normal distribution used to simulate the next candidate value centred on the current
value. For \emph{"SLICE"} the parameters are the estimated width of the slice and a value limiting the search for the next sample.
}
\item{sim.params}{
A dataframe containing simulation parameters which override the global simulation parameters (\emph{global.sim.params}) for particular model
parameters. \emph{sim.params} must contain the following columns: type: the simulation type ("MH" or "SLICE"); variable: the model parameter 
for which the simulation parameters are being overridden; Outcome.Grp: the outcome grouping (if applicable); Outcome: the outcome (if applicable);
param: the simulation parameter; value: the overridden value; control: the overridden control value.

The function \emph{bhpm.sim.control.params} generates a template for \emph{sim.params} which can be edited by the user.
}

\item{monitor}{
A dataframe indicating which sets of variables to monitor.
}

  \item{initial_values}{
The initial values for starting the chains. If NULL (the default) is passed the function generates the initial
values for the chains. initial_values is a list with the following format:
\preformatted{
list(gamma, theta, mu.gamma, mu.theta, sigma2.gamma,
		sigma2.theta)
}
where each element of the list is either a dataframe or array.
The function \emph{bhpm.gen.initial.values} can be used to generate a template for the list which can be updated by the user if required.
}

\item{level}{
The level of dependancy between the clusters. 0 - independent clusters, 1 - common cluster means.
dependancy.
}
\item{hyper_params}{
The hyperparameters for the model.
}

\item{memory_model}{
Allowed values are "HIGH" and "LOW". "HIGH" means use as much memory as possible. "LOW" means use the minimum amount of memory.
}
}
\details{
The model is fitted by a Gibbs sampler.
The posterior distributions for \emph{gamma} and \emph{theta} are sampled with either a Metropolis-Hastings step or a slice sampler.
}
\value{
The output from the simulation including all the sampled values is as follows:
\preformatted{
list(id, sim_type, chains, nClusters, Clusters, Trt.Grps, nOutcome.Grp, maxOutcome.Grps,
	maxOutcomes, nOutcome, Outcome, Outcome.Grp, burnin, iter, monitor,
	mu.gamma, mu.theta, sigma2.gamma, sigma2.theta, gamma,
	theta, gamma_acc, theta_acc)
}
where

\emph{id} - a string identifying the verion of the function

\emph{sim_type} - an string identifying the samlping method used for non-standard distributions, either \emph{"MH"} or \emph{"SLICE"}

\emph{chains} - the number of chains for which the simulation was run.

\emph{nClusters} - the number of clusters in the simulation

\emph{Clusters} - an array. The clusters.

\emph{Trt.Grps} - an array. The treatments.

\emph{nOutcome.Grp} - the number of outcome groupings.

\emph{maxOutcome.Grps} - the maximum number of outcome groupings in a cluster.

\emph{maxOutcomes} - the maximum number of outcomes in an outcome grouping.

\emph{nOutcome} - an array. The number of outcomes in each outcome grouping.

\emph{Outcome} - an array of dimension \emph{nOutcome.Grp}, \emph{nOutcome}. The outcomes.

\emph{Outcome.Grp} - an array. The outcome groupings.

\emph{burnin} - burn-in used for the simulation.

\emph{iter} - the total number of iterations in the simulation.

\emph{monitor} - the variables being monitored. A dataframe.

\emph{mu.gamma} - array of generated samples.

\emph{mu.theta} - array of generated samples.

\emph{sigma2.gamma} - array of generated samples.

\emph{sigma2.theta} - array of generated samples.

\emph{gamma} - array of generated samples.

\emph{theta} - array of generated samples.

\emph{gamma_acc} - array of generated samples.

\emph{theta_acc} - the acceptance rate for the theta samples if a Metropolis-Hastings method is used.
}
%\references{
%}
\author{
R. Carragher
}
\note{
The function performs the simulation and returns the raw output. No checks for convergence are performed. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(bhpm.cluster.data1)
raw = bhpm.cluster.1a.hier2(bhpm.cluster.data1, level = 1, burnin = 100, iter = 200)
\donttest{
data(bhpm.cluster.data1)
raw = bhpm.cluster.1a.hier2(bhpm.cluster.data1, level = 1)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{bhpm.cluster.1a.hier2}
\keyword{Bayesian} % __ONLY ONE__ keyword per line
\keyword{Hierarchy} % __ONLY ONE__ keyword per line
\keyword{Cluster} % __ONLY ONE__ keyword per line
\keyword{Adverse event} % __ONLY ONE__ keyword per line
\keyword{Adverse outcome} % __ONLY ONE__ keyword per line

