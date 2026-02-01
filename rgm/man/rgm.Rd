\name{rgm}
\alias{rgm}
\title{Random Graphical Model}
\description{
  The function implements Bayesian inference of a random graphical model for multivariate data across multiple environments. The random graph prior assumes that there exists an underlying 2D latent space where the environments are located. Their vicinity in this space relates to structural similaries between the conditions. The model estimates these latent positions, the sparsity levels for each network, the regression coefficients of edge covariates associated to the propensity of two nodes ot connect (if available) and the network structures for each environment.
}
\usage{
  rgm(data, X=NULL, iter = 1000, burnin = 0,
  initial.graphs = NULL, D = 2, initial.loc = NULL,
  initial.alpha = NULL, initial.theta = NULL,
  bd.iter = 20, bd.jump = 10,
  method = c("ggm", "gcgm"), gcgm.dwpar = NULL)
}
\arguments{
  \item{data}{a list of B multivariate datasets measuring p variables across B number of environments.}
  \item{X}{an n.edge x ncol(X) data matrix for the edge covariates. Default is \code{NULL}, corresponding to the absence of edge covariates.}
  \item{iter}{number of iterations for the MCMC sampler. Default is 1000.}
  \item{burnin}{number of burn-in iterations to discard. Default is 0.}
  \item{initial.graphs}{an optional matrix of binary adjacency matrices for the initial graphs, with dimension n.edge x B. Default is \code{NULL} and in this case the initial graphs are constructed using graphical lasso (function huge).}
  \item{D}{number of dimensions in the latent space. Default is 2.}
  \item{initial.loc}{initial values for the B x D matrix of latent node positions. Default is \code{NULL} and in this case the initial values are drawn from a N(0,1) distribution.}
  \item{initial.alpha}{initial values for the B-dimensional intercepts. Default is \code{NULL} and in this case they are set to 0.}
  \item{initial.theta}{initial values for the regression coefficients associated to the covariates in X. Default is \code{NULL} and in this case they are set via a probit regression of the initial graphs on the edge covariates.}
  \item{bd.iter}{number of iterations for the BDgraph function. Default is 20.}
  \item{bd.jump}{number of links to be updated simulateneously for the BDgraph function. Default is 10.}
  \item{method}{method used for network estimation. Options are "ggm" (Gaussian graphical model) or "gcgm" (Gaussian copula graphical model). Default is "ggm".}
  \item{gcgm.dwpar}{a list of B elements, each containing the parameters of the discrete Weibull marginal fitting within each environment. This input is required only for method "gcgm" and is passed on to the function "sample.data". Default is \code{NULL}.}
}
\details{
rgm is a Bayesian random graphical model that infers the location of each environment in a 2-dimensional latent space. The probability of a link between two nodes in one environment is related to the distance of this environment to the other environments in the latent space as well as to the presence of an edge in the related environments. The model also allows for network-specific intercepts and regression coefficients for covariates measured at the edge level.

The function first initializes the latent positions, intercepts, regression coefficients and the initial graphs, if not provided. It then loops through the iterations and updates the latent positions, regression coefficients and intercepts using the Gmcmc function. Next, it calculates the probability of edge connections for each condition. Finally, it updates the network structure for each condition using the BDgraph package. The function returns the posterior samples of the parameters after discarding the burn-in period.
}
\value{
  A list containing the posterior samples of the model parameters. The list includes:
  \item{sample.alpha}{a B x (iter - burnin) matrix of the \code{alpha} posterior samples of the network-specific intercepts}
  \item{sample.theta}{an ncol(X) x (iter - burnin) matrix of the posterior samples of the regression coefficients for the covariates in X. This is only returned if X is not \code{NULL}.}
  \item{sample.loc}{a B x D x (iter - burnin) array of the posterior samples of the latent positions of the conditions.}
  \item{sample.graphs}{an n.edge x B x (iter - burnin) array of the posterior samples of the network structures.}
  \item{sample.K}{an (n.edge+p) x B x (iter - burnin) array of the posterior samples of the precision matrices.}
  \item{sample.pi}{an n.edge x B x (iter - burnin) array of the posterior edge probabilities in each network.}
  \item{pi.probit}{an n.edge x B x (iter - burnin) array of the estimated probit probabilities of the edge connections in each network.}
}
\examples{
\donttest{
  # simulate data
  sim_data <- sim.rgm(n = 10, D = 2, p = 7, B = 5)

  # run inference
  rgm(sim_data$data,X=sim_data$X,iter=1000)
}
}

\author{
Veronica Vinciotti, Ernst C. Wit and Francisco Richter
}
