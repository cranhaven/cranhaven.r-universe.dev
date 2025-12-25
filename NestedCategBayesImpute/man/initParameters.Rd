\name{initParameters}
\alias{initParameters}
\title{
Initialize the model parameters for the MCMC.
}
\description{
Initialize the model parameters for the MCMC.
}
\usage{
initParameters(data, hyper, HHhead_at_group_level)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  A list object including all the necessary data variables needed by the sampler; output of the \verb{initData} function.
}
  \item{hyper}{
  Hyper parameters for the prior distributions.
}
  \item{HHhead_at_group_level}{
  Logical indicator for data structure with respect to the household head. Set to TRUE if the household head has been moved to the household level and FALSE otherwise.
}
}

\value{
A list of the initial values of the parameters.

\item{alpha}{Concentration parameter in the Dirichlet process for the group-level latent classes.}
\item{beta}{Concentration parameter in the Dirichlet process for the individual-level latent classes. Currently, this is assumed to be the same within all group-level classes.}
\item{phi}{Matrix of posterior samples for the individual-level probabilities for each individual-level variable by each pair of group-level and individual-level latent classes.}
\item{HHdata_all}{The transposed household level data -- each column represents each household.}
\item{lambda}{A list of matrices of the group-level probabilities for each group-level variable by the group-level latent classes. Each matrix in the list is for each group-level variable.}
\item{u}{Vector of the beta-distributed variables in the stick breaking representation of the group-level latent classes.}
\item{pi}{Vector of the probabilities for the group-level latent classes.}
\item{v}{Matrix of the beta-distributed variables in the stick breaking representation of the individual-level latent classes by the group-level latent classes.}
\item{omega}{Matrix of the probabilities for the individual-level latent classes by the group-level latent classes.}
}

\author{
Quanli Wang
}
