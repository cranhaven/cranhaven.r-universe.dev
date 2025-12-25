\name{UpdatePhi}
\alias{UpdatePhi}
\title{
Update phi.
}
\description{
Update phi -- the matrix of posterior samples for the individual-level probabilities for each individual-level variable by each pair of group-level and individual-level latent classes -- when the weighting/capping option is not used.
}
\usage{
UpdatePhi(data, M_all, FF, SS, d, maxd)
}
\arguments{
  \item{data}{
  Data matrix for the individual-level data from both the original data and the sampled impossible households.
}
  \item{M_all}{
 A vector of both the household-level and individual-level latent class indexes for all households both in the original data and the sampled impossible households.
}
  \item{FF}{
  Maximum number of household-level latent classes allowed.
}
  \item{SS}{
  Maximum number of individual-level latent classes allowed.
}
  \item{d}{
  A vector for the number of levels of each individual-level variable.
}
  \item{maxd}{
  Maximum value in \verb{d}.
}
}
\details{
Function for obtaining a posterior sample of phi when the weighting/capping option is not used.
}
\value{
Updated (posterior) value for phi based on the corresponding full conditional distribution.
}

\author{
Quanli Wang
}

\keyword{ sampler }
