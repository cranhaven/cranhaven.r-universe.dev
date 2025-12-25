\name{UpdateOmega}
\alias{UpdateOmega}
\title{
Update omega and v.
}
\description{
Update omega -- the matrix of the probabilities for the individual-level latent classes by the group-level latent classes -- and v -- the matrix of the beta-distributed variables in the stick breaking representation of the individual-level latent classes by the group-level latent classes -- when the weighting/capping option is not used.
}
\usage{
UpdateOmega(beta, M_all, FF, SS)
}
\arguments{
  \item{beta}{
  Concentration parameter in the Dirichlet process for the individual-level latent classes. Currently, this is assumed to be the same within all group-level classes.
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
}

\value{
A list containing the updated (posterior) values for omega and v based on the corresponding full conditional distributions.
}

\author{
Quanli Wang
}

\keyword{sampler}

