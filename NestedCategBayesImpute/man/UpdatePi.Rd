\name{UpdatePi}
\alias{UpdatePi}
\title{
Update pi and u.
}
\description{
Update pi -- the vector of the probabilities for the group-level latent classes -- and u -- the vector of the beta-distributed variables in the stick breaking representation of the group-level latent classes -- when the weighting/capping option is not used.
}
\usage{
UpdatePi(alpha, G_all, FF)
}
\arguments{
  \item{alpha}{
  Concentration parameter in the Dirichlet process for the group-level latent classes
}
  \item{G_all}{
  A vector of the household-level latent class indexes for all households both in the original data and the sampled impossible households.
}
  \item{FF}{
  Maximum number of household-level latent classes allowed.
}
}
\details{
Function for obtaining a posterior sample of pi when the weighting/capping option is not used.
}
\value{
A list containing the updated (posterior) values for pi and u based on the corresponding full conditional distributions.
}

\author{
Quanli wang
}

\keyword{ sampler }

