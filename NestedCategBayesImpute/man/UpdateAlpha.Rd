\name{UpdateAlpha}
\alias{UpdateAlpha}
\title{
Update alpha.
}
\description{
Update alpha -- the concentration parameter in the Dirichlet process for the group-level latent classes.
}
\usage{
UpdateAlpha(aa, ab, u)
}
\arguments{
  \item{aa}{
  Hyper-parameter a for alpha.
}
  \item{ab}{
  Hyper-parameter b for alpha.
}
  \item{u}{
  Vector of the beta-distributed variables in the stick breaking representation of the group-level latent classes.
}
}

\value{
Updated (posterior) value for alpha based on the corresponding full conditional distribution.
}

\author{
Quanli Wang
}

\keyword{ sampler }

