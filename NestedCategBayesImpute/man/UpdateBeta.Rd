\name{UpdateBeta}
\alias{UpdateBeta}
\title{
Update beta.
}
\description{
Update beta -- the concentration parameter in the Dirichlet process for the individual-level latent classes. Currently, this is assumed to be the same within all group-level classes.
}
\usage{
UpdateBeta(ba, bb, v)
}
\arguments{
  \item{ba}{
  Hyper-parameter a for beta.
}
  \item{bb}{
  Hyper-parameter b for beta.
}
  \item{v}{
  Matrix of the beta-distributed variables in the stick breaking representation of the individual-level latent classes by the group-level latent classes.
}
}

\value{
Updated (posterior) value for beta based on the corresponding full conditional distribution..
}

\author{
Quanli Wang
}

\keyword{ sampler }

