\name{sampleM}
\alias{sampleM}
\title{
Update individual level latent class  indexes.
}
\description{
Update individual level latent class  indexes.
}
\usage{
sampleM(phi, data, omega, G, serial,  Parallel)
}
\arguments{
  \item{phi}{
  Matrix of posterior samples for the individual-level probabilities for each individual-level variable by each pair of group-level and individual-level latent classes.
}
  \item{data}{
  Input individual-level data.
}
  \item{omega}{
  Matrix of the probabilities for the individual-level latent classes by the group-level latent classes.
}
  \item{G}{
  Household-level latent class indexes.
}
  \item{serial}{
  Vector containing the household index for each individual in the data.
}
\item{Parallel}{
  Logical indicator for running the function in parallel mode.
}
}
\details{
Function for obtaining a posterior sample of the individual-level latent class indexes for all individuals in the input data based on the corresponding full conditional distribution.
}
\value{
A vector for the updated values of the individual-level latent class indexes for all individuals in the input data.
}

\author{
Quanli Wang
}
\keyword{sampler}
\keyword{individual level}
