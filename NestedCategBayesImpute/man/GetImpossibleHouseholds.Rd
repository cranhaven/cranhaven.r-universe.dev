\name{GetImpossibleHouseholds}
\alias{GetImpossibleHouseholds}
\title{
Generate the desired number of impossible households required to observe a given number of possible households.
}
\description{
Given model parameters, generate the desired number of impossible households required to observe a given number of possible households. Also generate synthetic (and valid) data of the same size as the observed data when required.
}
\usage{
GetImpossibleHouseholds(d, n_star_h, lambda, omega, phi, pi, blocksize, n, synindex,
                        HHhead_at_group_level,Parallel)
}
\arguments{
  \item{d}{
  Vector containing the number of levels for each individual-level variable.
}
  \item{n_star_h}{
  Vector containing the number of observed households for the different household sizes in the original data.
}
  \item{lambda}{Multinomial probabilities for each group-level variable.
}
  \item{omega}{Latent class probabilities for the group-level and individual-level latent class pairs.
}
  \item{phi}{Multinomial probabilities for each individual-level variable by each pair of group-level and individual-level latent classes.
}
  \item{pi}{Latent class probabilities for the group-level latent classes.
}
  \item{blocksize}{
  Number of households to be generated at a time; batch sampling is used to improve computing speed.
}
  \item{n}{
  Number of households in the original input data and the sum of \verb{n_star_h}.
}
  \item{synindex}{Logical indicator for sampling synthetic data. Set to TRUE when synthetic data is needed.
}
  \item{HHhead_at_group_level}{
  Logical indicator for data structure with respect to the household head. Set to TRUE if the household head has been moved to the household level and FALSE otherwise.
}
\item{Parallel}{
  Logical indicator for running the function in parallel mode.
}
}

\value{
  \item{G_Individuals_and_M_extra}{A data matrix containing both the group-level (in long format) and individual-level latent classes for the impossible households.}
  \item{G_extra }{A vector containing the group-level latent classes for the impossible households.}
  \item{IndividualData_extra }{A data matrix containing the individual-level data for the impossible households.}
  \item{HHdata_extra}{A data matrix containing the group-level data for the impossible households.}
  \item{hh_size_new }{A vector for the number of impossible households for the different household sizes.}
  \item{synIndividuals_all }{Synthetic data when synindex is TRUE. NULL otherwise.}
}

\author{
Quanli Wang
}

\keyword{sampler}
\keyword{impossible household}
\keyword{synthetic data}
