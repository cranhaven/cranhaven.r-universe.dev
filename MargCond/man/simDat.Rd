\name{simDat}
\alias{simDat}
\title{
Function to simulate multivariate longitudinal data
}
\description{
A function that simulates correlated multivariate data based on a set of fixed and random effects.
}
\usage{
simDat(n, fixed_effects, rand_effects, error_var = c(2, 2), 
error_structure = "normal", rho = 0, times = 1:5, X = NULL, Z = NULL)
}
\arguments{
  \item{n}{
  total sample size (number of clusters)
}
  \item{fixed_effects}{
  list of fixed effect vectors for each outcome
}
  \item{rand_effects}{
  list of random effect vectors for each outcome
}
  \item{error_var}{
  vector of error variances for each outcome
}
  \item{error_structure}{
  structure for the random error term, either \code{"normal"} for multivariate normal or \code{"50:50 normal"} for a mixture of two normal distributions
}
  \item{rho}{
  correlation between outcomes
}
  \item{times}{
  times for each repeated measure
}
  \item{X}{
  fixed effect design matrix
}
  \item{Z}{
  random effect design matrix
}
}

\value{
A dataframe included simulated outcomes and the design matrices
}

\examples{
set.seed(2112)
NN = 80
n_times = 1:3

## Simulating some data
simdat <- simDat(n = NN, 
                 fixed_effects = list(c(1, 1, 2), c(1.5, 1, 3)), 
                 rand_effects = list(1, 1),
                 error_var = c(4, 4),
                 error_structure = 'normal',
                 rho = .35,
                 times = n_times,
                 X = cbind(rep(1, NN * length(n_times)), 
                           rnorm(NN * length(n_times), 0, 2), 
                           rbinom(NN * length(n_times), 1, .5)),
                 Z = cbind(rep(1, NN * length(n_times))))
                  
## Adding random missing values
aa <- sample(1:nrow(simdat), 10, replace = TRUE)
bb <- sample(1:7, 10, replace = TRUE)
for (i in 1:length(aa)) {
  simdat[aa[i], bb[i]] <- NA
}
}

\keyword{datagen}