\docType{data}
\name{tsne.defaults}
\alias{tsne.defaults}
\title{Default configuration for Rtsne}
\format{
  An object of class \code{tsne.defaults} of length 11.
}
\usage{
  tsne.defaults
}
\description{
  A list with parameters customizing a Rtsne embedding. Each component of the
  list is an effective argument for Rtsne_neighbors().
}
\details{
  dims: integer, Output dimensionality 
  
  perplexity: numeric, Perplexity parameter (should not be bigger than 3 * perplexity < nrow(X) - 1, see details for interpretation)
  
  theta: numeric, Speed/accuracy trade-off (increase for less accuracy), set to 0.0 for exact TSNE 
  
  max_iter: integer, Number of iterations 
  
  verbose: logical, Whether progress updates should be printed (default: global "verbose" option, or FALSE if that is not set)
  
  Y_init: matrix, Initial locations of the objects. If NULL, random initialization will be used (default: NULL). Note that when using this, the initial stage with exaggerated perplexity values and a larger momentum term will be skipped.
  
  momentum: numeric, Momentum used in the first part of the optimization 
  
  final_momentum: numeric, Momentum used in the final part of the optimization 
  
  eta: numeric, Learning rate 
  
  exaggeration_factor:
  
  num_threads: integer, Number of threads to use when using OpenMP, default is 1. Setting to 0 corresponds to detecting and using all available cores
}
\examples{
  # display all default settings
  tsne.defaults
  
  # create a new settings object with perplexity set to 100
  custom.settings = tsne.defaults
  custom.settings$perplexity = 100
  custom.settings
  
}
\keyword{datasets}
