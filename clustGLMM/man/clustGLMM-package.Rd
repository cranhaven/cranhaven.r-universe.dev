\name{clustGLMM-package}
\alias{clustGLMM-package}
\docType{package}
\title{
\packageTitle{clustGLMM}
}
\description{
\packageDescription{clustGLMM}
}
\details{
The key function is \code{\link{clustGLMM}} which creates MCMC
samples. The data, formulae and other auxiliary parameters need to be
set up carefully before calling this function. The examples and
tutorials may be used for guidance. Other functions contained in the
package consist of:
\itemize{
  \item a function for generating longitudinal data of mixed-type:
  \code{\link{generate_longitudinal_mixed_type_data}},
  \item functions to generate plots: \code{\link{plot_traceplots}},
  \code{\link{plot_traceplots_param}}, \code{\link{plot_kerneldensity}},
  \code{\link{plot_kerneldensity_param}}, \code{\link{plot_ACF}},
  \code{\link{plot_ACF_param}}, \code{\link{plot_ECDF}},
  \code{\link{plot_ECDF_param}}, \code{\link{plot_clusters}},
  \code{\link{plot_clusters_ECDF_param}},
  \code{\link{plot_clusters_kerneldensity_param}},
  \code{\link{plot.clustglmm}}, \code{\link{plot_num_vs_x_grouped}},
  \code{\link{plot_cat_vs_x_grouped}},
  \item functions for post-processing the samples:
  \code{\link{post_processing}}, \code{\link{permute_cluster_labels}},
  \item functions for internal computations.
}
}
\author{
  \packageAuthor{clustGLMM}
  
  Maintainer: \packageMaintainer{clustGLMM}
}
\references{
Vávra, J., Komárek, A., Grün, B., Malsiner-Walli, G. Clusterwise multivariate regression of mixed-type panel data. Stat Comput 34, 46 (2024). https://doi.org/10.1007/s11222-023-10304-5
}
\keyword{package}
