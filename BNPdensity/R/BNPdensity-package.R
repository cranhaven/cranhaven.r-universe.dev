#' Acidity Index Dataset
#'
#' Concerns an acidity index measured in a sample of 155 lakes in north-central
#' Wisconsin.
#'
#'
#' @name acidity
#' @docType data
#' @format A real vector with 155 observations.
#' @references Crawford, S. L., DeGroot, M. H., Kadane, J. B. and Small, M. J.
#' (1992). Modeling lake chemistry distributions: approximate Bayesian methods
#' for estimating a finite mixture model. Technometrics, 34, 441-453.
#' @keywords datasets
#' @examples
#'
#' data(acidity)
#' hist(acidity)
NULL




#' Fit of MixNRMI1 function to the enzyme dataset
#'
#' This object contains the output when setting set.seed(150520) and running
#' the function Enzyme1.out <- MixNRMI1(enzyme, Alpha = 1, Kappa = 0.007, Gama = 0.5, distr.k = "gamma", distr.p0 = "gamma", asigma = 1, bsigma = 1, Meps = 0.005, Nit = 5000, Pbi = 0.2)
#'
#' See function MixNRMI1
#'
#' @name Enzyme1.out
#' @docType data
#' @keywords datasets
#' @examples
#'
#' data(Enzyme1.out)
NULL





#' Fit of MixNRMI2 function to the enzyme dataset
#'
#' This object contains the output when setting set.seed(150520) and running
#' the function Enzyme2.out <- MixNRMI2(enzyme, Alpha = 1, Kappa = 0.007, Gama = 0.5, distr.k = "gamma", distr.py0 = "gamma", distr.pz0 = "gamma", mu.pz0 = 1, sigma.pz0 = 1, Meps = 0.005, Nit = 5000, Pbi = 0.2)
#' See function MixNRMI2
#'
#' @name Enzyme2.out
#' @docType data
#' @keywords datasets
#' @examples
#'
#' data(Enzyme2.out)
NULL





#' Enzyme Dataset
#'
#' Concerns the distribution of enzymatic activity in the blood, for an enzyme
#' involved in the metabolism of carcinogenetic substances, among a group of
#' 245 unrelated individuals.
#'
#'
#' @name enzyme
#' @docType data
#' @format A data frame with 244 observations on the following variable:
#' \describe{ \item{list("enzyme")}{A numeric vector.} }
#' @references Bechtel, Y. C., Bonaiti-Pellie, C., Poisson, N., Magnette, J.
#' and Bechtel, P.R. (1993). A population and family study of
#' N-acetyltransferase using caffeine urinary metabolites. Clin. Pharm. Therp.,
#' 54, 134-141.
#' @keywords datasets
#' @examples
#'
#' data(enzyme)
#' hist(enzyme)
NULL





#' Fit of MixNRMI1 function to the galaxy dataset
#'
#' This object contains the output when setting set.seed(150520) and running
#' the function MixNRMI1(galaxy, Alpha = 1, Kappa = 0.015, Gama = 0.5, distr.k = "normal", distr.p0 = "gamma", asigma = 1, bsigma = 1, delta = 7, Meps = 0.005, Nit = 5000, Pbi = 0.2)
#'
#' See function MixNRMI1.
#'
#' @name Galaxy1.out
#' @docType data
#' @keywords datasets
#' @examples
#'
#' data(Galaxy1.out)
NULL





#' Fit of MixNRMI2 function to the galaxy dataset
#'
#' This object contains the output when setting set.seed(150520) and running
#' the function Enzyme2.out <- MixNRMI2(x, Alpha = 1, Kappa = 0.007, Gama = 0.5, distr.k = "gamma", distr.py0 = "gamma", distr.pz0 = "gamma", mu.pz0 = 1, sigma.pz0 = 1, Meps = 0.005, Nit = 5000, Pbi = 0.2)
#'
#' See function MixNRMI2.
#'
#' @name Galaxy2.out
#' @docType data
#' @keywords datasets
#' @examples
#'
#' data(Galaxy2.out)
NULL





#' Galaxy Data Set
#'
#' Velocities of 82 galaxies diverging from our own galaxy.
#'
#'
#' @name galaxy
#' @docType data
#' @format A data frame with 82 observations on the following variable:
#' \describe{ \item{list("velocity")}{A numeric vector.} }
#' @references Roeder, K. (1990) "Density estimation with confidence sets
#' exemplified by superclusters and voids in the galaxies". Journal of the
#' American Statistical Association. 85, 617-624.
#' @keywords datasets
#' @examples
#'
#' data(galaxy)
#' hist(galaxy)
NULL

#' Salinity tolerance
#'
#' 72-hour acute salinity tolerance (LC50 values) of riverine
#' macro-invertebrates.
#'
#'
#' @name salinity
#' @docType data
#' @format A data frame with 108 observations on the following two variables:
#' \describe{
#' \item{left}{A numeric vector.}
#' \item{right}{A
#' numeric vector.} }
#' @references Kefford, B.J., Nugegoda, D., Metzeling, L., Fields, E. 2006.
#' Validating species sensitivity distributions using salinity tolerance of
#' riverine macroinvertebrates in the southern Murray-darling Basin (Victoria,
#' Australia). Canadian Journal of Fisheries and Aquatic Science, 63,
#' 1865-1877.
#' @source \code{fitdistrplus} R-package
#' @keywords datasets
#' @examples
#'
#' data(salinity)
#' hist(salinity$left)
NULL
