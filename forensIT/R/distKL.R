#' distKL: KL distribution obtained for specific relative contributor
#'
#' @param ped Reference pedigree. It could be an input from read_fam() function or a pedigree built with pedtools. # nolint
#' @param frequency Allele frequency database.
#' @param relative Selected relative.
#' @param numsims Number of simulated genotypes.
#' @param missing Missing person
#' @param cores Enables parallelization.
#' @param frequency Allele frequency database.
#' @return An object of class data.frame with KLs.
#' @export
#' @importFrom pedprobr oneMarkerDistribution
#' @import forrel
#' @importFrom mispitools getfreqs
#'
#' @examples
#' library(forrel)
#' x = linearPed(2)
#' x = setMarkers(x, locusAttributes = NorwegianFrequencies[1:2])
#' x = profileSim(x, N = 1, ids = 2)
#' distKL(ped = x, missing = 5, relative = 1, cores = 1,
#' frequency = NorwegianFrequencies[1:2], numsims = 3)
distKL <- function(ped, missing, relative, frequency, numsims = 100, cores = 1) { # nolint
  peds <- forrel::profileSim(ped, numsims, ids = relative, numCores = cores)
  out <- list()
  for (j in 1:length(peds)) { # nolint
    dat <- perMarkerKLs(peds[[j]], missing, frequency)
    out1 <- c(sum(unlist(dat$KLpopped)), sum(unlist(dat$KLpedpop)))
    names(out1) <- c("KLpopped", "KLpedpop")
    out[[j]] <- out1
  }
  result <- as.data.frame(do.call(rbind, out))
  names(result) <-  c("KLpopped", "KLpedpop")
  return(result)
  }
