#' Network model for nitrogen fluxes in Trinidadian streams (Collins et al. 2016)
#'
#' This model is used in the package case study about Trinidadian streams and
#' is based on an original dataset taken from Collins et al. (2016).
#'
#' The model is complete, with topology, initial conditions, observations,
#' covariates and priors.
#'
#' It is ready for an MCMC run as shown in the example. Note that it might be a
#' good idea to relax the priors for uptake rates from seston to Leptonema
#' (e.g. using hcauchy_p(10)), seston being a compartment that is flowing with
#' the stream water and that can be replenished from upstream.
#'
#' @source
#' 
#' This network model contains data from the original article: Collins, Sarah
#' M., Steven A. Thomas, Thomas Heatherly, Keeley L. MacNeill, Antoine
#' O.H.C. Leduc, Andrés López-Sepulcre, Bradley A. Lamphere, et al. 2016. “Fish
#' Introductions and Light Modulate Food Web Fluxes in Tropical Streams: A
#' Whole-Ecosystem Experimental Approach.” Ecology, <doi:10.1002/ecy.1530>.
#'
#' This dataset was also used in the paper: López-Sepulcre, Andrés, Matthieu
#' Bruneaux, Sarah M. Collins, Rana El-Sabaawi, Alexander S. Flecker, and
#' Steven A. Thomas. 2020. “A New Method to Reconstruct Quantitative Food Webs
#' and Nutrient Flows from Isotope Tracer Addition Experiments.” The American
#' Naturalist 195 (6): 964–85. <doi:10.1086/708546>.
#'
#' @examples
#' trini_mod
#' ggtopo(trini_mod)
#'
#' \dontrun{
#' # Warning: the run below can take quite a long time!
#' # (about 15 min with 4 cores at 3.3 Ghz).
#' run <- run_mcmc(trini_mod, iter = 500, chains = 4, cores = 4)
#' }
#' 
"trini_mod"
