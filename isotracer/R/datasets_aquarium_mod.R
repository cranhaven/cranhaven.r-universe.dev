#' A simple aquarium network model, ready to run
#'
#' This network model is the model used in the Quick Start tutorial
#' vignette. It is ready to be run at once with \code{\link{run_mcmc}}.
#'
#' The code used to built the model is given in the example section below.
#'
#' The \code{\link{aquarium_run}} dataset is a corresponding MCMC run.
#' 
#' @examples
#' library(tibble)
#' library(dplyr)
#' exp <- tibble::tribble(
#'   ~time.day,  ~species, ~biomass, ~prop15N,
#'           0,   "algae",     1.02,  0.00384,
#'           1,   "algae",       NA,   0.0534,
#'         1.5,   "algae",    0.951,       NA,
#'           2,   "algae",    0.889,   0.0849,
#'         2.5,   "algae",       NA,   0.0869,
#'           3,   "algae",    0.837,   0.0816,
#'           0, "daphnia",     1.74,  0.00464,
#'           1, "daphnia",       NA,  0.00493,
#'         1.5, "daphnia",     2.48,       NA,
#'           2, "daphnia",       NA,  0.00831,
#'         2.5, "daphnia",     2.25,       NA,
#'           3, "daphnia",     2.15,   0.0101,
#'           0,     "NH4",    0.208,     0.79,
#'           1,     "NH4",    0.227,       NA,
#'         1.5,     "NH4",       NA,    0.482,
#'           2,     "NH4",    0.256,    0.351,
#'         2.5,     "NH4",       NA,    0.295,
#'           3,     "NH4",     0.27,        NA
#'   )
#' inits <- exp %>% dplyr::filter(time.day == 0)
#' obs <- exp %>% dplyr::filter(time.day > 0)
#' 
#' aquarium_mod <- new_networkModel() %>%
#'     set_topo("NH4 -> algae -> daphnia -> NH4") %>%
#'     set_init(inits, comp = "species", size = "biomass",
#'              prop = "prop15N") %>%
#'     set_obs(obs, comp = "species", size = "biomass",
#'                   prop = "prop15N", time = "time.day")
#' 
"aquarium_mod"

#' An MCMC run from a simple aquarium network model
#'
#' This is an MCMC run on \code{\link{aquarium_mod}}. The code used to run the
#' MCMC is: \code{aquarium_run <- run_mcmc(aquarium_mod, thin = 4)} (note that
#' \code{thin = 4} was only used here to reduce the size of the data file
#' shipped with the package, but for a real-life analysis keeping the default
#' \code{thin = 1} is usually recommended). The code used to build the model
#' itself is given in the help page for \code{\link{aquarium_mod}}.
#'
#' @examples
#' \dontrun{
#' plot(aquarium_run)
#' summary(aquarium_run)
#' }
"aquarium_run"
